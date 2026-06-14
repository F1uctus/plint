let usage =
  "plint [--check|--git-check [REF]|--update|--watch] [OPTIONS]\n\n\
   Per-page PDF layout linter. Renders a document to PDF with a configurable\n\
   command, extracts the text of each page (pdftotext) and compares it against\n\
   a reference, so an accidental layout shift never goes unnoticed.\n\n\
   Commands:\n\
   \  --check            (default) render and compare against the snapshot\n\
   \                     directory.\n\
   \  --git-check [REF]  render REF (or config 'git-base') in a detached git\n\
   \                     worktree as the reference and compare it against the\n\
   \                     current tree. Does not use the snapshot directory.\n\
   \  --update           render and overwrite the snapshot directory.\n\
   \  --watch            watch the configured directories and re-run --check.\n\n\
   Options:\n\
   \  --config PATH      path to plint.toml (default: discovered)\n\
   \  --doc PATH         override the 'document' config value\n\
   \  --snapshot DIR     override the 'snapshot' config value\n\
   \  -h, --help         show this help\n\n\
   Exit codes: 0 unchanged; 1 pages changed (soft drift); 3 a hard layout\n\
   violation (page count or a critical page shifted); 2 a usage or runtime\n\
   error.\n\n\
   Configuration is read from plint.toml; see plint.toml.example.\n"

let die code msg =
  Printf.eprintf "plint: %s\n" msg;
  exit code

(* --- small filesystem and process helpers ------------------------------- *)

let mkdir_p dir =
  try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let read_all ic =
  let buf = Buffer.create 65536 in
  let chunk = Bytes.create 65536 in
  let rec loop () =
    let n = input ic chunk 0 (Bytes.length chunk) in
    if n > 0 then (
      Buffer.add_subbytes buf chunk 0 n;
      loop ())
  in
  loop ();
  Buffer.contents buf

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* Run a program capturing its stdout; stderr is inherited. *)
let run_capture prog args =
  let argv = Array.of_list (prog :: args) in
  let ic = Unix.open_process_args_in prog argv in
  let out = read_all ic in
  let code =
    match Unix.close_process_in ic with Unix.WEXITED c -> c | _ -> 1
  in
  (out, code)

(* Run a shell command line with inherited stdio; returns the exit code. *)
let run_sh cmd =
  let argv = [| "/bin/sh"; "-c"; cmd |] in
  let pid =
    Unix.create_process "/bin/sh" argv Unix.stdin Unix.stdout Unix.stderr
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 1

(* --- path helpers ------------------------------------------------------- *)

let abs p =
  if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

let resolve base p = if Filename.is_relative p then Filename.concat base p else p

(* Collapse '.' and '..' segments, preserving a leading '/'. *)
let normalize path =
  let is_abs = not (Filename.is_relative path) in
  let parts = String.split_on_char '/' path in
  let rec go acc = function
    | [] -> List.rev acc
    | "" :: rest | "." :: rest -> go acc rest
    | ".." :: rest -> (
        match acc with
        | a :: tl when a <> ".." -> go tl rest
        | _ -> go (".." :: acc) rest)
    | seg :: rest -> go (seg :: acc) rest
  in
  let s = String.concat "/" (go [] parts) in
  if is_abs then "/" ^ s else if s = "" then "." else s

(* Relative path from directory [from] to [target] (both absolute). *)
let relpath ~from target =
  let f = String.split_on_char '/' (normalize from) in
  let t = String.split_on_char '/' (normalize target) in
  let rec strip a b =
    match (a, b) with
    | x :: xs, y :: ys when x = y -> strip xs ys
    | _ -> (a, b)
  in
  let fa, tb = strip f t in
  match List.map (fun _ -> "..") fa @ tb with
  | [] -> "."
  | segs -> String.concat "/" segs

(* POSIX single-quote escaping. *)
let sh_quote s =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

let replace_all s sub by =
  let sub_len = String.length sub in
  if sub_len = 0 then s
  else begin
    let buf = Buffer.create (String.length s) in
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      if !i + sub_len <= n && String.sub s !i sub_len = sub then (
        Buffer.add_string buf by;
        i := !i + sub_len)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    Buffer.contents buf
  end

(* --- configuration ------------------------------------------------------ *)

type config = {
  root : string;           (* absolute project root *)
  document : string;       (* relative to root *)
  render : string;         (* render command template *)
  snapshot : string;       (* absolute snapshot directory *)
  watch_dirs : string list;        (* absolute *)
  watch_exts : string list;        (* empty = all files *)
  watch_exclude : string list;     (* directory basenames *)
  critical_pages : int list;
  git_base : string option;
  fold_math : bool;                (* fold math-alphanumeric to base letters *)
  ignore_ws : bool;                (* drop spaces and tabs before comparing *)
  rules : (string * string) list;  (* literal [from -> to] substitutions *)
}

type vnode = VStr of string | VArr of string list

let unquote s =
  let s = String.trim s in
  let n = String.length s in
  if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2)
  else s

let parse_value raw =
  let s = String.trim raw in
  if s = "" then VStr ""
  else if s.[0] = '"' then
    match String.index_from_opt s 1 '"' with
    | Some j -> VStr (String.sub s 1 (j - 1))
    | None -> VStr (String.sub s 1 (String.length s - 1))
  else if s.[0] = '[' then begin
    let close =
      match String.rindex_opt s ']' with Some j -> j | None -> String.length s
    in
    let inner = String.sub s 1 (close - 1) in
    String.split_on_char ',' inner
    |> List.filter_map (fun p ->
           let p = String.trim p in
           if p = "" then None else Some (unquote p))
    |> fun l -> VArr l
  end
  else
    (* bare scalar: drop a trailing '# comment' *)
    let s = match String.index_opt s '#' with Some i -> String.sub s 0 i | None -> s in
    VStr (String.trim s)

let read_config_file path =
  let ic = open_in path in
  let tbl = Hashtbl.create 16 in
  (try
     while true do
       let line = String.trim (input_line ic) in
       if line = "" || line.[0] = '#' then ()
       else
         match String.index_opt line '=' with
         | None -> ()
         | Some i ->
             let key = String.trim (String.sub line 0 i) in
             let raw = String.sub line (i + 1) (String.length line - i - 1) in
             Hashtbl.replace tbl key (parse_value raw)
     done
   with End_of_file -> ());
  close_in ic;
  tbl

let get_str tbl k =
  match Hashtbl.find_opt tbl k with Some (VStr s) -> Some s | _ -> None

let get_arr tbl k =
  match Hashtbl.find_opt tbl k with
  | Some (VArr l) -> Some l
  | Some (VStr s) when s <> "" -> Some [ s ]
  | _ -> None

let get_bool tbl k =
  match get_str tbl k with Some "true" -> true | _ -> false

(* First index of [sub] in [s], or None. *)
let index_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  if m = 0 then None else go 0

let drop_trailing_space s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = ' ' then String.sub s 0 (n - 1) else s

let drop_leading_space s =
  let n = String.length s in
  if n > 0 && s.[0] = ' ' then String.sub s 1 (n - 1) else s

(* A `from => to` substitution rule: split on the first `=>` and trim a single
   space adjacent to the delimiter on each side. Entries without `=>` are
   dropped. *)
let parse_rule entry =
  match index_sub entry "=>" with
  | None -> None
  | Some i ->
      let from = drop_trailing_space (String.sub entry 0 i) in
      let by =
        drop_leading_space
          (String.sub entry (i + 2) (String.length entry - i - 2))
      in
      Some (from, by)

let load_config config_path =
  let config_dir = Filename.dirname config_path in
  let tbl = read_config_file config_path in
  let req k =
    match get_str tbl k with
    | Some v -> v
    | None -> die 2 (Printf.sprintf "config: '%s' is required" k)
  in
  let root = normalize (resolve config_dir (Option.value ~default:"." (get_str tbl "root"))) in
  let document = req "document" in
  let render = req "render" in
  let snapshot =
    normalize (resolve root (Option.value ~default:"snapshot" (get_str tbl "snapshot")))
  in
  let watch_dirs =
    (match get_arr tbl "watch-dirs" with Some l -> l | None -> [ "." ])
    |> List.map (fun d -> normalize (resolve root d))
  in
  {
    root;
    document;
    render;
    snapshot;
    watch_dirs;
    watch_exts = Option.value ~default:[] (get_arr tbl "watch-exts");
    watch_exclude = Option.value ~default:[] (get_arr tbl "watch-exclude");
    critical_pages =
      (match get_arr tbl "critical-pages" with
      | Some l -> List.filter_map int_of_string_opt l
      | None -> []);
    git_base = get_str tbl "git-base";
    fold_math = get_bool tbl "fold-math";
    ignore_ws = get_bool tbl "ignore-whitespace";
    rules =
      (match get_arr tbl "normalize" with
      | Some l -> List.filter_map parse_rule l
      | None -> []);
  }

(* plint.toml: explicit --config, else nearest ancestor, else a single match
   one directory level below the current directory. *)
let discover_config () =
  let cwd = Sys.getcwd () in
  let rec up dir =
    let cand = Filename.concat dir "plint.toml" in
    if Sys.file_exists cand then Some cand
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else up parent
  in
  match up cwd with
  | Some p -> p
  | None ->
      let down =
        (try Sys.readdir cwd with _ -> [||])
        |> Array.to_list
        |> List.filter_map (fun e ->
               let sub = Filename.concat cwd e in
               if (try Sys.is_directory sub with _ -> false) then
                 let cand = Filename.concat sub "plint.toml" in
                 if Sys.file_exists cand then Some cand else None
               else None)
      in
      (match down with
      | [ p ] -> p
      | [] ->
          die 2
            "plint.toml not found (searched upward and one level down); \
             pass --config PATH"
      | _ -> die 2 "multiple plint.toml found one level down; pass --config PATH")

(* --- render and page extraction ----------------------------------------- *)

(* Render [doc] to a temporary PDF and return its pages. Runs in the current
   working directory (the project root); [root] is substituted for {root}. *)
let render_and_extract ~doc ~root template =
  let out = Filename.temp_file "plint-" ".pdf" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove out with _ -> ())
    (fun () ->
      let cmd =
        template
        |> (fun t -> replace_all t "{doc}" (sh_quote doc))
        |> (fun t -> replace_all t "{out}" (sh_quote out))
        |> fun t -> replace_all t "{root}" (sh_quote root)
      in
      let code = run_sh cmd in
      if code <> 0 then die code (Printf.sprintf "render command exited with %d" code);
      let text, code = run_capture "pdftotext" [ out; "-" ] in
      if code <> 0 then die code (Printf.sprintf "pdftotext exited with %d" code);
      (* split on form-feed, dropping the trailing empty chunk after the last
         page break *)
      let parts = String.split_on_char '\012' text in
      let parts =
        match List.rev parts with "" :: rest -> List.rev rest | _ -> parts
      in
      let strip_trailing line =
        let n = String.length line in
        let rec last i =
          if i > 0 && (line.[i - 1] = ' ' || line.[i - 1] = '\t' || line.[i - 1] = '\r')
          then last (i - 1)
          else i
        in
        String.sub line 0 (last n)
      in
      let normalize_page page =
        page |> String.split_on_char '\n' |> List.map strip_trailing
        |> String.concat "\n"
      in
      List.map normalize_page parts)

(* --- comparison and reporting ------------------------------------------- *)

let truncate_line s =
  let limit = 72 in
  if String.length s <= limit then s else String.sub s 0 limit ^ "\xe2\x80\xa6"

(* 1-based number and both versions of the first diverging line of a page. *)
let first_diff_line a b =
  let la = String.split_on_char '\n' a and lb = String.split_on_char '\n' b in
  let rec go i la lb =
    match (la, lb) with
    | x :: xs, y :: ys -> if x = y then go (i + 1) xs ys else Some (i, x, y)
    | x :: _, [] -> Some (i, x, "")
    | [], y :: _ -> Some (i, "", y)
    | [], [] -> None
  in
  go 1 la lb

(* The 1-based [ln]th line of [s], or "" if out of range. *)
let nth_line s ln =
  match List.nth_opt (String.split_on_char '\n' s) (ln - 1) with
  | Some l -> l
  | None -> ""

let report cfg ~reference ~current =
  (* Normalization folds below-threshold typographic differences away before
     comparing; it preserves line count, so the raw line at a normalized
     diverging index is shown to the reader. *)
  let norm =
    Norm.normalize ~fold:cfg.fold_math ~strip:cfg.ignore_ws ~rules:cfg.rules
  in
  let cur = Array.of_list current and ref_ = Array.of_list reference in
  let n_cur = Array.length cur and n_ref = Array.length ref_ in
  let n = max n_cur n_ref in
  let changed = ref [] in
  for i = 0 to n - 1 do
    let r = if i < n_ref then Some (norm ref_.(i)) else None in
    let c = if i < n_cur then Some (norm cur.(i)) else None in
    if r <> c then changed := (i + 1) :: !changed
  done;
  let changed = List.rev !changed in
  List.iter
    (fun page ->
      let i = page - 1 in
      if i < n_ref && i < n_cur then (
        match first_diff_line (norm ref_.(i)) (norm cur.(i)) with
        | Some (ln, _, _) ->
            Printf.printf "Page %d changed (line %d):\n" page ln;
            Printf.printf "  was: %s\n" (truncate_line (nth_line ref_.(i) ln));
            Printf.printf "  now: %s\n" (truncate_line (nth_line cur.(i) ln))
        | None -> ())
      else if i >= n_ref then Printf.printf "Page %d added\n" page
      else Printf.printf "Page %d removed\n" page)
    changed;
  let count_changed = n_cur <> n_ref in
  if count_changed then
    Printf.printf "ERROR: page count changed: was %d, now %d\n" n_ref n_cur;
  let critical_changed =
    List.filter (fun p -> List.mem p changed) cfg.critical_pages
  in
  List.iter
    (fun p ->
      Printf.printf "ERROR: critical page %d changed; frozen layout shifted\n" p)
    critical_changed;
  if changed = [] then (
    Printf.printf "plint: layout unchanged (%d pages)\n" n_cur;
    0)
  else (
    Printf.printf "changed pages: %s\n"
      (String.concat ", " (List.map string_of_int changed));
    (* exit 3 marks a hard layout violation (page count or a critical page
       shifted) so CI can gate on it; ordinary drift stays exit 1 *)
    if count_changed || critical_changed <> [] then 3 else 1)

(* --- snapshot ----------------------------------------------------------- *)

let snapshot_page_path dir n =
  Filename.concat dir (Printf.sprintf "page-%03d.txt" n)

let load_snapshot dir =
  if not (Sys.file_exists dir) then
    die 2 (Printf.sprintf "no snapshot at %s; run --update first" dir);
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".txt")
  |> List.sort compare
  |> List.map (fun f -> read_file (Filename.concat dir f))

(* --- commands ----------------------------------------------------------- *)

let cmd_check cfg =
  let current = render_and_extract ~doc:cfg.document ~root:"." cfg.render in
  let reference = load_snapshot cfg.snapshot in
  report cfg ~reference ~current

let cmd_update cfg =
  let pages = render_and_extract ~doc:cfg.document ~root:"." cfg.render in
  mkdir_p cfg.snapshot;
  Sys.readdir cfg.snapshot
  |> Array.iter (fun f ->
         if Filename.check_suffix f ".txt" then
           Sys.remove (Filename.concat cfg.snapshot f));
  List.iteri
    (fun i page ->
      let oc = open_out_bin (snapshot_page_path cfg.snapshot (i + 1)) in
      output_string oc page;
      close_out oc)
    pages;
  Printf.printf "plint: snapshot updated (%d pages)\n" (List.length pages)

let git_or_die args =
  let out, code = run_capture "git" args in
  if code <> 0 then
    die code
      (Printf.sprintf "git %s exited with %d" (String.concat " " args) code);
  out

let cmd_git_check cfg ref_opt =
  let ref_ =
    match (ref_opt, cfg.git_base) with
    | Some r, _ | None, Some r -> r
    | None, None -> die 2 "no commit given and no 'git-base' in config"
  in
  let toplevel = String.trim (git_or_die [ "rev-parse"; "--show-toplevel" ]) in
  (* a fresh, non-existent path for git to populate *)
  let wt =
    let p = Filename.temp_file "plint-wt-" "" in
    Sys.remove p;
    p
  in
  ignore (git_or_die [ "worktree"; "add"; "--detach"; wt; ref_ ]);
  Fun.protect
    ~finally:(fun () -> ignore (run_capture "git" [ "worktree"; "remove"; "--force"; wt ]))
    (fun () ->
      let doc_abs = normalize (resolve cfg.root cfg.document) in
      let doc_wt = normalize (Filename.concat wt (relpath ~from:toplevel doc_abs)) in
      let root_wt = normalize (Filename.concat wt (relpath ~from:toplevel cfg.root)) in
      Printf.printf "plint: comparing against %s\n%!" ref_;
      let reference = render_and_extract ~doc:doc_wt ~root:root_wt cfg.render in
      let current = render_and_extract ~doc:cfg.document ~root:"." cfg.render in
      report cfg ~reference ~current)

let ext_ok cfg path =
  match cfg.watch_exts with
  | [] -> true
  | exts -> List.exists (Filename.check_suffix path) exts

let collect_watched cfg =
  let acc = ref [] in
  let rec walk dir =
    if List.mem (Filename.basename dir) cfg.watch_exclude then ()
    else
      match try Some (Sys.readdir dir) with _ -> None with
      | None -> ()
      | Some entries ->
          Array.iter
            (fun e ->
              let p = Filename.concat dir e in
              if (try Sys.is_directory p with _ -> false) then walk p
              else if ext_ok cfg p then acc := p :: !acc)
            entries
  in
  List.iter walk cfg.watch_dirs;
  List.sort compare !acc

let mtimes files = List.map (fun f -> (f, (Unix.stat f).Unix.st_mtime)) files

let cmd_watch cfg =
  Printf.printf "plint: watching for changes (Ctrl-C to exit)\n%!";
  let last = ref (mtimes (collect_watched cfg)) in
  ignore (cmd_check cfg);
  while true do
    Unix.sleepf 1.0;
    let cur = mtimes (collect_watched cfg) in
    if cur <> !last then (
      last := cur;
      Printf.printf "\nplint: change detected, rebuilding...\n%!";
      ignore (cmd_check cfg))
  done

(* --- entry point -------------------------------------------------------- *)

let () =
  let config_override = ref None in
  let doc_override = ref None in
  let snap_override = ref None in
  let mode = ref `Check in
  let git_ref = ref None in
  let rec parse = function
    | [] -> ()
    | "--config" :: v :: rest -> config_override := Some v; parse rest
    | "--doc" :: v :: rest -> doc_override := Some v; parse rest
    | "--snapshot" :: v :: rest -> snap_override := Some v; parse rest
    | "--check" :: rest -> mode := `Check; parse rest
    | "--update" :: rest -> mode := `Update; parse rest
    | "--watch" :: rest -> mode := `Watch; parse rest
    | "--git-check" :: rest -> (
        mode := `GitCheck;
        match rest with
        | v :: more when String.length v > 0 && v.[0] <> '-' ->
            git_ref := Some v; parse more
        | _ -> parse rest)
    | ("-h" | "--help") :: _ -> print_string usage; exit 0
    | x :: _ -> die 2 (Printf.sprintf "unknown argument: %s" x)
  in
  parse (List.tl (Array.to_list Sys.argv));
  let config_path =
    match !config_override with Some p -> abs p | None -> discover_config ()
  in
  let cfg = load_config config_path in
  let cfg =
    match !doc_override with Some d -> { cfg with document = d } | None -> cfg
  in
  let cfg =
    match !snap_override with
    | Some s -> { cfg with snapshot = normalize (resolve cfg.root s) }
    | None -> cfg
  in
  Sys.chdir cfg.root;
  match !mode with
  | `Check -> exit (cmd_check cfg)
  | `GitCheck -> exit (cmd_git_check cfg !git_ref)
  | `Update -> cmd_update cfg
  | `Watch -> cmd_watch cfg
