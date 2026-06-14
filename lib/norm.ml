(* --- UTF-8 decode / encode --------------------------------------------- *)

(* Decode a UTF-8 string to a list of Unicode scalars. Bytes that do not form a
   valid sequence pass through as their Latin-1 value, so the function never
   raises on real pdftotext output. *)
let utf8_decode s =
  let n = String.length s in
  let byte i = Char.code s.[i] in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let c0 = byte i in
      if c0 < 0x80 then go (i + 1) (c0 :: acc)
      else if c0 < 0xE0 && i + 1 < n then
        let cp = ((c0 land 0x1F) lsl 6) lor (byte (i + 1) land 0x3F) in
        go (i + 2) (cp :: acc)
      else if c0 < 0xF0 && i + 2 < n then
        let cp =
          ((c0 land 0x0F) lsl 12)
          lor ((byte (i + 1) land 0x3F) lsl 6)
          lor (byte (i + 2) land 0x3F)
        in
        go (i + 3) (cp :: acc)
      else if c0 < 0xF8 && i + 3 < n then
        let cp =
          ((c0 land 0x07) lsl 18)
          lor ((byte (i + 1) land 0x3F) lsl 12)
          lor ((byte (i + 2) land 0x3F) lsl 6)
          lor (byte (i + 3) land 0x3F)
        in
        go (i + 4) (cp :: acc)
      else go (i + 1) (c0 :: acc)
  in
  go 0 []

let utf8_encode_scalar buf cp =
  if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
  else if cp < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else if cp < 0x10000 then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end

(* --- math-alphanumeric fold -------------------------------------------- *)

let code = Char.code

(* Styled letters that live in the Letterlike Symbols block instead of in the
   U+1D400..U+1D7FF block (the "holes"), mapped to their base letter. *)
let letterlike_holes =
  [
    (0x210E, code 'h'); (* PLANCK CONSTANT (italic h) *)
    (0x2102, code 'C'); (* DOUBLE-STRUCK CAPITAL C *)
    (0x210D, code 'H'); (* DOUBLE-STRUCK CAPITAL H *)
    (0x2115, code 'N'); (* DOUBLE-STRUCK CAPITAL N *)
    (0x2119, code 'P'); (* DOUBLE-STRUCK CAPITAL P *)
    (0x211A, code 'Q'); (* DOUBLE-STRUCK CAPITAL Q *)
    (0x211D, code 'R'); (* DOUBLE-STRUCK CAPITAL R *)
    (0x2124, code 'Z'); (* DOUBLE-STRUCK CAPITAL Z *)
    (0x2145, code 'D'); (* DOUBLE-STRUCK ITALIC CAPITAL D *)
    (0x2146, code 'd'); (* DOUBLE-STRUCK ITALIC SMALL D *)
    (0x2147, code 'e'); (* DOUBLE-STRUCK ITALIC SMALL E *)
    (0x2148, code 'i'); (* DOUBLE-STRUCK ITALIC SMALL I *)
    (0x2149, code 'j'); (* DOUBLE-STRUCK ITALIC SMALL J *)
    (0x212C, code 'B'); (* SCRIPT CAPITAL B *)
    (0x2130, code 'E'); (* SCRIPT CAPITAL E *)
    (0x2131, code 'F'); (* SCRIPT CAPITAL F *)
    (0x210B, code 'H'); (* SCRIPT CAPITAL H *)
    (0x2110, code 'I'); (* SCRIPT CAPITAL I *)
    (0x2112, code 'L'); (* SCRIPT CAPITAL L *)
    (0x2133, code 'M'); (* SCRIPT CAPITAL M *)
    (0x211B, code 'R'); (* SCRIPT CAPITAL R *)
    (0x212F, code 'e'); (* SCRIPT SMALL E *)
    (0x210A, code 'g'); (* SCRIPT SMALL G *)
    (0x2134, code 'o'); (* SCRIPT SMALL O *)
    (0x2113, code 'l'); (* SCRIPT SMALL L *)
    (0x212D, code 'C'); (* BLACK-LETTER CAPITAL C *)
    (0x210C, code 'H'); (* BLACK-LETTER CAPITAL H *)
    (0x2111, code 'I'); (* BLACK-LETTER CAPITAL I *)
    (0x211C, code 'R'); (* BLACK-LETTER CAPITAL R *)
    (0x2128, code 'Z'); (* BLACK-LETTER CAPITAL Z *)
  ]

(* Variant Greek forms in the styled blocks: epsilon, theta, kappa, phi, rho,
   pi, mapped to their base small-letter codepoints. *)
let greek_variants = [| 0x3F5; 0x3D1; 0x3F0; 0x3D5; 0x3F1; 0x3D6 |]

(* Fold one styled Greek scalar (U+1D6A8..U+1D7CB) to its base Greek codepoint.
   Each of the five styles is 58 codepoints: 25 capitals (with the theta-symbol
   in the U+03A2 reserved slot), nabla, 25 smalls, partial, then six variants. *)
let fold_greek cp =
  if cp = 0x1D7CA then 0x3DC (* GREEK LETTER DIGAMMA *)
  else if cp = 0x1D7CB then 0x3DD (* GREEK SMALL LETTER DIGAMMA *)
  else
    let r = (cp - 0x1D6A8) mod 58 in
    if r <= 16 then 0x391 + r (* Α..Ρ *)
    else if r = 17 then 0x398 (* capital theta symbol -> Θ *)
    else if r <= 24 then 0x3A3 + (r - 18) (* Σ..Ω *)
    else if r = 25 then 0x2207 (* nabla, no base letter *)
    else if r <= 50 then 0x3B1 + (r - 26) (* α..ω (includes final sigma) *)
    else if r = 51 then 0x2202 (* partial, no base letter *)
    else greek_variants.(r - 52)

let fold_scalar cp =
  if cp >= 0x1D7CE && cp <= 0x1D7FF then code '0' + ((cp - 0x1D7CE) mod 10)
  else if cp >= 0x1D400 && cp <= 0x1D6A3 then
    let idx = (cp - 0x1D400) mod 52 in
    if idx < 26 then code 'A' + idx else code 'a' + (idx - 26)
  else if cp >= 0x1D6A8 && cp <= 0x1D7CB then fold_greek cp
  else match List.assoc_opt cp letterlike_holes with Some b -> b | None -> cp

let fold_math s =
  let buf = Buffer.create (String.length s) in
  List.iter (fun cp -> utf8_encode_scalar buf (fold_scalar cp)) (utf8_decode s);
  Buffer.contents buf

(* --- whitespace and literal rules -------------------------------------- *)

let strip_ws s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c -> if c <> ' ' && c <> '\t' then Buffer.add_char buf c) s;
  Buffer.contents buf

(* Replace every occurrence of [sub] in [s] with [by]. *)
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

let apply_rules rules s =
  List.fold_left (fun acc (from, by) -> replace_all acc from by) s rules

let normalize ~fold ~strip ~rules s =
  let s = if fold then fold_math s else s in
  let s = if strip then strip_ws s else s in
  apply_rules rules s
