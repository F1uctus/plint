let failures = ref 0

let check name got want =
  if got <> want then begin
    incr failures;
    Printf.printf "FAIL %s: got %S want %S\n" name got want
  end

let check_neq name a b =
  if a = b then begin
    incr failures;
    Printf.printf "FAIL %s: %S should differ from %S\n" name a b
  end

let () =
  (* fold-math: italic Latin subscripts collapse to ASCII *)
  check "fold italic Pss" (Norm.fold_math "𝑃𝑠𝑠") "Pss";
  check "fold mixed Kss" (Norm.fold_math "𝐾ss") "Kss";
  (* fold-math: Letterlike-Symbol holes *)
  check "fold hole C" (Norm.fold_math "ℂ") "C";
  check "fold hole R" (Norm.fold_math "ℝ") "R";
  check "fold hole N" (Norm.fold_math "ℕ") "N";
  (* fold-math: in-block double-struck E *)
  check "fold blackboard E" (Norm.fold_math "𝔼") "E";
  (* fold-math: Greek small alpha and a digit *)
  check "fold greek alpha" (Norm.fold_math "𝛼") "α";
  check "fold bold digit" (Norm.fold_math "𝟏") "1";
  (* fold-math: distinct base letters stay distinct *)
  check_neq "no over-merge P/Q" (Norm.fold_math "𝑃") (Norm.fold_math "𝑄");
  (* fold-math: leaves non-math characters and ASCII alone *)
  check "fold passthrough" (Norm.fold_math "x ⪯ y") "x ⪯ y";

  (* strip-ws: a space vs no space around an operator becomes equal *)
  check "strip operator spacing"
    (Norm.strip_ws "𝐴 ⪯𝐵")
    (Norm.strip_ws "𝐴 ⪯ 𝐵");
  (* strip-ws: newlines preserved (line count unchanged) *)
  check "strip keeps newlines" (Norm.strip_ws "a b\nc d") "ab\ncd";

  (* apply-rules: literal substitution in order *)
  check "rule citation sep"
    (Norm.apply_rules [ ("; [1,", ", [1,") ] "p. 783]; [1, App")
    "p. 783], [1, App";

  (* normalize: composes fold then strip then rules *)
  check "normalize compose"
    (Norm.normalize ~fold:true ~strip:true ~rules:[] "𝑃𝑠𝑠 ⪯𝐵")
    "Pss⪯B";
  (* normalize: all off is the identity *)
  check "normalize identity"
    (Norm.normalize ~fold:false ~strip:false ~rules:[] "𝑃𝑠𝑠 ⪯ 𝐵")
    "𝑃𝑠𝑠 ⪯ 𝐵";

  if !failures > 0 then begin
    Printf.printf "%d failure(s)\n" !failures;
    exit 1
  end
  else print_string "all norm tests passed\n"
