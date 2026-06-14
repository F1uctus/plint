(* Text normalization applied symmetrically to the reference and the current
   page text before comparison, so below-threshold typographic differences do
   not register as page changes. All transforms preserve line count. *)

(* Fold the Unicode Mathematical Alphanumeric Symbols (Latin and Greek styled
   alphabets and digits, U+1D400..U+1D7FF) and the Letterlike-Symbol holes
   (e.g. U+2102 double-struck C) to their base codepoint. Only the style of a
   base character is folded; distinct base letters stay distinct. *)
val fold_math : string -> string

(* Remove every space and tab; newlines are kept, so line count is preserved. *)
val strip_ws : string -> string

(* Apply literal [from -> to] substitutions in order to the whole string. *)
val apply_rules : (string * string) list -> string -> string

(* Compose the three transforms in order: fold, strip, rules. *)
val normalize :
  fold:bool -> strip:bool -> rules:(string * string) list -> string -> string
