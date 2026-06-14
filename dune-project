(lang dune 3.21)

(name plint)

(generate_opam_files true)

(license GPL-3.0-or-later)

(authors "Ilya I. Nikitin")

(maintainers "Ilya I. Nikitin")

(source
 (github F1uctus/plint))

(package
 (name plint)
 (synopsis "Per-page PDF layout linter for frozen-print documents")
 (description
  "plint renders a document to PDF with a configurable command, extracts the\ntext of each page with pdftotext and compares it against a reference, so an\naccidental layout shift never goes unnoticed. It is renderer-agnostic (typst,\nLaTeX, or anything that produces a PDF) and can build the reference either from\na stored snapshot or, in CI, from a chosen git commit.")
 (depends
  (ocaml
   (>= 4.08))
  dune))
