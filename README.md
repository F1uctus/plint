# plint

A per-page PDF layout linter: render a document to PDF with a configurable
command, extract the text of each page with `pdftotext`, and compare it against
a reference. An accidental layout shift, for example one that would force a
reprint of a typeset document, is reported page by page instead of slipping
through unnoticed.

plint is renderer-agnostic. The render step is just a command that produces a
PDF, so it works with [Typst](https://typst.app), LaTeX, or anything else. The
reference can come from a stored snapshot or, in CI, from a chosen git commit.

## Requirements

- A renderer that produces a PDF (e.g. `typst`, `latexmk`).
- `pdftotext` (from poppler-utils).
- `git` (only for `--git-check`).
- To build: OCaml (>= 4.08) and dune.

## Build

```sh
dune build
```

The executable is `_build/default/bin/plint.exe`. Prebuilt Linux binaries are
attached to each [release](../../releases).

## Configuration

plint reads a `plint.toml` file. It is discovered, in order, from `--config
PATH`, the nearest ancestor directory containing `plint.toml`, or a single
`plint.toml` one directory level below the current directory.

| Key | Type | Default | Meaning |
|-----|------|---------|---------|
| `root` | string | `"."` | project root, relative to the config file; plint runs here |
| `document` | string | — (required) | main source file, relative to `root` |
| `render` | string | — (required) | render command template (see below) |
| `snapshot` | string | `"snapshot"` | reference snapshot directory |
| `watch-dirs` | string[] | `["."]` | directories watched by `--watch` |
| `watch-exts` | string[] | `[]` (all) | watched file extensions |
| `watch-exclude` | string[] | `[]` | directory basenames skipped while walking |
| `critical-pages` | int[] | `[]` | pages whose change is reported as a hard error |
| `git-base` | string | none | default base commit/ref for `--git-check` |
| `fold-math` | bool | `false` | fold math-alphanumeric Unicode (italic, bold, etc.) to base letters before comparing |
| `ignore-whitespace` | bool | `false` | drop spaces and tabs before comparing (newlines kept) |
| `normalize` | string[] | `[]` | literal `from => to` substitutions applied to both sides before comparing |

These three normalizations run, in order, on the reference and the current page
text alike, so below-threshold typographic differences (math italic vs upright,
operator spacing) do not register as page changes. The snapshot is never
rewritten. Page-count and `critical-pages` checks still run on the normalized
pages.

### Render command

`render` is run with `/bin/sh -c`, so environment prefixes and compound commands
work. plint substitutes three shell-quoted placeholders:

- `{doc}` — the document path to render.
- `{out}` — the PDF path plint expects the command to produce.
- `{root}` — the project root (for renderers that take one, e.g. typst).

The only contract is that the command writes a PDF to `{out}`.

```toml
# Typst
render = "typst compile {doc} {out} --root {root}"

# LaTeX
render = "latexmk -pdf -interaction=nonstopmode -jobname=plint {doc} && cp plint.pdf {out}"
```

See [`plint.toml.example`](plint.toml.example) for a fully commented file.

## Commands

```sh
plint --check            # default: compare the current render against the snapshot
plint --git-check [REF]  # compare against REF (or git-base) rendered from a worktree
plint --update           # (re)write the snapshot from the current render
plint --watch            # re-run --check whenever a watched file changes
```

Options: `--config PATH`, `--doc PATH`, `--snapshot DIR`, `-h`/`--help`.

`--check` and `--git-check` print the changed page numbers with the first
diverging line of each. Exit codes:

| Code | Meaning |
|------|---------|
| 0 | no page changed |
| 1 | pages changed (soft drift) |
| 3 | a hard layout violation: page count changed, or a `critical-pages` page shifted |
| 2 | usage or runtime error |

This lets CI gate on hard violations only (`code -ge 2`) while tolerating
ordinary, expected content drift.

## CI usage

`--git-check` builds the reference itself, so CI does not need a committed
snapshot. It checks out the base commit in a detached `git worktree`, renders it
reusing the current checkout's assets (packages, fonts), and compares it against
the checked-out tree:

```yaml
- uses: actions/checkout@v4
  with:
    fetch-depth: 0          # the base commit and history must be present
- run: sudo apt-get install -y poppler-utils
# ... set up the renderer ...
- run: |
    curl -fsSL https://github.com/F1uctus/plint/releases/latest/download/plint-linux-x86_64 -o plint
    chmod +x plint
    ./plint --git-check     # base commit taken from git-base in plint.toml
```

## License

GPL-3.0-or-later. See [LICENSE](LICENSE).
