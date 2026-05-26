# Nekomata

Experimental non-deterministic concatenative golfing language, written in Haskell.

## Build & Test

```sh
cabal build
cabal run Nekomata -- -c '"Hello, World!"'
cabal run Nekomata -- -c '+' -i '1 2'
cabal test --enable-test
```

Requires **GHC 9.6.7+**. CI tests on 9.6.7, 9.10.3, 9.12.4, 9.14.1.

No formatter/linter beyond `-Wall` in `.cabal`.

## Project structure

| Path            | Role                                                                                        |
| --------------- | ------------------------------------------------------------------------------------------- |
| `src/Nekomata/` | Library: Eval, Function, Data, NonDet, Parser, Program, Builtin, Particle, CodePage, Result |
| `app/`          | Executable (Main.hs, Repl.hs, Doc.hs)                                                       |
| `test/`         | Hspec tests (Main.hs + Eval.hs)                                                             |
| `doc/`          | Generated docs + tutorials                                                                  |
| `analysis/`     | Python frequency analysis of golf solutions                                                 |

## Testing

Tests are Hspec. Two categories in `test/Main.hs`:
1. **Builtin examples** — each `Builtin` carries `examples :: [(String, Result)]`; auto-tested via `testBuiltins`.
2. **CGCC challenges** — integration tests in `test/Eval.hs` (`testEval`).

Use `shouldMatch` helpers from `Nekomata.Result`: `all_`, `first_`, `last_`, `nothing_`, `truncate_`, `Count`, `Check`.

## Key architecture

- **Non-determinism**: `Try` monad (`Val | Choice Id a a | Fail | Cut`), based on KiCS2 paper. `Id` tracks choice tree via binary encoding.
- **Stack**: infinite `TryData :+ Stack`; cycles input values.
- **Code page**: 256-char custom encoding (`src/Nekomata/CodePage.hs`). Unassigned slots use `�`. All source chars must pass `checkCodePage`.
- **Data types**: `Data = DNum Rational | DChar Word8 | DList [Data]` (deterministic); `TryData = Try DataTry` (non-deterministic).
- **Particles**: higher-order functions (`onBoth`, `map`, `zipWith`, `iterate`, `while`, `fold1`, etc.). Applied before their argument.

## CLI flags

| Flag                      | Meaning                                   |
| ------------------------- | ----------------------------------------- |
| `-c CODE`                 | Run code string                           |
| `-f FILE` / `-u FILE`     | Run file (custom encoding / UTF-8)        |
| `-i INPUT`                | Input (space-separated values)            |
| `-s`                      | Read input from stdin                     |
| `-m`                      | Multiple inputs (newline-separated lines) |
| `-1` / `-t` / `-n` / `-e` | Mode: first / last / count / exists       |
| `-l N`                    | Limit results                             |
| `-r`                      | REPL (default with no args)               |
| `--doc` / `--codepage`    | Generate docs                             |

## Input quirks

- `->` starts a comment in input (ignored). E.g., `1 2 -> sum is 3`.
- REPL: `\Input ...` sets stack, `\Mode ...` changes output mode, `\H` for help.

## Doc generation

```sh
bash gen_doc.sh    # cabal build + cabal run Nekomata -- --doc > doc/Builtins.md + codepage doc + analysis
```

## Choosing short chars for builtins/particles

When adding a new builtin or particle:

1. Pick a `short` character that is listed in `analysis/LiberationMonoGlyphs.txt` (ensures font coverage) but NOT already in the code page. Check by `grep`-ing the code page or scanning the tables below.
2. Add the chosen character to `src/Nekomata/CodePage.hs`, replacing one of the 30 unassigned `�` slots:
   - **0x89–0x8f** (7 slots, after particle block)
   - **0xb4–0xbf** (12 slots, mid uppercase block)
   - **0xe5–0xef** (11 slots, mid lowercase block)
3. Builtin `short` must NOT be a Unicode Modifier Letter; particle `short` MUST be a Unicode Modifier Letter.

Current code page rows with unassigned slots:

```
Row  8 (0x80): ᵒᵖʳᵗʷˣʸᶻᶾ�������
Row 11 (0xb0): ŽƵẐÞ������������
Row 14 (0xe0): ũůžƶþ�����������
```

## Adding builtins

1. Define the Haskell implementation in the appropriate `src/Nekomata/Builtin/` module.
2. Add a `Builtin` record to `builtins` list in `src/Nekomata/Builtin.hs` with `name`, `short`, `func`, `help`, `examples`.
3. Run tests to verify examples.

## Adding particles

1. Define the `Particle` (higher-order function on `Function`) in `src/Nekomata/Particle.hs`.
2. Add a `BuiltinParticle` record to `builtinParticles` with `name`, `short`, `particle`, `arity` string, `help`, `examples`.

## Dependencies

`arithmoi`, `containers`, `integer-roots`, `parsec` (library); `haskeline`, `optparse-applicative`, `bytestring` (executable); `hspec`, `unicode-data` (tests).
