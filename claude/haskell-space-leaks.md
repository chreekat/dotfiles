# Hunting Haskell space leaks

Read before chasing memory growth. The order below is the method: **classify
first, measure second, guess never.**

## First: is it actually a leak?

Three different problems wear the same symptom (memory goes up and stays up),
and they take three different tools. Decide which one you have before opening a
profiler.

- **Leak** — data is retained that the program can no longer use. A thunk chain,
  a lazy field pinning the buffer it was parsed from, an unbounded queue nobody
  drains. Fix: strictness or a broken retainer path.
- **Retention by design, priced badly** — the data is genuinely reachable and
  genuinely needed, but the representation costs many times what it should. Not
  a leak; a data-structure problem. Profilers will happily point at it and you
  will happily "fix" nothing. (A real case: reload rehydration retained ~10.9 kB
  per line of scrollback that the C layer next door held for ~1.1 kB. Every byte
  reachable, every byte wanted; the answer was the representation, not a bang.)
- **Not the Haskell heap at all** — RSS counts C allocations, arenas the RTS
  hasn't returned to the OS, and mmapped regions. 44 MB resident against 437 kB
  of live Haskell heap is normal when a C library owns the bulk data. **Never
  start a heap hunt from an RSS number.** Get live-heap and RSS side by side and
  see which one is moving.

The discriminating question for leak-vs-retention: *after the owner becomes
unreachable and a major GC runs, is it still live?* If yes, something still
points at it. If it was never unreachable, you have a representation problem.

## The measurement ladder

Climb only as far as the answer requires; each rung costs more setup and
distorts more.

1. **`+RTS -s` / `-t <file> --machine-readable`** — maximum residency, total
   allocation, GC time. No profiling build. If the binary has `-rtsopts`, you
   need **no rebuild at all**: pass flags through `GHCRTS`. Maximum residency is
   the retention figure that doesn't depend on when GCs happen to fire; prefer
   it to any instantaneous sample.
2. **`+RTS -hT -i0.1`** — heap census by closure type. Still **no profiling
   build**. This is the cheapest thing that answers "growing or churning," and
   it is usually enough to pick the next rung.
3. **`-finfo-table-map -fdistinct-constructor-tables` at compile time, run with
   `+RTS -hi -l`, rendered by `eventlog2html`** — info-table profiling gives
   every band a *source location*. Still **no profiling way needed**; those two
   flags emit an IPE map into the eventlog, and that map is the whole trick.
   This is the workhorse for "which allocation site." Put them behind a cabal
   flag so ordinary builds and the test suite stay uninstrumented.
4. **`+RTS -hb`** (biography: lag / drag / void / use) — the specific tool for
   classic thunk leaks. High *drag* means retained long after last use; high
   *void* means never used at all. Needs `-prof`, and it is slow.
5. **`ghc-debug`** — the only tool that answers **"who retains this?"** with an
   actual retainer path, on a *live, running* process, without restarting it.
   Reach for it when a profile shows what is retained but not why.
6. **Time/allocation profile (`--profiling-detail=late`)** — late cost centres
   attribute allocation without wrecking the inlining that non-late profiling
   destroys. Use it to find *who allocates*, which is a different question from
   who retains.

## Compile-time flags, by mechanism

Verified on GHC 9.10.3. The headline: **most of the ladder needs no profiling
build**, which is what lets you profile the binary you already have instead of
rebuilding and losing the repro.

| Mechanism | Build/link flags | Run flags |
|---|---|---|
| `-s` / `-t` summary stats | `-rtsopts` | `+RTS -s`, or `-t<file> --machine-readable` |
| Eventlog | *none* | `+RTS -l -ol<file>` |
| `-hT`, census by closure type | `-rtsopts` | `+RTS -hT -i0.1` |
| `-hi`, census by info table | `-rtsopts -finfo-table-map -fdistinct-constructor-tables` | `+RTS -hi -l -ol<file>` |
| Cost-centre profiles: `-p`, `-hc`, `-hy`, `-hd`, `-hr`, `-hb` | `-prof`, plus `-fprof-late` | `+RTS -p`, `-hb`, ... |
| ghc-debug | `ghc-debug-stub` dep, `-finfo-table-map`, `withGhcDebug` wrapping main | attach with `ghc-debug-brick` |

- **The eventlog needs nothing at all.** `-eventlog` is a leftover from when the
  eventlog lived in its own RTS way; every way carries it now and the flag is a
  no-op (true from GHC 9.4 as I recall, verified on 9.10). `-l` is even a *safe*
  RTS option: it works through `GHCRTS` and on the command line with no
  `-rtsopts`.
- **`-rtsopts` is the gate for everything else** — without it you get "Most RTS
  options are disabled. Link with -rtsopts to enable them." Put it in the
  executable stanza permanently. That one flag is what makes a shipped binary
  profilable through `GHCRTS` with no rebuild.
- **`-hT` and `-hi` do not need `-prof`.** `-hi` will even run *without*
  `-finfo-table-map`, and that is the trap: it succeeds silently and produces
  bands with no source provenance. `-finfo-table-map` is what attaches the
  source location; `-fdistinct-constructor-tables` is what stops every use site
  of a constructor from collapsing into one band.
- **`-p`, `-hc`, `-hb` and friends do need `-prof`** ("the flag -hc requires the
  program to be built with -prof"). `-fprof-late` (cabal
  `--profiling-detail=late`) inserts cost centres *after* optimization, so you
  get attribution without paying the inlining damage the default detail causes.
- **Keep the info-table flags behind a cabal `flag`** — they inflate binary size
  and compile time — and build every instrumented variant into **its own
  `--builddir`**, so the ordinary build and `cabal test` keep unprofiled
  artifacts.

```
# info-table heap profile -- note: NO --enable-profiling needed
cabal build -fprofiling --builddir=dist-prof exe:foo
$(cabal list-bin --builddir=dist-prof exe:foo) +RTS -hi -l -olfoo.eventlog
eventlog2html foo.eventlog

# cost-centre time/allocation profile -- this one does need the profiling way
cabal build --enable-profiling --profiling-detail=late --builddir=dist-prof exe:foo
```

```
flag profiling
    default:     False
    manual:      True
-- ...
    if flag(profiling)
        ghc-options: -finfo-table-map -fdistinct-constructor-tables
```

Mind the naming collision: a hand-rolled cabal flag called `profiling`
(`-fprofiling`) is **not** cabal's `--enable-profiling`. The first turns on
info-table maps in a normal build; the second switches to the profiling way.
Wanting both at once is legitimate; conflating them wastes an afternoon.

## Reading a heap profile

- A census **forces a major GC**, so the bands are live data, not floating
  garbage. That is exactly what makes them trustworthy.
- **Plateau = retained.** It climbed and stayed: the data is reachable. Hunt the
  retainer.
- **Sawtooth = churn.** Allocation rate, not a leak. If it hurts, it's a
  performance problem, not a space leak.
- **Staircase that never comes down** across repeated identical workloads is the
  clearest leak signature available. Design the workload to repeat.

## Scale the workload, don't stare at one run

One number tells you nothing. Run the same workload at N and 4N.

- Cost linear in N → **per-item retention**. Divide it out: bytes-per-item is
  the number to reason about and to compare against what the data actually is.
- Constant → fixed overhead, probably not your problem.
- **Superlinear → look for O(N²)**, usually an accumulating structure appended
  to in a loop.

Fix the workload and vary the *knobs* (build flags, RTS flags), never the other
way round, or runs aren't comparable.

## What actually causes them

Ranked by how often they're the answer, not by how interesting they are.

- **A lazy field pinning the buffer it was parsed from.** A small record built
  from a big array or ByteString keeps the whole source alive through one
  unforced field. Classic and vicious: the leak is proportional to the *source*,
  not the record. Fix: `{-# LANGUAGE StrictData #-}` on the module, plus
  `pure $!` at the construction site. For plain data types there is no reason to
  keep laziness you didn't ask for.
- **Text/ByteString slices** retaining a large parent buffer. `Data.Text.copy` /
  `ByteString.copy` when you keep a small slice of something big.
- **Lazy accumulators**: `foldl` (use `foldl'`), lazy `StateT` (use `Strict`),
  `modifyIORef` (use `modifyIORef'`), `Map.insertWith` building thunk chains in
  values (`insertWith'`-style forcing, or a strict map).
- **Unforced thunks parked in a long-lived `IORef`/`MVar`/`TVar`.** In a server
  these accumulate for the process's whole lifetime. Force on write.
- **Unbounded queues, caches, and CAFs.** Growth by design with no eviction. Not
  a leak, but it looks exactly like one; the fix is a bound, not a bang.

Strictness is a scalpel, not a hammer. Banging every field to make a graph flat
hides the real retainer and costs you laziness you may need. Find the path
first.

## Measurement landmines

These are cheap to hit and expensive to diagnose.

- **`cabal run` poisons `GHCRTS`-based stats.** `cabal` is itself a Haskell
  program, honors `GHCRTS`, and will overwrite your stats file. Use
  `cabal build` + `cabal list-bin` and exec the binary directly. (This is the
  one exception to "always work through cabal" — and it applies *only* to the
  measured invocation.)
- **Scope `GHCRTS` to the one process under measurement.** If the client and the
  server are the same binary, every client invocation clobbers the same stats
  path.
- **The RTS writes `-s`/`-t` stats only on a clean shutdown.** A SIGKILLed
  process writes nothing. Tear down through the app's own quit path.
- **`execve` runs no exit hooks**, and a successor image truncates the `.hp` the
  predecessor was writing. Across a self-restart, the stats you get are the
  *post-restart image alone* — which is often convenient, as long as you know it.
- **`-po<prefix>`** puts `.hp`/`.prof` where you want them instead of the
  caller's cwd.
- **An inherited `GHCRTS=-p` kills a non-profiled successor** — it rejects the
  unknown flag and prints RTS usage. Strip `GHCRTS` (`env -u GHCRTS`) for any
  child that isn't the one being measured.
- **`-A1m -F1.1`** gives a tighter RSS curve at the cost of speed, and perturbs
  allocation behavior. Useful for a shape, not for a number.
- Lazy `readFile` on `/proc` is forced after the process is gone; use
  `readFile'`.

## Make the fix stick

A leak fixed without a guard comes back.

- **Keep the harness in the repo as a script**, black-box over the real binary:
  fixed workload, syncing on observable state (never a fixed sleep), knobs as
  flags.
- **Pull the verdict out as a pure, unit-tested core**: measured live bytes vs a
  committed baseline and a tolerance band → `Regressed` / `WithinBand` /
  `Improved`. The band and the baseline are the thing that turns a one-off hunt
  into a ratchet, and the classification logic tests without the RTS.
- **Verify both directions at two sizes.** The bug-capture rule applies: the
  measurement must show the leak on the unfixed code and its absence on the fix,
  at N *and* at 4N — otherwise you've measured noise.
