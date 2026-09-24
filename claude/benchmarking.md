# Benchmarking principles

How to build and run performance benchmarks. Distilled from the HAT
typing-perf work; applies anywhere.

## What to measure

- Measure **instructions retired**, not wall time. Instruction counts are
  immune to load, frequency scaling, and background noise — no idle-box
  requirement, unlike any time- or cycle-based metric.
- Measure the **marginal cost**: run the workload at several sizes N and fit
  `instructions = slope*N + intercept`. The slope is the per-unit cost; the
  intercept absorbs startup.
- Sizes must be large enough that `slope*N` stands clear of the intercept.
  A slope fitted under a dominating intercept is noise — it can swing ±40%
  run to run or even come out negative. Give each workload its own size
  range; never force one range onto all workloads.
- Black box: the real binary through its real entry points (pty, socket,
  CLI). Pair every `cabal list-bin` with a build in the same step — list-bin
  happily hands back a stale binary.

## The gate

- Commit a baseline file of fitted slopes; a bench-gate run re-measures and
  trips on any out-of-band series — regressions AND stale improvements
  ("improved; record a new baseline"). An improvement that isn't recorded is
  a regression detector that no longer works.
- The gate must measure **every series the baseline lists**. A hand-added
  row the gate never measures fails every run (or worse, is silently never
  checked).
- Tolerance is a relative band **plus an absolute slack floor**. A
  near-zero series (an idle client's slope) is pure fit noise; ±20% of
  nothing trips on nothing. Any flake in the gate is a drop-everything bug.
- Re-record the baseline **inside the commit that changes the numbers**.
  The gate file then ratchets downward through history: each win commit
  carries its own in-band proof, and `git log` on the baseline is the
  performance changelog.

## Workload coverage

- One workload per structural case, not per feature. A structural blind
  spot hides multiples: a layout shape no workload exercised concealed a
  40x per-keystroke cost.
- When a blind spot is found: add the workload and record its (bad) number
  in one commit, land the fix with the re-record in the next. The
  before/after pair lives in history.

## Attribution

- The fitted instruction slope is ground truth; profilers only attribute
  it. Ladder: instruction slope -> cost-centre profile (language-level) ->
  `perf record` (native/RTS/GC split). Each sees what the previous cannot:
  a cost-centre profile is blind to C libraries and GC; a flat profile
  means the remaining cost is elsewhere, not that you are done.
- Cycle percentages understate instruction-dense loops (superscalar IPC).
  When the gate counts instructions, trust the instruction delta over the
  cycle share.
- Before believing a delta, repeat the run. Know each series' run-to-run
  variance; a single outlier is a fit artifact, not a result.
- Diagnose regressions by differential profile: same profile, before and
  after, side by side. A symbol present in one and absent in the other
  (e.g. GC's `evacuate`) names the mechanism.
