# Testing rules

How tests earn their keep. These extend the rules in [CLAUDE.md](CLAUDE.md).

- Tests must exercise library code. That's the whole point.
- Every test is one of two kinds:
  - Behavior tests document how the system is meant to work. Keep them fast and
    light. A new feature's tests are all behavior tests -- there's no bug yet --
    and the TDD "failing test first" IS the behavior test: write it as the spec.
  - Bug-capture tests pin a specific fixed bug. Verify both directions: it must
    fail on the unfixed code and pass on the fix. Keep it permanently as a
    regression guard; unlike a behavior test, it earns its keep even when heavy,
    where the code already has a test suite.
- Bug-capture tests also map the terrain: a region that accumulates lots of them
  is "here be dragons" -- a fragile hotspot worth architectural attention.
- Behavior tests are a cost, not just an asset. Prune or lighten one when it
  duplicates coverage or ossifies a refactor. Don't write a new one per
  message/variant when an existing test already covers the path.
- Never use a fixed sleep/timer to synchronize a test. Sync on an observable
  event (await/poll with a bounded deadline). A timeout used as a deadline (fail
  if X hasn't happened by T) is fine; a sleep used as a guess is not.
- Every test must be parallelizable. A test that needs sequential execution is
  an emergency on par with flakiness (it usually IS latent flakiness: hidden
  shared state). Fix the test -- private dirs, scoped namespaces, injected
  clocks -- or question whether it belongs (wall-clock assertions are
  benchmarks, not tests); never just mark it sequential and move on.
- "Integration test" is a term reserved for testing the interaction of the
  system-under-test with other systems outside our direct control. They should
  be few and used sparingly.
  - The best integration test is real usage and good logging.
- The final tests for a task, including integration tests, SHOULD be scripted
  and included in the repo.
- Good tests demand good architecture. If it's hard to write small, targeted,
  orthogonal tests, STOP and reconsider the architecture.
  - Yet another reason for a pure core with a light wiring at the very top
    level. Wiring gets the integration test -- everything else is focused unit
    test.
