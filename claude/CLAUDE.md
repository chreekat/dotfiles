# Rules

Unless specifically overruled, never add any claude-specific trailers to git
messages. (Past examples: Co-Authored-By, Claude-Session)

Coding rules:
- REMOVE "PRE-EXISTING FLAKINESS" from your vocabulary! Any whiff of test
  flakiness is a drop-everything, pants-on-fire emergency. It supercedes ANY
  ongoing work.
- Use TDD: write a failing test before implementing behavior.
- Commit early and often. Write one logical change per commit.
- Run tests before declaring a task complete.
- Prefer understanding existing patterns over inventing new ones. Do not reinvent the wheel.
- Comments explain BEHAVIOR, commit messages explain CHANGE. Only add comments
  about changes if it's absolutely critical to understanding the code on its
  own.
- Document behavior at the function that enacts it, never at a value's
  declaration site. A type/field/constructor/constant doc describes only what
  the value IS (meaning, invariants, representation) -- at most a bare "see
  'fn'" pointer to its consumer, never a narration of the consumer's behavior,
  sequencing, or consequences. (In Haskell, functions are the thing that
  matters; types only enrich function definitions and should only document
  themselves.)
- Commit messages: subject line only, no body, unless the reason for the change
  would be non-obvious to someone reading the diff. Never summarize the diff.
- Avoid boolean blindness: use descriptive domain types.

Database rules:
- Use singular table names (e.g. `instagram_token`, not `instagram_tokens`).

Haskell rules:
- Always write shrinks for Arbitrary instances. If the type does not admit a
  good shrink, suggest how it could be modified to enable it.
- Do not improvise on version bounds. Use existing bounds if found in the
  project, otherwise ask me what they should be.
- Do not use show or read for serializing or deserializing. They are only for
  debugging.
- Never pipe `cabal build` to 'head'. Cabal does not handle SIGPIPE properly.
  You CAN pipe it to 'tail', however.
- Use four-space indent.
- Use diff-minimizing whitespace.
- Always work through cabal (or stack) with `cabal build`, `cabal run`, and
  `cabal test`. Avoid the habit of calling built binaries directly and running
  stale artifacts.

Nix rules:
- Never use 'with'.

Confidence and honesty rules:
- Do not present guesses as facts. Qualify uncertain statements with confidence
  indicators: "I think ... but I'm not sure", "With ~[some %] confidence, ...",
  "Assuming X, then probably Y".
- When you don't know something, say so. "I don't know" is always a valid and
  valuable answer -- it maintains trust and is far more useful than a
  confident-sounding guess. People who say "I don't know" get trusted with
  harder problems.
- Distinguish between: what you've verified (read the code, ran the test),
  what you're inferring (pattern-matching on context), and what you're
  speculating about (no direct evidence).

Architecture defaults:
- For code shape, layering, testing, logging, database access, and scheduled
  work, follow [architecture-defaults.md](architecture-defaults.md). Read it
  when starting non-trivial new code in any of those areas.

Planning rules:
- A plan should be broken down into committable steps. A rule of thumb seems to be
  that a single step takes 1-8 commits.
- For prompts requiring larger plans -- say, more than 5 steps -- it should
  first be broken into larger, demo-able milestones of 1-5 steps. Any larger
  than that and we need to have a discussion.
- Think carefully about when a prototype or a tracer bullet might be a better
  strategy for developing new features.
- Use BDD: organize new development into observable, demoable behaviors.
- Functions are the fundamental unit of organization.
- Types and database schemas are secondary artifacts to functions. Only develop
  them on demand. Start with small types and grow them as a function is fleshed
  out.
- Planning artifacts are living documents. They should not contain their own
  history (e.g. references to course corrections or responses to feedback).
  When modified, they should keep their point-in-time nature.

Research rules:
- Never run `find /`.

Testing rules:
- Tests must exercise library code. That's the whole point.
- Every test is one of two kinds:
  - Behavior tests document how the system is meant to work. Keep them fast and
    light. A new feature's tests are all behavior tests -- there's no bug yet --
    and the TDD "failing test first" IS the behavior test: write it as the spec.
  - Bug-capture tests pin a specific fixed bug. Verify both directions: it must
    fail on the unfixed code and pass on the fix. Keep it permanently as a
    regression guard; unlike a behavior test, it earns its keep even when heavy.
- Bug-capture tests also map the terrain: a region that accumulates lots of them
  is "here be dragons" -- a fragile hotspot worth architectural attention.
- Behavior tests are a cost, not just an asset. Prune or lighten one when it
  duplicates coverage or ossifies a refactor. Don't write a new one per
  message/variant when an existing test already covers the path.
- Never use a fixed sleep/timer to synchronize a test. Sync on an observable
  event (await/poll with a bounded deadline). A timeout used as a deadline (fail
  if X hasn't happened by T) is fine; a sleep used as a guess is not.
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
