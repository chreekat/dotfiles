# Rules

Unless specifically overruled, never add any claude-specific trailers to git
messages. (Past examples: Co-Authored-By, Claude-Session)

## Git rules:
- Always stage by explicit path: `git add src/Foo.hs test/Bar.hs`. Never `git
  add -A`, `git add .`, or `git commit -a`. Blanket staging commits whatever
  happens to be sitting in the tree -- editor backups, mergetool `.orig` files,
  a formatter's leavings. Once such a file is tracked it goes invisible:
  `git status` is quiet because it is committed and unmodified, and `git clean`
  skips it because clean only ever removes untracked files.
- If you do not know what is in the tree, run `git status` and look before
  staging. Stage the paths you changed on purpose, nothing else.

## Dotfiles:
- `~/.claude/` is not where my config lives. `CLAUDE.md`, `settings.json`,
  `keybindings.json`, `architecture-defaults.md`, `haskell-exceptions.md`,
  `statusline-command.sh` and `notify` are all symlinks into
  `~/Projects/dotfiles/claude/`, deployed by `deploy.sh`.
- Editing tools refuse to write through a symlink. Resolve the real path first
  (`readlink -f ~/.claude/CLAUDE.md`) and edit the file in the repo. Read is
  fine either way; only writes are refused.
- `~/Projects/dotfiles` is a git repo, but never commit or push in it -- I do
  that by hand, always. Uncommitted changes there are deliberate: leaving a
  change dirty is how I keep track of what is new and still experimental, so
  committing it destroys that signal. Make the edit, leave it uncommitted, and
  tell me what you changed.

## Documentation rules:
- One fact, one home -- across the WHOLE change, not one file. The same
  explanation must never appear in two of {a code comment, another comment, a
  test's docstring, the commit body}. Reworded restatement still counts. A
  fix's mechanism -- the race/bug it removes, why the old code was wrong -- is
  CHANGE: it lives ONLY in the commit message. The comment at the fixed site
  states just the invariant the new code upholds ("snapshot both reads together
  so the frame and the flag agree"), never the failure that motivated it ("read
  apart, a resize could slip between them..."); if that invariant is obvious
  from the code, write no comment. When a rationale genuinely spans several
  sites, use the Note convention (one prose block, bare pointers), never a
  copy at each site.
- A test's docstring states what it PINS: its spec in one line ("a shrunk
  client must fully repaint"), plus a bug id when it's a regression guard. It
  does not re-narrate the bug's mechanism -- that's already in the commit that
  fixed it.
- A comment is to a function/value what a commit message is to a change: a
  pithy, descriptive title. Never let a comment duplicate the code it describes
  -- code is self-documenting; the comment names intent the code can't. Like a
  commit subject, go past 1-2 lines only when the function/value is genuinely
  tricky or unclear from its shape alone. Unlike a commit message, a comment
  can attach to a single line -- use that: document each field on its own field,
  each action on its own action, never hoisted into one blob at the top of the
  value or function. When one explanation must connect disparate bits of code,
  use GHC's Note convention: write the prose once under a titled `Note [Some
  title]` block, and point each relevant site at it with a bare `-- See Note
  [Some title]` reference instead of restating or splitting it.
- Document behavior at the function that enacts it, never at a value's
  declaration site. A type/field/constructor/constant doc describes only what
  the value IS (meaning, invariants, representation) -- at most a bare "see
  'fn'" pointer to its consumer, never a narration of the consumer's behavior,
  sequencing, or consequences. (In Haskell, functions are the thing that
  matters; types only enrich function definitions and should only document
  themselves.)
- Commit messages: subject line only, no body, unless the reason for the change
  would be non-obvious to someone reading the diff. Never summarize the diff.

## Coding rules:
- REMOVE "PRE-EXISTING FLAKINESS" from your vocabulary! Any whiff of test
  flakiness is a drop-everything, pants-on-fire emergency. It supercedes ANY
  ongoing work.
- Solve the GENERAL case, not a convenient special case. Do not kick the can
  down the road: when the correct fix costs more (a schema/era bump, capturing
  state you were approximating, a broader refactor), pay that cost now rather
  than shipping a narrow fix that leaves the real bug latent. Flag the tradeoff,
  but default to doing the whole dance.
- Use TDD: write a failing test before implementing behavior.
- Commit early and often. Write one logical change per commit.
- Run tests before declaring a task complete.
- Prefer understanding existing patterns over inventing new ones. Do not reinvent the wheel.
- Comments explain BEHAVIOR, commit messages explain CHANGE. Only add comments
  about changes if it's absolutely critical to understanding the code on its
  own.
- Avoid boolean blindness: use descriptive domain types.

## Database rules:
- Use singular table names (e.g. `instagram_token`, not `instagram_tokens`).

## Haskell rules:
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
- For exception handling and structured concurrency (synchronous vs
  asynchronous exceptions, bracket/withAsync for lifetimes, link vs race for
  supervision, the ExitCode footgun), follow
  [haskell-exceptions.md](haskell-exceptions.md). Read it before writing
  exception handlers or spawning threads.
- For memory growth, retention, and heap profiling (leak vs. costly-but-correct
  retention vs. non-heap RSS, the -s/-hT/-hi/ghc-debug ladder, measurement
  landmines), follow [haskell-space-leaks.md](haskell-space-leaks.md). Read it
  before profiling memory or adding strictness to fix a heap graph.

## Nix rules:
- Never use 'with'.

## Confidence and honesty rules:
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

## Architecture defaults:
- For code shape, layering, testing, logging, database access, and scheduled
  work, follow [architecture-defaults.md](architecture-defaults.md). Read it
  when starting non-trivial new code in any of those areas.

## Planning rules:
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

## Research rules:
- Never run `find /`.

## Testing rules:
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
