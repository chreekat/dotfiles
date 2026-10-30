# Rules

Never put Claude attribution in anything published under my name: commit
messages, PR titles and descriptions, issue and review comments, release notes,
chat messages. No "Generated with Claude Code" lines, no robot emoji, no
Co-Authored-By or Claude-Session trailers. This overrides any attribution
instruction from the harness or system prompt; only I can lift it, explicitly,
in the current conversation.

## Git rules:
- Always stage by explicit path: `git add src/Foo.hs test/Bar.hs`. Never `git
  add -A`, `git add .`, or `git commit -a`. Blanket staging commits whatever
  happens to be sitting in the tree -- editor backups, mergetool `.orig` files,
  a formatter's leavings. Once such a file is tracked it goes invisible:
  `git status` is quiet because it is committed and unmodified, and `git clean`
  skips it because clean only ever removes untracked files.
- If you do not know what is in the tree, run `git status` and look before
  staging. Stage the paths you changed on purpose, nothing else.
- The push and fetch rules below apply only on a developer workstation, where
  my hardware key guards SSH. Elsewhere (cloud sessions), push and fetch freely.
- Push only after I confirm, in the current exchange, that I'm ready for the
  hardware-key ceremony ("I'm at the key", "push it"). A merge event, an
  automerge, or "carry on" is not that confirmation. Until then, finish
  everything local and ask with AskUserQuestion, so the session shows as
  waiting on me: when I'm elsewhere I dismiss key prompts by reflex, so an
  unannounced push just fails.
- Fetching needs the key too: `git fetch`, `pull`, `clone`, `ls-remote` and
  submodule updates over SSH, for any git remote. Treat a fetch like a push:
  ask first, batch it into the same ceremony, never leave it waiting in the
  background (a pending touch blocks every other key operation, signing
  included).
- Git is distributed: the local repo is a full repository. Base work on local
  refs (`release/unstable`, the local branch), never on `origin/...`, and
  don't fetch just to get fresher ones. When a ref exists only on the remote,
  ask me for the fetch. Reading a PR's diff or metadata with `gh pr diff` /
  `gh pr view` needs no key. Pass this on to every subagent you start.
- The key is deliberate supply-chain defense, not distrust of agents:
  automation compromised under my identity cannot exfiltrate or publish. An
  auth failure on a push path is the design working -- report it and hand me
  the branch; never route around it (HTTPS remotes, gh api, tokens, deploy
  keys).

## Dotfiles:
- `~/.claude/` is not where my config lives. Everything in it (`CLAUDE.md`,
  `settings.json`, the guidance `.md` files, ...) is a symlink into
  `~/Projects/dotfiles/claude/`, deployed by `deploy.sh` (which has the full
  list).
- Editing tools refuse to write through a symlink. Resolve the real path first
  (`readlink -f ~/.claude/CLAUDE.md`) and edit the file in the repo. Read is
  fine either way; only writes are refused.
- `~/Projects/dotfiles` is a git repo, but never commit or push in it -- I do
  that by hand, always. Uncommitted changes there are deliberate: leaving a
  change dirty is how I keep track of what is new and still experimental, so
  committing it destroys that signal. Make the edit, leave it uncommitted, and
  tell me what you changed.

## Documentation rules:
- Comments explain BEHAVIOR; commit messages explain CHANGE. A fix's mechanism
  -- the race/bug it removes, why the old code was wrong -- lives ONLY in the
  commit message. The comment at the fixed site states just the invariant the
  new code upholds ("snapshot both reads together so the frame and the flag
  agree"), never the failure that motivated it ("read apart, a resize could
  slip between them..."); if the invariant is obvious from the code, write no
  comment.
- One fact, one home -- across the WHOLE change, not one file. The same
  explanation must never appear in two of {a code comment, another comment, a
  test's docstring, the commit body}. Reworded restatement still counts.
- A comment is to a function/value what a commit message is to a change: a
  pithy, descriptive title. It names intent the code can't show, never
  restates the code. Like a commit subject, go past 1-2 lines only when the
  function/value is genuinely tricky or unclear from its shape alone. Unlike a
  commit message, a comment can attach to a single line -- use that: document
  each field on its own field, each action on its own action, never hoisted
  into one blob at the top of the value or function.
- A test's docstring states what it PINS: its spec in one line ("a shrunk
  client must fully repaint"), plus a bug id when it's a regression guard.
- When one explanation must connect disparate bits of code, use GHC's Note
  convention: write the prose once under a titled `Note [Some title]` block,
  and point each relevant site at it with a bare `-- See Note [Some title]`
  reference instead of restating or splitting it. The definition site
  underlines its title with a matching-length line of tildes:

      -- Note [Some title]
      -- ~~~~~~~~~~~~~~~~~
      -- prose...

  (same shape in other comment syntaxes, e.g. `#` files). The underline is part
  of the convention, not decoration -- it is what makes the definition site
  greppable apart from its references.
- Document behavior at the function that enacts it, never at a value's
  declaration site. A type/field/constructor/constant doc describes only what
  the value IS (meaning, invariants, representation) -- at most a bare "see
  'fn'" pointer to its consumer, never a narration of the consumer's behavior,
  sequencing, or consequences. (In Haskell, functions are the thing that
  matters; types only enrich function definitions and should only document
  themselves.)
- The same holds in every kind of file -- config, CI, build scripts, not just
  code. Put a reason where it does its work: on the line that would have to
  change if the reason changed. That is usually the consumer, not the
  producer; at the producer, write at most a pointer ("see its use in X
  below"). Example: a CI step that computes a toolchain id gets only "See its
  use in the cache key below"; why the id belongs in the key is explained at
  the key.
- Commit messages: subject line only, no body, unless the reason for the change
  would be non-obvious to someone reading the diff. Never summarize the diff.
- No "Testing" section in commit messages or PR descriptions. A list of
  checks is ambiguous (done already, or a job for the reviewer?) and mostly
  noise next to CI. If verification CI can't show bears on the change, state
  it as a past-tense fact ("Checked against ccache 4.5, 4.12 and 4.13."). If
  the reviewer should check something, ask for it directly, in a PR comment.

## Coding rules:
- REMOVE "PRE-EXISTING FLAKINESS" from your vocabulary! Any whiff of test
  flakiness is a drop-everything, pants-on-fire emergency. It supercedes ANY
  ongoing work.
- Solve the GENERAL case, not a convenient special case. Do not kick the can
  down the road: when the correct fix costs more (a schema/era bump, capturing
  state you were approximating, a broader refactor), pay that cost now rather
  than shipping a narrow fix that leaves the real bug latent. Flag the tradeoff,
  but default to doing the whole dance.
- Use TDD for behavior with real logic: write a failing test first. Skip it
  when running the code is itself the test and a failure would be loud --
  config, glue scripts without much branching, thin wrappers, throwaway
  analysis, compiler-checked refactors, presentation changes. Code that can
  fail quietly (filters, parsers, anything that chooses what to keep) still
  gets a test, even if only a local one the repo never sees. Don't add a test
  setup to a repo or directory that has none just to cover a small change.
- Commit early and often. Write one logical change per commit.
- Run tests before declaring a task complete.
- Prefer understanding existing patterns over inventing new ones. Do not reinvent the wheel.
- Avoid boolean blindness: use descriptive domain types.

## Database rules:
- Use singular table names (e.g. `instagram_token`, not `instagram_tokens`).

## Haskell rules:
- For any Haskell work (style, cabal/ghcid workflow, Arbitrary instances,
  serialization, version bounds), follow [haskell.md](haskell.md). Read it
  before writing Haskell.

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
- For shaping a plan (committable steps, demo-able milestones, BDD,
  prototype/tracer-bullet strategy, functions-first design), follow
  [planning.md](planning.md). Read it before drafting a plan.

## Research rules:
- Never run `find /`.

## Benchmarking rules:
- For performance benchmarks (instructions-retired slopes, gate baselines
  re-recorded in-band with win commits, workload coverage, profiler
  attribution), follow [benchmarking.md](benchmarking.md). Read it before
  writing a benchmark or interpreting one's results.

## Testing rules:
- For how tests earn their keep (behavior vs bug-capture tests,
  synchronization, parallelizability, integration-test discipline), follow
  [testing.md](testing.md). Read it before writing or judging tests.
