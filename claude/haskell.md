# Haskell rules

Baseline rules for any Haskell work. These extend the rules in
[CLAUDE.md](CLAUDE.md).

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
- An `errors.err` file in the project root means a ghcid/bhcid watcher is
  live: after editing, read `errors.err` instead of running `cabal build`;
  non-empty means the tree doesn't compile. Reloads are near-instant, so a
  plain read usually suffices; to be certain the contents postdate your edit,
  sync on the rewrite (`inotifywait -q -t 2 -e close_write errors.err`), never
  a sleep.
- For exception handling and structured concurrency (synchronous vs
  asynchronous exceptions, bracket/withAsync for lifetimes, link vs race for
  supervision, the ExitCode footgun), follow
  [haskell-exceptions.md](haskell-exceptions.md). Read it before writing
  exception handlers or spawning threads.
- For memory growth, retention, and heap profiling (leak vs. costly-but-correct
  retention vs. non-heap RSS, the -s/-hT/-hi/ghc-debug ladder, measurement
  landmines), follow [haskell-space-leaks.md](haskell-space-leaks.md). Read it
  before profiling memory or adding strictness to fix a heap graph.
