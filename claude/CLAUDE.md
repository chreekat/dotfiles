# Rules

Coding rules:
- Use TDD: write a failing test before implementing behavior.
- Commit early and often. Write one logical change per commit.
- Run tests before declaring a task complete.
- The final tests for a task, including integration tests, SHOULD be scripted and
  included in the repo.
- Ask before introducing new dependencies.
- Prefer understanding existing patterns over inventing new ones. Do not reinvent the wheel.
- Comments explain BEHAVIOR, commit messages explain CHANGE. Only add comments
  about changes if it's absolutely critical to understanding the code on its
  own.
- Commit messages: subject line only, no body, unless the reason for the change
  would be non-obvious to someone reading the diff. Never summarize the diff.

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
