# Planning rules

How to shape a plan before work starts. These extend the rules in
[CLAUDE.md](CLAUDE.md).

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
