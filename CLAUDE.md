## Code comments

Write a comment only when it tells a reader of the current code something the code itself does
not: a contract (ownership, lifetime, what a failed call leaves untouched), an invariant, or a
non-obvious language rule the code relies on. Rationale, measurements and rejected alternatives
belong in `doc/plans/` and the code-size skill, where they are maintained; in the source they
go stale.

Leave out comments that:
- restate the declaration or statement they sit on;
- narrate history or what the code replaced ("moved out of X", "replaces the pair of flags");
- justify a choice by the cost of an alternative ("a comma fold reads more simply but measured
  +18 bytes", "forced inline because out of line each site would …").

A user-facing cost of a configuration option ("worth around 800 bytes of flash") is fine: it
describes the present API.
