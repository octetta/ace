# ace and Zig `comptime`: Two Approaches to Compile-Time Computation

Both ace (Gosling, 1989) and Zig's `comptime` (Kelley, ~2016) address the same
fundamental problem: how do you let the programmer move work from runtime to
compile time without sacrificing readability of the source? They arrive at very
different answers, and the contrast is instructive.

---

## The Core Idea in Each

**ace** works by transformation. You write ordinary C, annotate it with rewrite
rules, and ace rewrites the syntax tree before handing it to the compiler. The
source stays readable; the optimised form is generated. The programmer reasons
about *patterns* and *substitutions*.

**Zig `comptime`** works by evaluation. You write Zig code and mark values or
parameters as `comptime`, which causes the compiler to evaluate that code at
compile time using a built-in interpreter. The programmer reasons about
*values* and *types* known at compile time.

---

## A Concrete Example: Compile-Time Dispatch

The classic ace example is selecting a code path based on a parameter that is
known at compile time.

### ace

```c
/* Rules fire when the third argument is a known constant */
$replace bool($0, $1, 0); $with $0 | $1;
$replace bool($0, $1, 1); $with $0 & $1;

x = bool(a, b, 0);   /* => a | b  — the call vanishes entirely */
y = bool(a, b, 1);   /* => a & b */
z = bool(a, b, op);  /* no rule matches — left as a runtime call */
```

The rule fires only when the pattern matches exactly. If the third argument is
not the literal `0` or `1`, the expression is left alone. No runtime overhead,
no special syntax at the call site.

### Zig

```zig
fn bool_op(a: u32, b: u32, comptime op: u2) u32 {
    return switch (op) {
        0 => a | b,
        1 => a & b,
        else => @compileError("unknown op"),
    };
}

const x = bool_op(a, b, 0);  // switch evaluated at compile time
const y = bool_op(a, b, 1);
// bool_op(a, b, op) where op is runtime — compile error
```

`comptime` is explicit in the function signature. Passing a runtime value for
`op` is a compile error, not a silent fallback. The compiler evaluates the
switch and emits only the selected branch.

---

## A Concrete Example: Loop-Invariant Code Motion

The flagship ace example: a test inside a loop that doesn't change between
iterations.

### ace

```c
$replace da > 0; $with 1;

for (i = 0; i < 10; i++) {
    if (da > 0) A[i]++;
    else        A[i]--;
}
```

ace rewrites `da > 0` to `1`, the dead `else` branch is eliminated by constant
folding, and the compiler sees a clean loop with no conditional. The original
source remains a single, readable loop.

### Zig

Zig does not have a direct equivalent for this pattern. You would either write
two loops manually, use a comptime parameter, or rely on the optimiser:

```zig
fn update(comptime increasing: bool, A: []i32) void {
    for (A) |*x| {
        if (increasing) x.* += 1
        else            x.* -= 1;
    }
}

// Caller decides at compile time:
update(da > 0, &A);   // da > 0 must be comptime-known for this to specialise
```

If `da > 0` is only known at runtime, Zig gives you no clean way to hoist it.
You write two versions and dispatch manually — which is exactly what ace
automates.

---

## Comparing the Mechanisms

| | ace | Zig `comptime` |
|---|---|---|
| **Mechanism** | Syntax-tree rewriting | Compile-time interpretation |
| **Input language** | C (extended with `$` directives) | Zig |
| **Rules defined by** | Programmer, in source file | Language semantics + `comptime` keyword |
| **Granularity** | Any expression or statement pattern | Values and types |
| **Fallback behaviour** | Rule doesn't match → code unchanged | Runtime value for `comptime` param → compile error |
| **Type awareness** | None — purely structural | Full — types are first-class comptime values |
| **Metaprogramming style** | Pattern / rewrite (term rewriting) | Evaluation (partial evaluation) |
| **Tooling required** | ace preprocessor before the compiler | Built into the Zig compiler |
| **Visibility** | Transformed output is inspectable C | No intermediate form exposed |

---

## Where ace Is Stronger

**Retroactive optimisation.** ace rules can be applied to existing code without
changing it. You add a `$replace` directive at the top of a file and the whole
file is transformed. In Zig, every function that needs specialisation must be
written with `comptime` parameters from the start.

**Structural pattern matching.** ace matches on the *shape* of expressions, not
on their values. The rule `$replace !($0 && $1); $with !$0 || !$1;` applies
DeMorgan's law to any expression of that form, regardless of what `$0` and `$1`
are. Zig has no equivalent — you cannot write a rule that transforms all
expressions of a given syntactic shape.

**Separation of concerns.** The algorithm and the optimisation strategy live in
different places. A library author ships readable source; a performance-conscious
user adds ace rules for their deployment context. In Zig, the specialisation
strategy is baked into the function signature.

**Dead branch elimination across arbitrary code.** The `$pullout` / `$assume`
pattern in ace can hoist any invariant test out of any loop body, no matter how
complex the body is. Zig's optimiser may or may not do this depending on whether
the condition is comptime-known.

---

## Where Zig `comptime` Is Stronger

**Type-level computation.** In Zig, types are values. You can write functions
that return types, build generic data structures, and do compile-time reflection.
ace is entirely type-blind — it matches syntax, not semantics. `$f0` restricts
to side-effect-free expressions, but there is no concept of "match only if this
expression has type `int`."

**Safety.** Zig's `comptime` is part of the language specification and is
verified by the compiler. ace pattern matching is best-effort: a rule that
matches the wrong thing will silently produce incorrect code. There is no
type-checking of rewrite rules.

**Turing-complete metaprogramming.** `comptime` in Zig can run arbitrary code —
loops, recursion, data structure construction. ace rules are rewrite rules;
they are powerful but not Turing-complete in the same sense.

**Integration.** `comptime` is in the compiler. There is no extra build step, no
separate tool to install, no risk of the preprocessor and compiler going out of
sync.

**Explicit contracts.** A function with a `comptime` parameter makes a
verifiable promise: this argument will be resolved at compile time. ace rules
make no such promise — whether a rule fires depends on whether the pattern
matches, which is not checked by any tool.

---

## The Deeper Difference: When You Decide

The sharpest distinction is *when the decision about specialisation is made*.

In Zig, specialisation is decided **when the code is written**. The author
of a function chooses which parameters are `comptime`. Users of that function
must respect that contract.

In ace, specialisation is decided **when the rules are written**, which can be
after the code is written, by a different person, for a specific deployment. You
can take a loop that was written without any thought of specialisation and add a
`$pullout` directive that generates specialised versions of it. The original
author need not have anticipated this.

This makes ace more like a *profile-guided* or *use-site-directed* optimisation
tool, and Zig `comptime` more like a *design-time* tool. Both are valid; they
answer different questions.

---

## Are They Compatible Ideas?

Yes — and it is not hard to imagine a language that combines both. You would
want:

- `comptime` for type-level computation and safe, verified specialisation at
  design time
- ace-style rewriting for post-hoc, pattern-directed optimisation that does not
  require anticipation by the original author

In fact, Zig's `comptime` already does a limited version of ace's constant
folding (evaluating `if (comptime_value)` and eliminating dead branches). The
difference is that ace generalises this to *arbitrary user-defined patterns*
rather than restricting it to values the type system already knows are constant.

A modern descendant of ace, integrated into a typed language, could conceivably
match on types as well as structure — firing a rule only when `$0` is known to
be a pointer, or only when a loop bound is a compile-time constant. That would
be a significant extension of what Gosling described in 1989, and it remains,
as far as the authors are aware, unbuilt.

---

## Summary

| Question | ace | Zig `comptime` |
|---|---|---|
| What is specialised? | Any expression matching a pattern | Values and types marked `comptime` |
| Who decides? | The rule author, after the fact | The function author, at design time |
| Type-aware? | No | Yes |
| Safe? | No — silent mismatch possible | Yes — compile error on violation |
| Retroactive? | Yes | No |
| Requires rewrite of existing code? | No | Yes |
| Turing-complete meta? | No | Yes |

ace and Zig `comptime` are not competing solutions to the same problem. They
occupy different points in the design space: ace is a post-hoc, structural,
type-blind transformer; `comptime` is an integrated, semantic, type-aware
evaluator. The comparison is worth making precisely because each reveals what
the other lacks.
