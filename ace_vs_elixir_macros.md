# ace and Elixir Macros: Two AST-Rewriting Systems

Of the comparisons one might make with ace, Elixir's macro system is the most
structurally similar. Both operate on syntax trees. Both allow the programmer
to define transformations that fire at compile time and produce new code. Both
are powerful enough to define new control-flow constructs. The comparison is
therefore not "are these the same idea" — they clearly share a lineage — but
"where do the design choices diverge, and what does that reveal?"

---

## The Core Idea in Each

**ace** is a preprocessor. It reads C source, builds a parse tree, matches
patterns against nodes in that tree using user-defined rewrite rules, substitutes
the replacements, and emits transformed C. The rules are written in the source
file itself using `$replace` directives. The programmer defines patterns
declaratively; ace drives the traversal.

**Elixir macros** are functions that run at compile time and receive their
arguments as quoted AST fragments, which they manipulate using ordinary Elixir
code before returning a new AST fragment to be spliced into the program. The
programmer writes imperative transformation code; the macro system calls it at
the right moment.

Both are AST-to-AST transformations. The difference is in how those
transformations are expressed.

---

## A Concrete Example: Unless

A classic Elixir macro is `unless`, which doesn't exist as a keyword but can
be defined as a macro:

### Elixir

```elixir
defmacro unless(condition, do: body) do
  quote do
    if !unquote(condition) do
      unquote(body)
    end
  end
end

unless x > 0 do
  IO.puts "not positive"
end
```

The macro receives the AST for `x > 0` and the AST for the body, wraps them
in a negated `if`, and returns that AST. Full Elixir is available inside the
macro to construct the result.

### ace

```c
$replace unless($0, $1); $with if(!$0) $1;

unless(x > 0, { puts("not positive"); });
```

The ace rule is a direct structural substitution. `$0` captures the condition,
`$1` captures the body, and the replacement splices them into a negated `if`.
No code runs; the rule is a template.

Both approaches work. The Elixir macro is more verbose but has the full language
available. The ace rule is a one-liner but can only express what a template can
express.

---

## A Concrete Example: Loop Optimisation

This is where the comparison gets interesting, because ace was designed for
something Elixir macros are rarely used for: performance transformation of
existing code.

### ace

```c
$replace $f0 = $0; $with $0;   /* eliminate identity assignments */
$replace !($0 && $1); $with !$0 || !$1;  /* DeMorgan */

/* These rules apply to ALL matching expressions in the file below,
   including ones written before the rules were defined. */

a = a;           /* eliminated */
if (!(p && q))   /* rewritten to if (!p || !q) */
    r = 1;
```

Rules in ace are applied retroactively to everything that follows (and, with
`$replaceafter`, after subnode reduction). The programmer does not need to
change the code being transformed.

### Elixir

Elixir macros do not apply retroactively. A macro must be explicitly called.
You cannot write a macro that scans all expressions in a module and rewrites
any that match a pattern. The closest approximation would be a custom compiler
pass using the `@before_compile` hook or a parse transform, but this is well
outside normal macro usage and requires deep integration with the compiler.

```elixir
# There is no Elixir equivalent of:
#   $replace !(a && b); $with !a || !b;
# applied transparently to all code that follows.
# You must explicitly call a macro at each site.
demorgan(!(p && q))  # explicit call required
```

This is a fundamental difference in philosophy. ace rules are *ambient* — they
reshape the world around them. Elixir macros are *explicit* — they transform
only what is directly passed to them.

---

## Comparing the Mechanisms

| | ace | Elixir macros |
|---|---|---|
| **Representation** | Internal parse tree (C AST) | Elixir quoted expressions (`{head, meta, args}`) |
| **Rule expression** | Declarative pattern + template | Imperative function over AST |
| **Pattern matching** | Structural, with meta-variables | Elixir pattern matching on AST tuples |
| **Transformation power** | Template substitution | Full Turing-complete Elixir code |
| **Application** | Ambient — fires on all matching nodes | Explicit — called at each use site |
| **Hygiene** | None — all names are literal | Hygienic by default (`quote/unquote`) |
| **Type awareness** | None | None (Elixir is dynamically typed) |
| **Host language** | C (with `$` extensions) | Elixir itself (homoiconic) |
| **Tooling** | External preprocessor | Built into the compiler |
| **Composability** | Rules compose by ordering | Macros compose by calling each other |

---

## Where ace Is Stronger

**Ambient application.** This is ace's most distinctive property. A single
`$replace` directive transforms every matching expression in the file below it.
In Elixir you must call a macro at every site where you want the transformation.
For performance work — where the goal is to systematically eliminate a class of
inefficiency throughout a codebase — ambient application is a significant
advantage.

**Retroactive optimisation.** You can take an existing C file, prepend ace
rules, and get a transformed version without touching the original code at all.
The Elixir macro system has no analogue of this. Every macro must be anticipated
at the point of authorship.

**Simplicity of rule definition.** A DeMorgan rule in ace is one line. The
equivalent in Elixir requires writing a macro that pattern-matches on AST tuples,
constructs a new AST, and handles quoting correctly. For simple structural
transformations, ace's declarative syntax is substantially less ceremony.

**Side-effect-free matching.** ace's `$f0`…`$f9` meta-variables restrict
matching to side-effect-free expressions, which allows rules like
`$replace $f0 = $0; $with $0;` to safely eliminate identity assignments. Elixir
macros receive ASTs and have no built-in mechanism for reasoning about whether
an expression has side effects; the macro author must implement this themselves.

---

## Where Elixir Macros Are Stronger

**Turing-complete transformation.** An Elixir macro is a function. It can loop,
recurse, query external data, call other functions, and build arbitrarily complex
ASTs. ace rules are templates — powerful within their domain but fundamentally
limited to structural substitution with capture variables.

**Hygiene.** Elixir macros are hygienic by default. Variables introduced inside
a macro do not leak into the caller's scope, and the macro cannot accidentally
capture variables from the caller. ace has no hygiene mechanism whatsoever.
A rule that introduces a temporary name will collide with any variable of the
same name in the surrounding code.

**Homoiconicity.** Elixir code *is* Elixir data. The AST is made of ordinary
Elixir tuples, lists, and atoms that you manipulate with the same tools you use
for everything else. There is no separate metalanguage. In ace, the `$replace`
syntax is a bolt-on extension to C; the pattern language is separate from C
itself.

**Macro composition.** Elixir macros are functions and compose naturally —
one macro can call another, build on another's output, or be parameterised by
another. ace rules compose only by ordering: earlier rules fire first, and the
output of one rule is input to subsequent rules, but there is no way to define
a rule in terms of another rule.

**Compile-time computation.** An Elixir macro can compute values at compile
time, not just rearrange structure. A macro can read a file, query a database,
or compute a lookup table and splice the result directly into the AST as a
literal. ace cannot compute new values; it can only rearrange and constant-fold.

**Integration and safety.** Elixir macros are part of the language. They are
subject to the same error reporting, tooling, and module system as everything
else. ace is an external tool that produces C source; if a rule produces
syntactically invalid output, the error appears in the generated C and may be
hard to trace back.

---

## The Hygiene Problem in More Depth

Hygiene deserves a longer look because it is where ace's design is most
obviously incomplete.

Consider this ace rule, which introduces a temporary variable:

```c
$replace swap($0, $1);
$with { int tmp = $0; $0 = $1; $1 = tmp; }

swap(a, tmp);   /* disaster: tmp is used as both the temp and the argument */
```

ace will happily generate:

```c
{ int tmp = a; a = tmp; tmp = tmp; }
```

which is wrong. Elixir's `quote` macro generates fresh, unambiguous names for
introduced variables by default. The programmer has to opt out of hygiene
(`var!`) to get the ace-like behaviour.

This is not a minor point. For the kinds of transformations ace was designed
for — inline expansion of complex operations — variable capture bugs are a real
hazard. The original ace documentation does not address this.

---

## The Deeper Difference: Declarative vs Imperative Metaprogramming

The comparison ultimately reduces to a classic tension in language design.

ace takes a **declarative** approach: you state *what* should be transformed and
*into what*, and the system figures out *how* to traverse the tree and apply the
rules. This is the approach of term rewriting systems, logic programming, and
algebraic simplifiers. It is concise for the cases it covers and opaque about
what it cannot express.

Elixir macros take an **imperative** approach: you write code that *describes
how* to transform the AST. This is more verbose for simple cases but scales
without limit — anything you can compute, you can use to drive the
transformation.

The tradeoff is familiar: declarative systems are easier to reason about and
optimise (the system can reorder or parallelise rules); imperative systems are
more expressive but harder to analyse. ace could in principle check that rules
are confluent (that they produce the same result regardless of application
order). Elixir macros, being arbitrary code, cannot.

---

## What a Synthesis Might Look Like

The features that each system has and the other lacks suggest what an ideal
system might combine:

- **From ace**: ambient application, declarative pattern syntax, side-effect-free
  matching constraints, retroactive applicability to existing code
- **From Elixir**: hygiene, homoiconicity, Turing-complete transformation,
  natural macro composition, compile-time computation

Racket's macro system (`syntax-rules` for declarative, `syntax-case` for
imperative) is perhaps the closest existing example of such a synthesis. It
provides both a pattern-matching declarative layer and an escape into full Racket
for cases that need it, with hygiene throughout. It lacks ace's ambient
application, but that may be a feature rather than a bug — explicit call sites
make it easier to understand what code has been transformed.

The ace property that remains genuinely unmatched in modern macro systems is
retroactive ambient transformation: the ability to state a rewrite rule and have
it apply to all existing code without modifying that code. This is arguably the
most powerful and most dangerous thing ace does, and it is the thing that no
mainstream language has chosen to adopt.

---

## Summary

| Question | ace | Elixir macros |
|---|---|---|
| AST-based? | Yes | Yes |
| Ambient / retroactive? | Yes | No — explicit call required |
| Rule expression | Declarative pattern + template | Imperative function |
| Hygienic? | No | Yes (by default) |
| Turing-complete? | No | Yes |
| Compile-time computation? | Constant folding only | Full language |
| Homoiconic? | No | Yes |
| Composes naturally? | By ordering only | Functions calling functions |
| Type-aware? | No | No (dynamic language) |
| Tooling | External preprocessor | Built into compiler |

ace and Elixir macros are the closest comparison in this series — both are
genuinely AST-rewriting systems. The gap between them is not in what they
transform but in how they express transformations (declarative vs imperative),
whether they require explicit invocation, and whether they provide hygiene.
Elixir macros are the more powerful and safer system. ace's ambient application
remains its most distinctive and uncopied idea.
