# ace and Racket Macros: Pattern-Rewriting with and without a Safety Net

Racket has two macro systems layered on top of each other, and the fact that
it needs two is itself revealing. `syntax-rules` is declarative and
pattern-based — close in spirit to ace. `syntax-case` is imperative and
Turing-complete — close in spirit to Elixir macros. That Racket provides both,
and that programmers reach for each in different situations, suggests that the
design space ace and Elixir macros occupy is genuinely two-dimensional, not a
spectrum with one right answer.

This document assumes no prior knowledge of Racket.

---

## A Minimal Racket Primer

Racket is a descendant of Scheme, which is a dialect of Lisp. The key property
for our purposes is that Racket code is made of *s-expressions* — nested lists
using parentheses:

```racket
(if (> x 0)
    (display "positive")
    (display "not positive"))
```

This is both the syntax and the data structure. `if` is the first element of a
list; `(> x 0)` is its second element (another list); and so on. Because code
and data have the same representation, manipulating code is just manipulating
lists — there is no separate AST format to learn. This property is called
*homoiconicity* and it is what makes Lisp-family macro systems unusually
natural.

Racket macros transform these s-expressions at compile time before evaluation.

---

## `syntax-rules`: The Declarative Layer

`syntax-rules` is Racket's pattern-matching macro system. A macro defined with
`syntax-rules` consists of one or more clauses, each with a pattern and a
template — exactly the structure of an ace rule.

Here is `unless` — an inverted `if` that does not exist as a built-in:

```racket
(define-syntax unless
  (syntax-rules ()
    [(unless condition body ...)
     (if (not condition) (begin body ...) (void))]))
```

Reading this:
- `(unless condition body ...)` is the pattern. `condition` and `body` are
  pattern variables (like ace's `$0`, `$1`). The `...` means "zero or more."
- `(if (not condition) (begin body ...) (void))` is the template. Pattern
  variables are substituted in.

Calling it:

```racket
(unless (> x 0)
  (display "not positive")
  (newline))
```

expands to:

```racket
(if (not (> x 0))
    (begin (display "not positive") (newline))
    (void))
```

The parallel with ace is direct:

```c
/* ace */
$replace unless($0, $1); $with if(!$0) $1;
```

Both are pattern-to-template substitutions. The differences are:
- Racket's pattern variables are named (`condition`, `body`); ace's are
  positional (`$0`, `$1`).
- Racket supports variadic patterns (`...`); ace does not.
- Racket `syntax-rules` is hygienic; ace is not.

---

## `syntax-case`: The Imperative Escape Hatch

`syntax-rules` breaks down when the transformation logic cannot be expressed
as a template. Suppose you want a macro that takes a list of variable-value
pairs and binds them all — like ace's `$LET`:

```racket
; We want: (let-pairs ([a 1] [b 2]) (+ a b))
; to expand to: (let ([a 1] [b 2]) (+ a b))
; but also validate that each pair has exactly two elements.
```

With `syntax-rules` you can do the expansion but not the validation. With
`syntax-case` you can do both, because you have access to full Racket:

```racket
(define-syntax let-pairs
  (lambda (stx)
    (syntax-case stx ()
      [(_ ([var val] ...) body ...)
       (for-each (lambda (v)
                   (unless (identifier? v)
                     (raise-syntax-error 'let-pairs "expected identifier" stx v)))
                 (syntax->list #'(var ...)))
       #'(let ([var val] ...) body ...)])))
```

The `lambda (stx)` receives the entire macro call as a syntax object. Inside,
you can run arbitrary Racket code — here, validating that each `var` is an
identifier — before returning the expansion with `#'(...)`.

This is exactly the Elixir macro model: a function that receives an AST,
does whatever computation it needs, and returns a new AST.

---

## The Three Systems Side by Side

A single example — simplifying `not (a and b)` to `not a or not b` — expressed
in all three systems.

### ace

```c
$replace !($0 && $1); $with !$0 || !$1;
```

One line. Fires on every matching expression in the file below it. No explicit
call required.

### Racket `syntax-rules`

```racket
(define-syntax demorgan-and
  (syntax-rules (and not)
    [(not (and a b))
     (or (not a) (not b))]))
```

A pattern-to-template rule, explicitly called:

```racket
(demorgan-and (not (and p q)))   ; => (or (not p) (not q))
```

### Racket `syntax-case`

```racket
(define-syntax demorgan-and
  (lambda (stx)
    (syntax-case stx (not and)
      [(_ (not (and a b)))
       #'(or (not a) (not b))])))
```

Functionally identical here, but now we could add validation or compute parts
of the expansion in Racket code.

The structural parallel between ace's `$replace` and Racket's `syntax-rules`
clause is clear. The difference is that ace's rule is ambient; the Racket macro
must be called at every site.

---

## Hygiene: Where Racket Solves ace's Hardest Problem

Recall the hygiene hazard in ace. This rule introduces a temporary variable:

```c
$replace swap($0, $1);
$with { int tmp = $0; $0 = $1; $1 = tmp; }

swap(a, tmp);   /* tmp clashes with the introduced tmp */
```

ace generates:

```c
{ int tmp = a; a = tmp; tmp = tmp; }  /* wrong */
```

The equivalent in Racket `syntax-rules`:

```racket
(define-syntax swap!
  (syntax-rules ()
    [(swap! a b)
     (let ([tmp a])
       (set! a b)
       (set! b tmp))]))

(swap! x tmp)   ; tmp in the call site, tmp introduced by the macro
```

Racket expands this correctly. The `tmp` introduced by `let` inside the macro
is automatically renamed to a fresh identifier that cannot clash with the `tmp`
passed by the caller. The expansion looks logically like:

```racket
(let ([tmp.42 x])     ; fresh name, invisible to caller
  (set! x tmp)        ; this tmp is the caller's tmp
  (set! tmp tmp.42))  ; correct
```

This is hygiene. The macro system guarantees that names introduced inside a
macro expansion do not capture names from the call site, and names from the call
site do not capture names introduced inside the macro. You get this for free
with `syntax-rules` and by default with `syntax-case`.

In ace, the programmer must manually avoid name collisions. For a preprocessor
intended for performance-critical inner loops — where temporary variables are
common — this is a genuine gap.

---

## Ambient Application: Where ace Has No Peer

The property Racket cannot replicate is ace's ambient application. In Racket,
every macro must be called explicitly. You cannot write a macro that silently
rewrites all expressions of a given shape in the module below it.

The closest Racket mechanism is `#%app` — a hook that intercepts every function
application in a module — but this only fires on calls, not on arbitrary
expression patterns.

For performance transformation of existing code, this matters enormously.
Consider a codebase with hundreds of identity assignments scattered through
legacy C:

```c
x = x;
arr[i] = arr[i];
p->field = p->field;
```

In ace, one rule eliminates all of them:

```c
$replace $f0 = $0; $with $0;
```

In Racket, you would need to either wrap every one in a macro call or write a
custom compiler pass — a much heavier intervention.

---

## Multiple Clauses: ace's Ordering vs Racket's Pattern Priority

Both systems support multiple patterns for the same macro/rule, applied in
order.

### ace

```c
$replace bool($0, $1, 0); $with $0 | $1;
$replace bool($0, $1, 1); $with $0 & $1;
/* no rule for other values — expression left unchanged */

x = bool(a, b, 0);   /* => a | b  */
y = bool(a, b, 1);   /* => a & b  */
z = bool(a, b, 2);   /* unchanged */
```

### Racket `syntax-rules`

```racket
(define-syntax bool-op
  (syntax-rules (or-op and-op)
    [(bool-op a b 0) (bitwise-ior a b)]
    [(bool-op a b 1) (bitwise-and a b)]))

(bool-op x y 0)   ; => (bitwise-ior x y)
(bool-op x y 1)   ; => (bitwise-and x y)
(bool-op x y 2)   ; compile error — no matching clause
```

The difference: in ace, an unmatched expression is left unchanged. In Racket,
an unmatched macro call is a compile error. Both are defensible choices —
Racket's is safer, ace's is more convenient for gradual transformation.

---

## `$replaceafter` and Racket's Two-Phase Expansion

ace's `$replaceafter` causes a rule to fire only *after* the node's children
have been reduced. This implements a form of bottom-up rewriting.

```c
$replace log2(2); $with 1;
$replace constant($c0); $with 1;   /* $c0 matches a numeric literal */
$replaceafter constant($0); $with 0;

constant(log2(2))   /* => 0, not 1 */
```

Without `$replaceafter`, the outer `constant(...)` rule fires before
`log2(2)` is reduced to `1`, so `log2(2)` is not yet a constant and the
third rule fires, giving `0` — which happens to be the right answer here but
for the wrong reason. With `$replaceafter`, the inner `log2(2)` is reduced
first, then `constant(1)` correctly matches the second rule and gives `1`.

Racket handles this through its macro expansion algorithm, which is
fundamentally outside-in: the outermost macro fires first, and its expansion
is then expanded again recursively. To get inside-out (bottom-up) behaviour,
a Racket macro explicitly recurse into sub-forms using `syntax-case` and
re-expand them before constructing the result. This is more explicit than
ace's `$replaceafter` but also more controllable.

---

## Comparing the Systems

| | ace | Racket `syntax-rules` | Racket `syntax-case` |
|---|---|---|---|
| **Style** | Declarative pattern + template | Declarative pattern + template | Imperative function over AST |
| **Ambient application** | Yes | No | No |
| **Explicit call required** | No | Yes | Yes |
| **Hygienic** | No | Yes | Yes (by default) |
| **Variadic patterns** | No | Yes (`...`) | Yes |
| **Turing-complete** | No | No | Yes |
| **Unmatched expression** | Left unchanged | Compile error | Compile error |
| **Homoiconic** | No | Yes | Yes |
| **Bottom-up rewriting** | `$replaceafter` | Manual recursion | Manual recursion |
| **Side-effect-free guard** | `$f0`…`$f9` | No built-in | Custom predicate |
| **Tooling** | External preprocessor | Built into compiler | Built into compiler |
| **Language** | C (with extensions) | Racket | Racket |

---

## What Each System Is Best At

**ace** is best at retroactive, ambient transformation of existing code. If you
have a body of C that was written without any thought of metaprogramming and you
want to apply systematic optimisations to it — hoist invariants, eliminate
identity operations, specialise on constant parameters — ace does this with
minimal syntax and no modification to the original source.

**Racket `syntax-rules`** is best at defining new syntax forms that feel like
built-in language constructs. `unless`, `when`, pattern-matching forms, DSLs
for configuration or data description — anything where you want clean syntax
and the transformation is expressible as a structural template. Hygiene makes
it safe. Explicit call sites make it comprehensible.

**Racket `syntax-case`** is best at macros that need to compute, validate, or
make decisions during expansion. Defining a struct macro that generates accessors
and mutators, a macro that reads a grammar file and generates a parser, or a
macro that type-checks its arguments — all of these need the full language, and
`syntax-case` provides it without sacrificing hygiene.

---

## The Design Space

Mapping the three systems on two axes:

```
                    Imperative
                        │
          syntax-case   │   Elixir macros
                        │
  ────────────────────────────────────────── Explicit ←──── Ambient
                        │
         syntax-rules   │   ace
                        │
                    Declarative
```

ace occupies the declarative-ambient quadrant alone. No mainstream language has
followed it there, almost certainly because ambient rewriting is genuinely
difficult to reason about — a rule defined at the top of a file silently
reshapes everything below it, and in a large codebase that can become very hard
to track.

Racket's contribution is showing that the declarative quadrant (syntax-rules)
and the imperative quadrant (syntax-case) can coexist in the same language,
with the programmer choosing based on what the transformation requires. The two
systems share hygiene, share the s-expression AST representation, and share the
explicit call-site model.

---

## What Racket Would Need to Become ace

To fully replicate ace's capabilities, a Racket-like system would need:

1. **Ambient rule application.** A way to declare that a pattern should be
   matched against every expression in the module, not just explicit call sites.
   This exists in some term-rewriting systems (Stratego, TXL) but not in any
   mainstream general-purpose language.

2. **Side-effect-free guards.** A built-in predicate for "this sub-expression
   has no side effects," available for use in pattern guards. Currently a macro
   author must implement this themselves, conservatively.

3. **`$replaceafter` semantics.** A simple annotation for "reduce children
   first, then attempt this pattern," without requiring the macro author to
   manually recursive-expand sub-forms.

None of these is architecturally incompatible with Racket's macro system. They
are missing because the use case — performance-directed transformation of
existing code — is not what Racket's macro system was designed for. Racket
macros are primarily for *syntactic abstraction* (defining new forms that feel
native), not for *semantic transformation* (reshaping code to run faster).

---

## Summary

Racket `syntax-rules` is the closest thing in a mainstream language to what ace
does: declarative, pattern-based, template-driven AST rewriting. The gap between
them is hygiene (Racket has it, ace does not), variadic patterns (Racket has
them, ace does not), and ambient application (ace has it, Racket does not).

`syntax-case` extends `syntax-rules` into the imperative territory that Elixir
macros occupy, while keeping hygiene and the same underlying model.

The fact that Racket needs both layers — and that programmers use both — is
evidence that ace's declarative style and Elixir's imperative style are not
competing answers to the same question but tools suited to different kinds of
transformation. ace's designers in 1989 chose the declarative approach because
they were solving an optimisation problem, not a syntactic abstraction problem.
That choice looks prescient: for the task they had in mind, a rule that says
`!($0 && $1)` → `!$0 || !$1` is more readable, more auditable, and easier to
compose than a macro function that manipulates AST nodes to achieve the same
end.

What ace lacked — and what no system since has combined with ambient rewriting
— is hygiene. That remains the open problem.
