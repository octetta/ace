# ace — A Syntax-Driven C Preprocessor

A reimplementation of James Gosling's **ace** preprocessor, originally described in:

> Gosling, James. "Ace: a syntax-driven C preprocessor." Sun Microsystems, Inc., July 1989.
> https://swtch.com/gosling89ace.pdf

Unlike `cpp`, which operates on characters, ace operates on **syntax trees**. You define rewrite rules as tree patterns; ace parses your C source, applies the rules, and pretty-prints the transformed result. This makes it possible to express source-level optimisations — loop-invariant code motion, dead-branch elimination, inline special-cases — in a way that stays readable and maintainable in the original source.

---

## Features

| Construct | Syntax | Effect |
|---|---|---|
| Rewrite rule | `$replace <pat>; $with <rep>;` | Replace all matching trees |
| Post-reduction rule | `$replaceafter <pat>; $with <rep>;` | Applied after children are reduced |
| Meta-variable | `$0`…`$9` | Matches any expression |
| Side-effect-free meta-var | `$f0`…`$f9` | Matches only side-effect-free expressions |
| Local substitution | `$LET(a, v, …, body)` | Substitutes `a→v` inside `body` |
| Prefix macro | `$defprefix($name, $fn);` | Defines a prefix-statement form |
| Trip-count annotation | `$trips(n) <stmt>` | Annotates expected loop iterations |
| Trade-off selector | `$tradeoff(time_code, space_code)` | Selects time- or space-efficient form |
| Probability annotation | `$P(expr)` | Annotates branch probability |

Rules are applied in definition order. Earlier rules take priority. Rules are applied to each node **before** and **after** its children are reduced, so substituted sub-trees get a second pass.

---

## Quick Start

### Build

```sh
gcc -Wall -Wextra -std=c99 -D_POSIX_C_SOURCE=200809L -g -o ace ace.c
```

No dependencies beyond a C99 compiler. Or use `make`:

```sh
make
```

Alternatively, if you have flex and bison installed:

```sh
make flex-bison
```

which runs:

```sh
bison -d -o ace_grammar.tab.c ace_grammar.y
flex  -o lex.ace_grammar.c   ace_grammar.l
gcc -Wall -Wextra -std=c99 -D_POSIX_C_SOURCE=200809L -g \
    -o ace ace_grammar.tab.c lex.ace_grammar.c -lfl
```

### Run

```sh
./ace input.c            # transformed output to stdout
./ace input.c > output.c
```

---

## Examples

All examples are taken directly from the 1989 paper.

### Constant rule

```c
$replace sqrt(4); $with 2;

a = sqrt(4);
```

Output:

```c
a = 2;
```

---

### Meta-variables — negation simplification

`$0` and `$1` match any sub-expression and are captured for use in the replacement.

```c
$replace !($0 < $1); $with $0 >= $1;

if (!(a < b + 3)) { x = 1; }
```

Output:

```c
if (a >= b + 3)
    {
        x = 1;
    }
```

Parentheses around `b + 3` are omitted because ace tracks operator precedence and inserts them only when needed.

---

### Side-effect-free meta-variables

`$f0` only binds to expressions that have no side effects.

```c
$replace $f0 = $0; $with $0;

a = a;       /* replaced with 'a', then dropped as a no-effect statement */
*p++ = *p++; /* not replaced — p++ has a side effect */
```

Output: the first assignment disappears entirely; the second is left alone.

---

### `$LET` — local substitution

```c
b = $LET(a, 1, a + b);
```

Output:

```c
b = 1 + b;
```

---

### DeMorgan's laws

```c
$replace !($0 && $1); $with !$0 || !$1;
$replace !($0 || $1); $with !$0 && !$1;
$replace !($0 == $1); $with $0 != $1;
$replace !($0 != $1); $with $0 == $1;
$replace !!$0;        $with $0;

if (!(p && q)) { r = 1; }
if (!(p == q)) { r = 2; }
```

Output:

```c
if (!p || !q) { r = 1; }
if (p != q)   { r = 2; }
```

---

### Member access sugar

```c
$replace angle($0); $with $0->angle;

x = angle(*p);
```

Output:

```c
x = (*p)->angle;
```

The parentheses around `*p` are inserted automatically based on operator precedence — no need to litter the rule with them.

---

### Loop-invariant code motion — `$pullout`

The flagship example from the paper. The test `da > 0` is loop-invariant.

```c
$replace $0 > 0; $with 1;

for (i = 0; i < 10; i++) {
    if (da > 0) A[i]++;
    else        A[i]--;
}
```

With the assumption that `da > 0` is true, the condition becomes the constant `1`, the dead `else` branch is eliminated, and the result is:

```c
for (i = 0; i < 10; i++)
    {
        A[i]++;
    }
```

In the original ace, `$pullout` generates both branches (true and false) and applies `$assume` to each. This implementation demonstrates the constant-folding half of that mechanism directly.

---

### Compile-time dispatch on a constant parameter

```c
$replace bool($0, $1, 0); $with $0 | $1;
$replace bool($0, $1, 1); $with $0 & $1;

x = bool(p, q, 0);   /* => p | q  */
y = bool(p, q, 1);   /* => p & q  */
z = bool(p, q, 2);   /* no rule matches — left as bool(p, q, 2) */
```

---

### `$replaceafter` — post-reduction rules

Rules defined with `$replaceafter` fire only after their arguments have been fully reduced. This allows rules that depend on what a sub-expression evaluates *to*, not what it looks like originally.

```c
$replace log2(2); $with 1;
$replace constant($c0); $with 1;
$replaceafter constant($0); $with 0;

v = constant(1);        /* => 1  (matches $c0 before reduction) */
w = constant(a);        /* => 0  (no match before; $replaceafter fires after) */
```

---

## File Overview

```
ace.c               Standalone implementation — hand-rolled lexer and
                    recursive-descent parser. Builds with gcc alone.

ace_grammar.l       Flex lexer source for the flex+bison build path.
                    Named ace_grammar.l (not ace.l) to prevent GNU
                    make's built-in implicit rules from treating it
                    as a source for ace.c.

ace_grammar.y       Bison LALR(1) grammar for the flex+bison build
                    path. Same naming rationale as above.

test_ace.c          Test file exercising all examples from the paper.
test_ace.golden     Expected output — used by make regression.
Makefile            Builds ace; see make targets below.
```

### Make targets

| Target | Effect |
|---|---|
| `make` | Compile `ace.c` with gcc — no other tools needed |
| `make flex-bison` | Build via bison + flex + gcc |
| `make test` | Build and run `test_ace.c`, report pass/fail per rule |
| `make golden` | Regenerate `test_ace.golden` from current output |
| `make regression` | Diff current output against `test_ace.golden` |
| `make clean` | Remove `ace` and generated flex/bison files |

---

## Limitations

This is a faithful reimplementation of the *language* described in the paper. A few things are simplified or not yet implemented:

- **`$tradeoff` time/space analysis** — the selector is present in the grammar; the paper's cost model (instruction counts, branch probabilities, `-pthresh`/`-mingain` flags) is not implemented. The time-efficient branch is always chosen.
- **`$assume`** — the full `$assume` / `$pullout` expansion pipeline from the paper (which generates both branches and applies assumptions to each) is not yet a built-in. It can be partially replicated with user-defined `$replace` rules as shown in the examples above.
- **C grammar coverage** — the parser handles the common subset of C well enough for the paper's examples. It does not handle all of C99/C11 (e.g. designated initialisers, complex declarators, `_Generic`).
- **Type-aware matching** — ace as described is not type-aware. Neither is this implementation.

---

## License

MIT License

Copyright (c) 2026

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

---

## Attribution

The ace preprocessor was designed by **James Gosling** at Sun Microsystems in 1989. This reimplementation is based solely on the published paper and does not incorporate any original Sun Microsystems source code.

Gosling, J. (1989). *Ace: a syntax-driven C preprocessor.* Sun Microsystems, Inc.
Retrieved from https://swtch.com/gosling89ace.pdf

The paper is archived and linked by **Russ Cox** at https://swtch.com/.
