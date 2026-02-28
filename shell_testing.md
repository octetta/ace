# Testing ace from the Shell

ace reads from a file or from stdin, and writes to stdout. This makes it
natural to test from the shell without any test framework.

---

## Basic Usage

### Heredoc (most readable for multi-line input)

```sh
./ace << 'EOF'
$replace sqrt(4); $with 2;
x = sqrt(4);
EOF
```

Output:
```
x = 2;
```

The single quotes on `'EOF'` are important — they prevent the shell from
expanding `$replace` and `$0` as shell variables.

### Pipe from echo

```sh
echo '$replace sqrt(4); $with 2;
x = sqrt(4);' | ./ace
```

### From a file

```sh
./ace my_rules.c
```

### Rule file + source file combined

ace processes a single input stream, so combine them with `cat`:

```sh
cat rules.ace source.c | ./ace
```

---

## One-Liner Tests

Quick checks at the prompt. Always quote with single quotes to protect `$`:

```sh
# Does a rule fire?
echo '$replace foo($0); $with $0->foo;
x = foo(p);' | ./ace

# Does a rule correctly not fire on a side-effecting expression?
echo '$replace $f0 = $0; $with $0;
a = a;
*p++ = *p++;' | ./ace

# Is constant folding working?
echo 'x = 1 + 2 * 3;' | ./ace
```

---

## Comparing Expected Output

### Inline with `diff`

```sh
./ace << 'EOF' | diff - <(echo 'x = 2;')
$replace sqrt(4); $with 2;
x = sqrt(4);
EOF
echo "exit: $?"
```

`diff` exits 0 on match, 1 on mismatch — so this works as a pass/fail test.

### Wrapping it as a function

Put this in your `.bashrc` or at the top of a test script:

```sh
ace_test() {
    local desc="$1"
    local input="$2"
    local expected="$3"
    local actual
    actual=$(echo "$input" | ./ace 2>&1)
    if [ "$actual" = "$expected" ]; then
        echo "PASS  $desc"
    else
        echo "FAIL  $desc"
        echo "  expected: $expected"
        echo "  actual:   $actual"
    fi
}
```

Then:

```sh
ace_test "constant rule" \
    '$replace sqrt(4); $with 2;
x = sqrt(4);' \
    'x = 2;'

ace_test "demorgan" \
    '$replace !($0 && $1); $with !$0 || !$1;
if (!(p && q)) r = 1;' \
    'if (!p || !q)
    {
        r = 1;
    }'
```

---

## A Self-Contained Test Script

Save as `run_tests.sh`, run with `bash run_tests.sh`:

```sh
#!/bin/bash
# run_tests.sh — quick ace smoke tests

ACE=./ace
PASS=0
FAIL=0

check() {
    local desc="$1"
    local input="$2"
    local expected="$3"
    local actual
    actual=$(printf '%s' "$input" | $ACE 2>&1)
    if [ "$actual" = "$expected" ]; then
        echo "PASS  $desc"
        PASS=$((PASS + 1))
    else
        echo "FAIL  $desc"
        echo "      expected: $(echo "$expected" | head -1)..."
        echo "      actual:   $(echo "$actual"   | head -1)..."
        FAIL=$((FAIL + 1))
    fi
}

# ---- constant rule ----
check "sqrt(4) -> 2" \
'$replace sqrt(4); $with 2;
x = sqrt(4);' \
'x = 2;'

# ---- meta-variable ----
check "negation rewrite" \
'$replace !($0 < $1); $with $0 >= $1;
if (!(a < b)) x = 1;' \
'if (a >= b)
    {
        x = 1;
    }'

# ---- side-effect-free guard ----
check "identity assignment eliminated" \
'$replace $f0 = $0; $with $0;
a = a;' \
''

check "side-effecting assignment NOT eliminated" \
'$replace $f0 = $0; $with $0;
*p++ = *p++;' \
'*p++ = *p++;'

# ---- LET ----
check "LET substitution" \
'x = $LET(a, 1, a + b);' \
'x = 1 + b;'

# ---- constant folding ----
check "constant folding" \
'x = 2 + 3;' \
'x = 5;'

# ---- dead branch elimination ----
check "if(1) pruned" \
'if (1) x = 1;
else x = 2;' \
'x = 1;'

check "if(0) pruned" \
'if (0) x = 1;
else x = 2;' \
'x = 2;'

# ---- member access sugar ----
check "member access rule" \
'$replace angle($0); $with $0->angle;
x = angle(*p);' \
'x = *p->angle;'

# ---- replaceafter ----
check "replaceafter fires post-reduction" \
'$replace log2(2); $with 1;
$replace constant($c0); $with 1;
$replaceafter constant($0); $with 0;
v = constant(log2(2));' \
'v = 1;'

# ---- rule does not fire on wrong literal ----
check "no match leaves expression unchanged" \
'$replace bool($0, $1, 0); $with $0 | $1;
$replace bool($0, $1, 1); $with $0 & $1;
z = bool(p, q, 2);' \
'z = bool(p, q, 2);'

# ---- parse error exits non-zero ----
result=$(echo 'if (;' | $ACE 2>/dev/null; echo $?)
check "parse error exits non-zero" \
'if (;' \
''
# check exit code separately
if echo 'if (;' | $ACE > /dev/null 2>&1; then
    echo "FAIL  parse error should exit non-zero"
    FAIL=$((FAIL + 1))
else
    echo "PASS  parse error exits non-zero"
    PASS=$((PASS + 1))
fi

echo ""
echo "$PASS passed, $FAIL failed"
[ $FAIL -eq 0 ]   # exit 0 only if all passed
```

---

## Running Under make

Add to your Makefile:

```makefile
.PHONY: shell-test
shell-test: all
	bash run_tests.sh
```

---

## Useful Shell Patterns

### See the full expansion of a complex input

```sh
./ace my_file.c | less
```

### Diff original against transformed

```sh
diff <(cat my_file.c) <(./ace my_file.c)
```

### Check that a rule fires at least once

```sh
./ace my_file.c | grep -q 'expected_output' && echo PASS || echo FAIL
```

### Count how many times a pattern was eliminated

```sh
# How many statements did ace drop?
wc -l < my_file.c
./ace my_file.c | wc -l
```

### Watch a rule fire interactively while editing

If you have `entr` installed:

```sh
echo my_file.c | entr ./ace my_file.c
```

Reruns ace every time you save the file.
