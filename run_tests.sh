#!/bin/bash
# run_tests.sh — ace smoke tests
# Run from the directory containing the ace binary: bash run_tests.sh

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
        diff <(echo "$expected") <(echo "$actual") | sed 's/^/      /'
        FAIL=$((FAIL + 1))
    fi
}

check_exit() {
    local desc="$1"
    local input="$2"
    local want_fail="$3"   # "fail" or "pass"
    printf '%s' "$input" | $ACE > /dev/null 2>&1
    local code=$?
    if [ "$want_fail" = "fail" ] && [ $code -ne 0 ]; then
        echo "PASS  $desc"
        PASS=$((PASS + 1))
    elif [ "$want_fail" = "pass" ] && [ $code -eq 0 ]; then
        echo "PASS  $desc"
        PASS=$((PASS + 1))
    else
        echo "FAIL  $desc (exit code $code)"
        FAIL=$((FAIL + 1))
    fi
}

echo "=== ace shell tests ==="
echo ""

# ---- basic constant rule ----
check "sqrt(4) -> 2" \
'$replace sqrt(4); $with 2;
x = sqrt(4);' \
'x = 2;'

# ---- meta-variables ----
check "negation rewrite with single statement" \
'$replace !($0 < $1); $with $0 >= $1;
if (!(a < b)) x = 1;' \
'if (a >= b)
    x = 1;'

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
check "constant folding add" \
'x = 2 + 3;' \
'x = 5;'

check "constant folding mixed" \
'x = 2 * 3 + 1;' \
'x = 7;'

# ---- dead branch elimination ----
check "if(1) true branch kept" \
'if (1) x = 1;
else x = 2;' \
'x = 1;'

check "if(0) false branch kept" \
'if (0) x = 1;
else x = 2;' \
'x = 2;'

check "if(1) no else" \
'if (1) x = 1;' \
'x = 1;'

# ---- member access sugar ----
check "member access rule" \
'$replace angle($0); $with $0->angle;
x = angle(*p);' \
'x = *p->angle;'

# ---- default parameter ----
check "default parameter" \
'$replace atan2($0); $with atan2($0, 1);
r = atan2(x);' \
'r = atan2(x, 1);'

# ---- DeMorgan ----
check "DeMorgan and" \
'$replace !($0 && $1); $with !$0 || !$1;
if (!(p && q)) r = 1;' \
'if (!p || !q)
    r = 1;'

check "DeMorgan eq" \
'$replace !($0 == $1); $with $0 != $1;
if (!(a == b)) r = 1;' \
'if (a != b)
    r = 1;'

# ---- compile-time dispatch ----
check "bool op 0 -> bitwise or" \
'$replace bool($0, $1, 0); $with $0 | $1;
$replace bool($0, $1, 1); $with $0 & $1;
x = bool(a, b, 0);' \
'x = a | b;'

check "bool op 1 -> bitwise and" \
'$replace bool($0, $1, 0); $with $0 | $1;
$replace bool($0, $1, 1); $with $0 & $1;
x = bool(a, b, 1);' \
'x = a & b;'

check "bool op unknown -> unchanged" \
'$replace bool($0, $1, 0); $with $0 | $1;
$replace bool($0, $1, 1); $with $0 & $1;
x = bool(a, b, 2);' \
'x = bool(a, b, 2);'

# ---- loop invariant ----
check "loop invariant pruned" \
'$replace da > 0; $with 1;
for (i = 0; i < 10; i++) {
    if (da > 0) A[i]++;
    else A[i]--;
}' \
'for (i = 0; i < 10; i++)
    {
        A[i]++;
    }'

# ---- replaceafter ----
check "replaceafter: constant(a) -> 0" \
'$replaceafter constant($0); $with 0;
v = constant(a);' \
'v = 0;'

check "replaceafter: constant(1) -> 0 (pre rule shadowed by after rule)" \
'$replace constant($c0); $with 1;
$replaceafter constant($0); $with 0;
v = constant(1);' \
'v = 0;'

# ---- exit codes ----
check_exit "valid input exits 0" \
'x = 1 + 2;' \
"pass"

check_exit "parse error exits non-zero" \
'if (;' \
"fail"

echo ""
echo "$PASS passed, $FAIL failed"
[ $FAIL -eq 0 ]
