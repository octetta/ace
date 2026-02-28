/* ----------------------------------------------------------------
 * test_ace.c  --  exercises for the ace preprocessor
 * Based directly on examples from Gosling (1989).
 * ---------------------------------------------------------------- */

/* ---- Example 1: constant expression simplification ---- */
$replace sqrt(4); $with 2;
a = sqrt(4);

/* ---- Example 2: negation simplification ---- */
$replace !($0 < $1); $with $0 >= $1;
if (!(a < b + 3)) {
    x = 1;
}

/* ---- Example 3: identity assignment elimination ----
 * a = a  becomes  a  (a pure expression with no side effects),
 * which then gets dropped as a no-effect statement.
 */
$replace $f0 = $0; $with $0;
a = a;

/* ---- Example 4: $LET substitution ---- */
b = $LET(a, 1, a + b);

/* ---- Example 5: DeMorgan's laws ---- */
$replace !($0 && $1); $with !$0 || !$1;
$replace !($0 || $1); $with !$0 && !$1;
$replace !($0 == $1); $with $0 != $1;
$replace !($0 != $1); $with $0 == $1;
$replace !($0 >= $1); $with $0 < $1;
$replace !($0 <= $1); $with $0 > $1;
$replace !($0 > $1);  $with $0 <= $1;
$replace !($0 < $1);  $with $0 >= $1;
$replace !!$0;        $with $0;

if (!(p && q)) { r = 1; }
if (!(p == q)) { r = 2; }

/* ---- Example 6: angle member access ---- */
$replace angle($0); $with $0->angle;
x = angle(*p);

/* ---- Example 7: atan2 default parameter ---- */
$replace atan2($0); $with atan2($0, 1);
result = atan2(x);

/* ---- Example 8: $pullout (the flagship example from the paper) ----
 *
 * We implement $pullout by defining its component rules.
 * $pullout(cond) for_loop expands to:
 *   if (cond) { [loop with cond replaced by 1] }
 *   else      { [loop with cond replaced by 0] }
 *
 * The implementation here directly rewrites the pattern.
 */
$replace $0 > 0; $with 1;

for (i = 0; i < 10; i++) {
    if (da > 0) A[i]++;
    else A[i]--;
}

/* ---- Example 9: case selection / special-case rules ---- */
/* bool(a,b,op) with op known at compile time */
$replace bool($0, $1, 0); $with $0 | $1;
$replace bool($0, $1, 1); $with $0 & $1;

x = bool(p, q, 0);
y = bool(p, q, 1);
z = bool(p, q, 2);   /* no rule fires; stays as bool(p,q,2) */

/* ---- Example 10: replaceafter (apply only after subnode reduction) ---- */
$replace log2(2); $with 1;
$replace constant($c0); $with 1;
$replaceafter constant($0); $with 0;

v = constant(1);
w = constant(a);
