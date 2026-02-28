# Makefile for ace — a syntax-driven C preprocessor
# Based on Gosling (1989), https://swtch.com/gosling89ace.pdf

CC     = gcc
CFLAGS = -Wall -Wextra -std=c99 -D_POSIX_C_SOURCE=200809L -g

TEST   = test_ace.c
OUT    = test_ace.out
GOLDEN = test_ace.golden

# Kill all built-in implicit rules so make never searches parent
# directories for ace.l or ace.y to satisfy dependencies.
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

# ------------------------------------------------------------------
# Default: standalone build — ace.c only, no flex/bison needed
# ------------------------------------------------------------------

.PHONY: all
all:
	$(CC) $(CFLAGS) -o ace ace.c

# ------------------------------------------------------------------
# flex + bison build (uses ace_grammar.l / ace_grammar.y)
# ------------------------------------------------------------------

.PHONY: flex-bison
flex-bison: ace_grammar.tab.c lex.ace_grammar.c
	$(CC) $(CFLAGS) -o ace ace_grammar.tab.c lex.ace_grammar.c -lfl

ace_grammar.tab.c ace_grammar.tab.h: ace_grammar.y
	bison -d -o ace_grammar.tab.c ace_grammar.y

lex.ace_grammar.c: ace_grammar.l ace_grammar.tab.h
	flex -o lex.ace_grammar.c ace_grammar.l

# ------------------------------------------------------------------
# Test
# ------------------------------------------------------------------

.PHONY: test
test: all
	@echo ""
	@echo "=== ace: running $(TEST) ==="
	@echo ""
	./ace $(TEST) | tee $(OUT)
	@echo ""
	@echo "=== Transformation check ==="
	@echo ""
	@grep -q 'a = 2'         $(OUT) && echo "  PASS  sqrt(4)          ->  2"                  || echo "  FAIL  sqrt(4)          ->  2"
	@grep -q '>='            $(OUT) && echo "  PASS  !(a<b+3)         ->  a>=b+3"              || echo "  FAIL  !(a<b+3)         ->  a>=b+3"
	@grep -q 'a = a'         $(OUT) && echo "  FAIL  a=a              ->  (eliminated)"        || echo "  PASS  a=a              ->  (eliminated)"
	@grep -q '1 + b'         $(OUT) && echo "  PASS  \$$LET(a,1,a+b)    ->  1+b"               || echo "  FAIL  \$$LET(a,1,a+b)    ->  1+b"
	@grep -q '!p || !q'      $(OUT) && echo "  PASS  !(p&&q)          ->  !p||!q"              || echo "  FAIL  !(p&&q)          ->  !p||!q"
	@grep -q 'p != q'        $(OUT) && echo "  PASS  !(p==q)          ->  p!=q"                || echo "  FAIL  !(p==q)          ->  p!=q"
	@grep -q '\*p->angle'    $(OUT) && echo "  PASS  angle(*p)        ->  (*p)->angle"         || echo "  FAIL  angle(*p)        ->  (*p)->angle"
	@grep -q 'atan2(x, 1)'   $(OUT) && echo "  PASS  atan2(x)         ->  atan2(x,1)"         || echo "  FAIL  atan2(x)         ->  atan2(x,1)"
	@grep -q 'A\[i\]++'      $(OUT) && echo "  PASS  loop pullout     ->  dead branch removed" || echo "  FAIL  loop pullout     ->  dead branch removed"
	@grep -q 'p | q'         $(OUT) && echo "  PASS  bool(p,q,0)      ->  p|q"                 || echo "  FAIL  bool(p,q,0)      ->  p|q"
	@grep -q 'p & q'         $(OUT) && echo "  PASS  bool(p,q,1)      ->  p&q"                 || echo "  FAIL  bool(p,q,1)      ->  p&q"
	@grep -q 'bool(p, q, 2)' $(OUT) && echo "  PASS  bool(p,q,2)      ->  (unchanged)"        || echo "  FAIL  bool(p,q,2)      ->  (unchanged)"
	@echo ""

# ------------------------------------------------------------------
# Golden reference
# ------------------------------------------------------------------

.PHONY: golden
golden: all
	./ace $(TEST) > $(GOLDEN)
	@echo "Golden reference written to $(GOLDEN)"
	@wc -l $(GOLDEN)

# ------------------------------------------------------------------
# Regression
# ------------------------------------------------------------------

.PHONY: regression
regression: all $(GOLDEN)
	@echo "=== Regression test against $(GOLDEN) ==="
	./ace $(TEST) > $(OUT)
	diff -u $(GOLDEN) $(OUT) \
		&& echo "PASS — output matches golden" \
		|| (echo "FAIL — see diff above"; exit 1)


# ------------------------------------------------------------------
# Shell test suite (run_tests.sh)
# ------------------------------------------------------------------

.PHONY: shell-test
shell-test: all
	bash run_tests.sh
# ------------------------------------------------------------------
# Clean
# ------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f ace $(OUT) ace_grammar.tab.c ace_grammar.tab.h lex.ace_grammar.c
