%{
/*
 * ace_grammar.y  --  bison grammar for the ace C preprocessor
 *
 * Named ace_grammar.y (not ace.y) to avoid GNU make's built-in %.c: %.y
 * implicit rule which would try to run yacc and overwrite ace.c.
 *
 * Build:
 *   bison -d -o ace_grammar.tab.c ace_grammar.y
 *   flex  -o lex.ace_grammar.c   ace_grammar.l
 *   gcc -Wall -Wextra -std=c99 -D_POSIX_C_SOURCE=200809L \
 *       -o ace ace_grammar.tab.c lex.ace_grammar.c -lfl
 */

#define _POSIX_C_SOURCE 200809L
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* =========================================================
 * AST
 * ========================================================= */

typedef enum {
    N_NUM, N_STR, N_IDENT,
    N_METAVAR, N_METAVAR_FREE,
    N_BINOP, N_UNARY_PRE, N_UNARY_POST,
    N_TERNARY, N_CALL, N_CAST,
    N_INDEX, N_MEMBER_DOT, N_MEMBER_ARROW,
    N_SIZEOF_EXPR, N_SIZEOF_TYPE,
    N_EXPR_STMT, N_BLOCK,
    N_IF, N_FOR, N_WHILE, N_DO,
    N_RETURN, N_BREAK, N_CONTINUE, N_GOTO,
    N_SWITCH, N_CASE, N_DEFAULT, N_LABEL,
    N_DECL, N_EMPTY,
    N_RULE, N_DEFPREFIX, N_TRIPS, N_TRADEOFF, N_LET,
    N_LIST,
} NodeKind;

#define MAX_CHILDREN 64
typedef struct Node {
    NodeKind    kind;
    char       *sval;
    int         ival;
    struct Node *children[MAX_CHILDREN];
    int         nchildren;
} Node;

static Node *node_new(NodeKind k) {
    Node *n = calloc(1, sizeof *n); n->kind = k; return n;
}
static void node_add(Node *p, Node *c) {
    assert(p->nchildren < MAX_CHILDREN);
    p->children[p->nchildren++] = c;
}
static Node *make_leaf(NodeKind k, const char *s) {
    Node *n = node_new(k); n->sval = strdup(s); return n;
}
static Node *make_ival(NodeKind k, int v) {
    Node *n = node_new(k); n->ival = v; return n;
}
static Node *make_binop(const char *op, Node *l, Node *r) {
    Node *n = node_new(N_BINOP);
    n->sval = strdup(op);
    node_add(n,l); node_add(n,r); return n;
}
static Node *make_unary(NodeKind k, const char *op, Node *e) {
    Node *n = node_new(k); n->sval = strdup(op); node_add(n,e); return n;
}
static Node *make_list(void) { return node_new(N_LIST); }

/* =========================================================
 * Rule table
 * ========================================================= */

#define MAX_RULES 256
typedef struct { Node *pattern; Node *replacement; int after; } Rule;
static Rule rules[MAX_RULES];
static int  nrules = 0;

static void add_rule(Node *p, Node *r, int a) {
    if (nrules >= MAX_RULES) { fprintf(stderr,"ace: too many rules\n"); return; }
    rules[nrules].pattern     = p;
    rules[nrules].replacement = r;
    rules[nrules].after       = a;
    nrules++;
}

/* =========================================================
 * Prefix-macro table
 * ========================================================= */

#define MAX_PREFIXES 64
typedef struct { char *name; char *func; } Prefix;
static Prefix prefixes[MAX_PREFIXES];
static int nprefixes = 0;

static void add_prefix(const char *name, const char *func) {
    if (nprefixes >= MAX_PREFIXES) return;
    prefixes[nprefixes].name = strdup(name);
    prefixes[nprefixes].func = strdup(func);
    nprefixes++;
}

/* =========================================================
 * Pattern matching
 * ========================================================= */

#define MAX_META 10
static Node *bindings[MAX_META];

static int is_side_effect_free(Node *n) {
    if (!n) return 1;
    switch (n->kind) {
    case N_NUM: case N_STR: case N_IDENT: return 1;
    case N_BINOP: {
        const char *op = n->sval;
        int last = op[strlen(op)-1];
        if (last=='=' && strcmp(op,"==") && strcmp(op,"!=") &&
            strcmp(op,"<=") && strcmp(op,">=")) return 0;
        return is_side_effect_free(n->children[0]) &&
               is_side_effect_free(n->children[1]);
    }
    case N_UNARY_PRE: case N_UNARY_POST:
        if (!strcmp(n->sval,"++") || !strcmp(n->sval,"--")) return 0;
        return is_side_effect_free(n->children[0]);
    case N_CALL: return 0;
    default:
        for (int i=0;i<n->nchildren;i++)
            if (!is_side_effect_free(n->children[i])) return 0;
        return 1;
    }
}

static int node_equal(Node *a, Node *b);

static int match(Node *pat, Node *n) {
    if (!pat && !n) return 1;
    if (!pat || !n) return 0;
    if (pat->kind == N_METAVAR) {
        int i = pat->ival;
        if (bindings[i]) return node_equal(bindings[i], n);
        bindings[i] = n; return 1;
    }
    if (pat->kind == N_METAVAR_FREE) {
        if (!is_side_effect_free(n)) return 0;
        int i = pat->ival;
        if (bindings[i]) return node_equal(bindings[i], n);
        bindings[i] = n; return 1;
    }
    if (pat->kind != n->kind) return 0;
    if (pat->sval && n->sval) { if (strcmp(pat->sval,n->sval)) return 0; }
    else if (pat->sval || n->sval) return 0;
    if (pat->ival != n->ival) return 0;
    if (pat->nchildren != n->nchildren) return 0;
    for (int i=0;i<pat->nchildren;i++)
        if (!match(pat->children[i], n->children[i])) return 0;
    return 1;
}

static int node_equal(Node *a, Node *b) {
    Node *save[MAX_META];
    memcpy(save, bindings, sizeof bindings);
    memset(bindings, 0, sizeof bindings);
    int r = match(a, b);
    memcpy(bindings, save, sizeof bindings);
    return r;
}

static Node *subst(Node *n) {
    if (!n) return NULL;
    if (n->kind == N_METAVAR || n->kind == N_METAVAR_FREE) {
        Node *b = bindings[n->ival];
        return b ? b : n;
    }
    Node *c = node_new(n->kind);
    c->sval = n->sval ? strdup(n->sval) : NULL;
    c->ival = n->ival;
    for (int i=0;i<n->nchildren;i++) node_add(c, subst(n->children[i]));
    return c;
}

/* =========================================================
 * Rewriter
 * ========================================================= */

static Node *rewrite(Node *n);

static Node *apply_rules(Node *n, int after) {
    for (int i=0;i<nrules;i++) {
        if (rules[i].after != after) continue;
        memset(bindings, 0, sizeof bindings);
        if (match(rules[i].pattern, n)) {
            Node *r = subst(rules[i].replacement);
            return rewrite(r);
        }
    }
    return n;
}

static Node *cfold(Node *n) {
    if (n->kind != N_BINOP) return n;
    Node *l = n->children[0], *r = n->children[1];
    if (!l || !r || l->kind != N_NUM || r->kind != N_NUM) return n;
    long lv = strtol(l->sval,NULL,0), rv = strtol(r->sval,NULL,0);
    long res; int ok = 1;
    const char *op = n->sval;
    if      (!strcmp(op,"+"))  res = lv+rv;
    else if (!strcmp(op,"-"))  res = lv-rv;
    else if (!strcmp(op,"*"))  res = lv*rv;
    else if (!strcmp(op,"/"))  res = rv ? lv/rv : (ok=0,0);
    else if (!strcmp(op,"&"))  res = lv&rv;
    else if (!strcmp(op,"|"))  res = lv|rv;
    else if (!strcmp(op,"^"))  res = lv^rv;
    else if (!strcmp(op,"<<")) res = lv<<rv;
    else if (!strcmp(op,">>")) res = lv>>rv;
    else if (!strcmp(op,"==")) res = lv==rv;
    else if (!strcmp(op,"!=")) res = lv!=rv;
    else if (!strcmp(op,"<"))  res = lv<rv;
    else if (!strcmp(op,">"))  res = lv>rv;
    else if (!strcmp(op,"<=")) res = lv<=rv;
    else if (!strcmp(op,">=")) res = lv>=rv;
    else if (!strcmp(op,"&&")) res = lv&&rv;
    else if (!strcmp(op,"||")) res = lv||rv;
    else ok=0, res=0;
    if (!ok) return n;
    char tmp[32]; snprintf(tmp,sizeof tmp,"%ld",res);
    return make_leaf(N_NUM, tmp);
}

static Node *expand_let(Node *args) {
    int n = args->nchildren;
    if (n == 0) return make_leaf(N_NUM,"0");
    Node *body = args->children[n-1];
    int saved = nrules;
    for (int i=0; i+1 < n-1; i+=2)
        add_rule(args->children[i], args->children[i+1], 0);
    Node *result = rewrite(body);
    nrules = saved;
    return result;
}

static Node *rewrite(Node *n) {
    if (!n) return NULL;
    if (n->kind == N_RULE) {
        add_rule(n->children[0], n->children[1], n->ival);
        return make_leaf(N_EMPTY,"");
    }
    if (n->kind == N_DEFPREFIX) {
        add_prefix(n->sval, n->children[0]->sval);
        return make_leaf(N_EMPTY,"");
    }
    if (n->kind == N_LET)
        return expand_let(n->children[0]);
    n = apply_rules(n, 0);
    for (int i=0;i<n->nchildren;i++)
        n->children[i] = rewrite(n->children[i]);
    n = apply_rules(n, 1);
    n = cfold(n);
    if (n->kind == N_IF) {
        Node *cond = n->children[0];
        if (cond && cond->kind == N_NUM) {
            long v = strtol(cond->sval,NULL,0);
            return rewrite(v ? n->children[1]
                             : (n->nchildren>=3 ? n->children[2]
                                                : make_leaf(N_EMPTY,"")));
        }
    }
    if (n->kind == N_EXPR_STMT) {
        Node *e = n->children[0];
        if (e && is_side_effect_free(e) && e->kind != N_CALL)
            return make_leaf(N_EMPTY,"");
    }
    return n;
}

/* =========================================================
 * Pretty-printer
 * ========================================================= */

static int indent_level = 0;
static void pi(void) { for(int i=0;i<indent_level;i++) printf("    "); }

static int op_prec(const char *op) {
    if (!op) return 99;
    if (!strcmp(op,","))  return 1;
    const char *aops[] = {"=","+=","-=","*=","/=","%=","&=","|=","^=","<<=",">>=",NULL};
    for(int i=0;aops[i];i++) if(!strcmp(op,aops[i])) return 2;
    if (!strcmp(op,"||")) return 4;
    if (!strcmp(op,"&&")) return 5;
    if (!strcmp(op,"|"))  return 6;
    if (!strcmp(op,"^"))  return 7;
    if (!strcmp(op,"&"))  return 8;
    if (!strcmp(op,"==") || !strcmp(op,"!=")) return 9;
    if (!strcmp(op,"<")  || !strcmp(op,">") ||
        !strcmp(op,"<=") || !strcmp(op,">=")) return 10;
    if (!strcmp(op,"<<") || !strcmp(op,">>")) return 11;
    if (!strcmp(op,"+")  || !strcmp(op,"-")) return 12;
    if (!strcmp(op,"*")  || !strcmp(op,"/") || !strcmp(op,"%")) return 13;
    return 15;
}

static void print_expr(Node *n, int pp);

static void print_expr(Node *n, int pp) {
    if (!n) return;
    switch (n->kind) {
    case N_NUM: case N_STR: case N_IDENT: printf("%s", n->sval); break;
    case N_METAVAR:      printf("$%d", n->ival); break;
    case N_METAVAR_FREE: printf("$f%d", n->ival); break;
    case N_EMPTY: break;
    case N_BINOP: {
        int p = op_prec(n->sval);
        if (p < pp) printf("(");
        print_expr(n->children[0], p);
        printf(" %s ", n->sval);
        print_expr(n->children[1], p+1);
        if (p < pp) printf(")");
        break;
    }
    case N_UNARY_PRE:
        if (!strcmp(n->sval,"sizeof")) {
            printf("sizeof("); print_expr(n->children[0],1); printf(")");
        } else { printf("%s", n->sval); print_expr(n->children[0],14); }
        break;
    case N_UNARY_POST: print_expr(n->children[0],14); printf("%s",n->sval); break;
    case N_TERNARY:
        if (pp > 3) printf("(");
        print_expr(n->children[0],4); printf(" ? ");
        print_expr(n->children[1],3); printf(" : ");
        print_expr(n->children[2],3);
        if (pp > 3) printf(")");
        break;
    case N_CALL:
        printf("%s(", n->sval ? n->sval : "?");
        if (n->nchildren > 0) {
            Node *args = n->children[0];
            for (int i=0;i<args->nchildren;i++) {
                if (i) printf(", ");
                print_expr(args->children[i], 2);
            }
        }
        printf(")");
        break;
    case N_CAST: printf("(%s)",n->sval); print_expr(n->children[0],14); break;
    case N_INDEX:
        print_expr(n->children[0],15); printf("[");
        print_expr(n->children[1],1);  printf("]");
        break;
    case N_MEMBER_DOT:   print_expr(n->children[0],15); printf(".%s",n->sval);  break;
    case N_MEMBER_ARROW: print_expr(n->children[0],15); printf("->%s",n->sval); break;
    case N_SIZEOF_TYPE:  printf("sizeof(%s)",n->sval); break;
    default: printf("/*?*/"); break;
    }
}

static void print_stmt(Node *n) {
    if (!n) return;
    switch (n->kind) {
    case N_EMPTY: break;
    case N_DECL:  pi(); printf("%s\n", n->sval); break;
    case N_EXPR_STMT: pi(); print_expr(n->children[0],1); printf(";\n"); break;
    case N_BLOCK:
        pi(); printf("{\n"); indent_level++;
        for (int i=0;i<n->nchildren;i++) print_stmt(n->children[i]);
        indent_level--; pi(); printf("}\n");
        break;
    case N_IF:
        pi(); printf("if ("); print_expr(n->children[0],1); printf(")\n");
        indent_level++; print_stmt(n->children[1]); indent_level--;
        if (n->nchildren >= 3 && n->children[2]->kind != N_EMPTY) {
            pi(); printf("else\n");
            indent_level++; print_stmt(n->children[2]); indent_level--;
        }
        break;
    case N_FOR:
        pi(); printf("for (");
        if (n->children[0]->kind != N_EMPTY) print_expr(n->children[0],1);
        printf("; ");
        if (n->children[1]->kind != N_EMPTY) print_expr(n->children[1],1);
        printf("; ");
        if (n->children[2]->kind != N_EMPTY) print_expr(n->children[2],1);
        printf(")\n");
        indent_level++; print_stmt(n->children[3]); indent_level--;
        break;
    case N_WHILE:
        pi(); printf("while ("); print_expr(n->children[0],1); printf(")\n");
        indent_level++; print_stmt(n->children[1]); indent_level--;
        break;
    case N_DO:
        pi(); printf("do\n");
        indent_level++; print_stmt(n->children[0]); indent_level--;
        pi(); printf("while ("); print_expr(n->children[1],1); printf(");\n");
        break;
    case N_RETURN:
        pi(); printf("return");
        if (n->nchildren>0 && n->children[0]->kind!=N_EMPTY)
            { printf(" "); print_expr(n->children[0],1); }
        printf(";\n");
        break;
    case N_BREAK:    pi(); printf("break;\n");            break;
    case N_CONTINUE: pi(); printf("continue;\n");         break;
    case N_GOTO:     pi(); printf("goto %s;\n", n->sval); break;
    case N_SWITCH:
        pi(); printf("switch ("); print_expr(n->children[0],1); printf(")\n");
        print_stmt(n->children[1]);
        break;
    case N_CASE:
        indent_level--; pi();
        printf("case "); print_expr(n->children[0],1); printf(":\n");
        indent_level++;
        break;
    case N_DEFAULT:
        indent_level--; pi(); printf("default:\n"); indent_level++; break;
    case N_LABEL:
        indent_level--; pi(); printf("%s:\n", n->sval); indent_level++; break;
    case N_TRIPS: print_stmt(n->children[1]); break;
    case N_LIST:  for(int i=0;i<n->nchildren;i++) print_stmt(n->children[i]); break;
    case N_RULE: case N_DEFPREFIX: break;
    default: pi(); printf("/* node %d */\n", n->kind); break;
    }
}

static void process_program(Node *p) {
    for (int i=0;i<p->nchildren;i++) {
        Node *n = p->children[i];
        if (n->kind == N_RULE || n->kind == N_DEFPREFIX)
            rewrite(n);
    }
    for (int i=0;i<p->nchildren;i++) {
        Node *n = p->children[i];
        if (n->kind == N_RULE || n->kind == N_DEFPREFIX) continue;
        print_stmt(rewrite(n));
    }
}

int yylex(void);
void yyerror(const char *s) {
    extern int yylineno;
    fprintf(stderr, "ace: line %d: %s\n", yylineno, s);
}
%}

/* ---- value types ---- */
%union {
    char  *str;
    int    ival;
    Node  *node;
}

/* ---- tokens ---- */
%token <str>  IDENT ACE_IDENT TYPESPEC NUM_LIT STR_LIT CHAR_LIT PP_LINE
%token <ival> META_VAR META_FREE

%token KW_REPLACE KW_REPLACEAFTER KW_WITH KW_DEFPREFIX KW_LET
%token KW_TRIPS KW_TRADEOFF KW_PROB KW_PULLOUT KW_ASSUME

%token KW_IF KW_ELSE KW_FOR KW_WHILE KW_DO
%token KW_RETURN KW_SWITCH KW_CASE KW_DEFAULT
%token KW_BREAK KW_CONTINUE KW_GOTO KW_STRUCT KW_UNION
%token KW_ENUM KW_TYPEDEF KW_SIZEOF

%token OP_LSHIFT OP_RSHIFT OP_LEQ OP_GEQ OP_EQ OP_NEQ OP_AND OP_OR
%token OP_INC OP_DEC OP_ARROW OP_ELLIPSIS
%token OP_ADDASSIGN OP_SUBASSIGN OP_MULASSIGN OP_DIVASSIGN
%token OP_MODASSIGN OP_ANDASSIGN OP_ORASSIGN OP_XORASSIGN
%token OP_LSHASSIGN OP_RSHASSIGN

/* ---- nonterminal types ---- */
%type <node> program top_list top_item
%type <node> stmt stmt_list block
%type <node> ace_rule ace_defprefix
%type <node> expr assign_expr ternary_expr
%type <node> or_expr and_expr bitor_expr bitxor_expr bitand_expr
%type <node> eq_expr rel_expr shift_expr add_expr mul_expr
%type <node> unary_expr postfix_expr primary_expr
%type <node> arg_list opt_arg_list for_init for_cond for_update
%type <str>  type_name func_name

/* Resolve the dangling-else shift/reduce conflict in favour of shifting
   (associating else with the nearest if) — the standard C rule.
   Every stmt-ending production gets %prec LOWER_THAN_ELSE. */
%nonassoc LOWER_THAN_ELSE
%nonassoc KW_ELSE

%%

program : top_list { process_program($1); } ;

top_list
    : /* empty */       { $$ = make_list(); }
    | top_list top_item { node_add($1,$2); $$ = $1; }
    ;

top_item
    : ace_rule      { $$ = $1; }
    | ace_defprefix { $$ = $1; }
    | stmt          { $$ = $1; }
    | PP_LINE       { $$ = make_leaf(N_DECL,$1); free($1); }
    ;

/* ---- ace directives ---- */

ace_rule
    : KW_REPLACE expr ';' KW_WITH expr ';' {
          Node *r = node_new(N_RULE); r->ival = 0;
          node_add(r,$2); node_add(r,$5); $$ = r;
      }
    | KW_REPLACEAFTER expr ';' KW_WITH expr ';' {
          Node *r = node_new(N_RULE); r->ival = 1;
          node_add(r,$2); node_add(r,$5); $$ = r;
      }
    ;

ace_defprefix
    : KW_DEFPREFIX '(' ACE_IDENT ',' ACE_IDENT ')' ';' {
          Node *r = node_new(N_DEFPREFIX); r->sval = $3;
          node_add(r, make_leaf(N_IDENT,$5)); free($5); $$ = r;
      }
    ;

/* ---- Statements ---- */

stmt
    : block                                          { $$ = $1; }
    | KW_IF '(' expr ')' stmt %prec LOWER_THAN_ELSE {
          Node *n = node_new(N_IF);
          node_add(n,$3); node_add(n,$5); $$ = n;
      }
    | KW_IF '(' expr ')' stmt KW_ELSE stmt {
          Node *n = node_new(N_IF);
          node_add(n,$3); node_add(n,$5); node_add(n,$7); $$ = n;
      }
    | KW_FOR '(' for_init ';' for_cond ';' for_update ')' stmt %prec LOWER_THAN_ELSE {
          Node *n = node_new(N_FOR);
          node_add(n,$3); node_add(n,$5); node_add(n,$7); node_add(n,$9); $$ = n;
      }
    | KW_WHILE '(' expr ')' stmt %prec LOWER_THAN_ELSE {
          Node *n = node_new(N_WHILE); node_add(n,$3); node_add(n,$5); $$ = n;
      }
    | KW_DO stmt KW_WHILE '(' expr ')' ';' {
          Node *n = node_new(N_DO); node_add(n,$2); node_add(n,$5); $$ = n;
      }
    | KW_RETURN ';'      { $$ = node_new(N_RETURN); }
    | KW_RETURN expr ';' { Node *n=node_new(N_RETURN); node_add(n,$2); $$=n; }
    | KW_BREAK ';'       { $$ = node_new(N_BREAK); }
    | KW_CONTINUE ';'    { $$ = node_new(N_CONTINUE); }
    | KW_GOTO IDENT ';'  { Node *n=node_new(N_GOTO); n->sval=$2; $$=n; }
    | KW_SWITCH '(' expr ')' stmt %prec LOWER_THAN_ELSE {
          Node *n=node_new(N_SWITCH); node_add(n,$3); node_add(n,$5); $$=n;
      }
    | KW_CASE expr ':'   { Node *n=node_new(N_CASE); node_add(n,$2); $$=n; }
    | KW_DEFAULT ':'     { $$=node_new(N_DEFAULT); }
    | IDENT ':' stmt %prec LOWER_THAN_ELSE {
          Node *n=node_new(N_LABEL); n->sval=$1;
          Node *blk=node_new(N_LIST);
          node_add(blk,n); node_add(blk,$3); $$=blk;
      }
    | KW_TRIPS '(' expr ')' stmt %prec LOWER_THAN_ELSE {
          Node *n=node_new(N_TRIPS); node_add(n,$3); node_add(n,$5); $$=n;
      }
    | ACE_IDENT '(' opt_arg_list ')' stmt %prec LOWER_THAN_ELSE {
          Node *call=node_new(N_CALL); call->sval=$1;
          node_add(call,$3); node_add(call,$5); $$=call;
      }
    | type_name ';' { Node *n=node_new(N_DECL); n->sval=$1; $$=n; }
    | expr ';'      { Node *n=node_new(N_EXPR_STMT); node_add(n,$1); $$=n; }
    | ';'           { $$ = node_new(N_EMPTY); }
    ;

block     : '{' stmt_list '}' { $$ = $2; $$->kind = N_BLOCK; } ;
stmt_list : /* empty */       { $$ = make_list(); }
          | stmt_list stmt    { node_add($1,$2); $$ = $1; }
          ;

for_init : /* empty */ { $$=node_new(N_EMPTY); } | expr { $$=$1; } ;
for_cond : /* empty */ { $$=node_new(N_EMPTY); } | expr { $$=$1; } ;
for_update:/* empty */ { $$=node_new(N_EMPTY); } | expr { $$=$1; } ;

type_name
    : TYPESPEC IDENT     { char b[512]; snprintf(b,sizeof b,"%s %s;",$1,$2);
                           free($1); free($2); $$=strdup(b); }
    | TYPESPEC '*' IDENT { char b[512]; snprintf(b,sizeof b,"%s *%s;",$1,$3);
                           free($1); free($3); $$=strdup(b); }
    ;

/* ---- Expressions (standard C precedence) ---- */

expr
    : assign_expr              { $$ = $1; }
    | expr ',' assign_expr     { $$ = make_binop(",",$1,$3); }
    ;

assign_expr
    : ternary_expr                            { $$ = $1; }
    | unary_expr '='          assign_expr     { $$ = make_binop("=",$1,$3); }
    | unary_expr OP_ADDASSIGN assign_expr     { $$ = make_binop("+=",$1,$3); }
    | unary_expr OP_SUBASSIGN assign_expr     { $$ = make_binop("-=",$1,$3); }
    | unary_expr OP_MULASSIGN assign_expr     { $$ = make_binop("*=",$1,$3); }
    | unary_expr OP_DIVASSIGN assign_expr     { $$ = make_binop("/=",$1,$3); }
    | unary_expr OP_MODASSIGN assign_expr     { $$ = make_binop("%=",$1,$3); }
    | unary_expr OP_ANDASSIGN assign_expr     { $$ = make_binop("&=",$1,$3); }
    | unary_expr OP_ORASSIGN  assign_expr     { $$ = make_binop("|=",$1,$3); }
    | unary_expr OP_XORASSIGN assign_expr     { $$ = make_binop("^=",$1,$3); }
    | unary_expr OP_LSHASSIGN assign_expr     { $$ = make_binop("<<=",$1,$3); }
    | unary_expr OP_RSHASSIGN assign_expr     { $$ = make_binop(">>=",$1,$3); }
    ;

ternary_expr
    : or_expr                              { $$ = $1; }
    | or_expr '?' expr ':' ternary_expr   {
          Node *n=node_new(N_TERNARY);
          node_add(n,$1); node_add(n,$3); node_add(n,$5); $$=n;
      }
    ;

or_expr  : and_expr                   { $$ = $1; }
         | or_expr OP_OR and_expr     { $$ = make_binop("||",$1,$3); }
         ;
and_expr : bitor_expr                 { $$ = $1; }
         | and_expr OP_AND bitor_expr { $$ = make_binop("&&",$1,$3); }
         ;
bitor_expr  : bitxor_expr                   { $$ = $1; }
            | bitor_expr '|' bitxor_expr    { $$ = make_binop("|",$1,$3); }
            ;
bitxor_expr : bitand_expr                   { $$ = $1; }
            | bitxor_expr '^' bitand_expr   { $$ = make_binop("^",$1,$3); }
            ;
bitand_expr : eq_expr                       { $$ = $1; }
            | bitand_expr '&' eq_expr       { $$ = make_binop("&",$1,$3); }
            ;
eq_expr  : rel_expr                    { $$ = $1; }
         | eq_expr OP_EQ  rel_expr     { $$ = make_binop("==",$1,$3); }
         | eq_expr OP_NEQ rel_expr     { $$ = make_binop("!=",$1,$3); }
         ;
rel_expr : shift_expr                  { $$ = $1; }
         | rel_expr '<'    shift_expr  { $$ = make_binop("<",$1,$3); }
         | rel_expr '>'    shift_expr  { $$ = make_binop(">",$1,$3); }
         | rel_expr OP_LEQ shift_expr  { $$ = make_binop("<=",$1,$3); }
         | rel_expr OP_GEQ shift_expr  { $$ = make_binop(">=",$1,$3); }
         ;
shift_expr : add_expr                        { $$ = $1; }
           | shift_expr OP_LSHIFT add_expr   { $$ = make_binop("<<",$1,$3); }
           | shift_expr OP_RSHIFT add_expr   { $$ = make_binop(">>",$1,$3); }
           ;
add_expr : mul_expr                    { $$ = $1; }
         | add_expr '+' mul_expr       { $$ = make_binop("+",$1,$3); }
         | add_expr '-' mul_expr       { $$ = make_binop("-",$1,$3); }
         ;
mul_expr : unary_expr                  { $$ = $1; }
         | mul_expr '*' unary_expr     { $$ = make_binop("*",$1,$3); }
         | mul_expr '/' unary_expr     { $$ = make_binop("/",$1,$3); }
         | mul_expr '%' unary_expr     { $$ = make_binop("%",$1,$3); }
         ;

unary_expr
    : postfix_expr              { $$ = $1; }
    | OP_INC unary_expr         { $$ = make_unary(N_UNARY_PRE,"++",$2); }
    | OP_DEC unary_expr         { $$ = make_unary(N_UNARY_PRE,"--",$2); }
    | '&' unary_expr            { $$ = make_unary(N_UNARY_PRE,"&",$2); }
    | '*' unary_expr            { $$ = make_unary(N_UNARY_PRE,"*",$2); }
    | '+' unary_expr            { $$ = make_unary(N_UNARY_PRE,"+",$2); }
    | '-' unary_expr            { $$ = make_unary(N_UNARY_PRE,"-",$2); }
    | '~' unary_expr            { $$ = make_unary(N_UNARY_PRE,"~",$2); }
    | '!' unary_expr            { $$ = make_unary(N_UNARY_PRE,"!",$2); }
    | KW_SIZEOF '(' unary_expr ')' { $$ = make_unary(N_UNARY_PRE,"sizeof",$3); }
    | KW_SIZEOF '(' type_name  ')' { Node *n=node_new(N_SIZEOF_TYPE); n->sval=$3; $$=n; }
    ;

postfix_expr
    : primary_expr                           { $$ = $1; }
    | postfix_expr OP_INC                    { $$ = make_unary(N_UNARY_POST,"++",$1); }
    | postfix_expr OP_DEC                    { $$ = make_unary(N_UNARY_POST,"--",$1); }
    | postfix_expr '[' expr ']'              {
          Node *n=node_new(N_INDEX); node_add(n,$1); node_add(n,$3); $$=n; }
    | postfix_expr '.' IDENT                 {
          Node *n=node_new(N_MEMBER_DOT); n->sval=$3; node_add(n,$1); $$=n; }
    | postfix_expr OP_ARROW IDENT            {
          Node *n=node_new(N_MEMBER_ARROW); n->sval=$3; node_add(n,$1); $$=n; }
    | func_name '(' opt_arg_list ')' {
          Node *n=node_new(N_CALL); n->sval=$1; node_add(n,$3); $$=n; }
    ;

func_name
    : IDENT     { $$ = $1; }
    | ACE_IDENT { $$ = $1; }
    | KW_LET    { $$ = strdup("$LET"); }
    ;

primary_expr
    : NUM_LIT               { $$ = make_leaf(N_NUM,$1);   free($1); }
    | STR_LIT               { $$ = make_leaf(N_STR,$1);   free($1); }
    | CHAR_LIT              { $$ = make_leaf(N_STR,$1);   free($1); }
    | IDENT                 { $$ = make_leaf(N_IDENT,$1); free($1); }
    | ACE_IDENT             { $$ = make_leaf(N_IDENT,$1); free($1); }
    | META_VAR              { $$ = make_ival(N_METAVAR,$1); }
    | META_FREE             { $$ = make_ival(N_METAVAR_FREE,$1); }
    | '(' expr ')'          { $$ = $2; }
    ;

opt_arg_list
    : /* empty */           { $$ = make_list(); }
    | arg_list              { $$ = $1; }
    ;
arg_list
    : assign_expr                { Node *l=make_list(); node_add(l,$1); $$=l; }
    | arg_list ',' assign_expr   { node_add($1,$3); $$=$1; }
    ;

%%

int main(int argc, char **argv) {
    extern FILE *yyin;
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) { perror(argv[1]); return 1; }
    }
    return yyparse();
}
