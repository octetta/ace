/*
 * ace.c  --  A C implementation of James Gosling's "ace" preprocessor (1989).
 *
 * "Ace: a syntax-driven C preprocessor", James Gosling, July 1989.
 * https://swtch.com/gosling89ace.pdf
 *
 * ace reads C source extended with ace directives, builds an AST, applies
 * tree-rewrite rules, and pretty-prints the result.
 *
 * Supported ace constructs:
 *   $replace <pat>; $with <repl>;          -- define rewrite rule
 *   $replaceafter <pat>; $with <repl>;     -- rule applied post-subnode reduction
 *   $defprefix($name, $fn);                -- define prefix-statement macro
 *   $0..$9                                 -- meta-variable (match any expr)
 *   $f0..$f9                               -- meta-variable (side-effect-free)
 *   $LET(a0,a1, a2,a3, ..., body)          -- local substitution
 *   $trips(n) <stmt>                       -- annotate loop trip count
 *   $tradeoff(time_code, space_code)       -- time/space selector
 *   $P(expr)                               -- probability annotation (kept as-is)
 *
 * Build:  gcc -Wall -o ace ace.c
 * Usage:  ace [file.c]   (reads stdin if no file)
 */

/* strdup is POSIX — expose it */
#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>

/* ============================================================
 *  LEXER
 * ============================================================ */

typedef enum {
    /* literals */
    TK_NUM, TK_STR, TK_CHAR,
    /* identifiers */
    TK_IDENT,
    /* ace tokens */
    TK_META,        /* $0..$9  */
    TK_META_FREE,   /* $f0..$f9 */
    TK_ACE_IDENT,   /* $name   */
    TK_REPLACE, TK_REPLACEAFTER, TK_WITH,
    TK_DEFPREFIX,
    TK_LET,
    TK_TRIPS,
    TK_TRADEOFF,
    TK_PROB,
    TK_PULLOUT,
    TK_ASSUME,
    /* C keywords */
    TK_IF, TK_ELSE, TK_FOR, TK_WHILE, TK_DO,
    TK_RETURN, TK_SWITCH, TK_CASE, TK_DEFAULT,
    TK_BREAK, TK_CONTINUE, TK_GOTO,
    TK_STRUCT, TK_UNION, TK_ENUM, TK_TYPEDEF, TK_SIZEOF,
    TK_TYPESPEC,    /* int char short long float double void unsigned signed
                       register auto static extern const volatile */
    /* operators */
    TK_LSHIFT, TK_RSHIFT,
    TK_LEQ, TK_GEQ, TK_EQ, TK_NEQ,
    TK_AND, TK_OR,
    TK_INC, TK_DEC,
    TK_ARROW,
    TK_ADDASSIGN, TK_SUBASSIGN, TK_MULASSIGN, TK_DIVASSIGN,
    TK_MODASSIGN, TK_ANDASSIGN, TK_ORASSIGN, TK_XORASSIGN,
    TK_LSHASSIGN, TK_RSHASSIGN,
    /* single-char punctuation stored as char value – but we use a sentinel */
    TK_PUNCT,
    /* misc */
    TK_PP,          /* #... preprocessor line */
    TK_EOF,
} TKind;

typedef struct {
    TKind kind;
    char  *val;   /* heap-allocated text */
    int    ival;  /* for META, META_FREE */
    int    line;
} Token;

/* ---- token buffer ---- */
#define MAX_TOKENS 1<<20
static Token toks[MAX_TOKENS];
static int   ntoks = 0;
static int   tpos  = 0;

static Token *tok_peek(int off) {
    int i = tpos + off;
    if (i >= ntoks) return &toks[ntoks-1]; /* EOF sentinel */
    return &toks[i];
}
static Token *tok_cur(void)   { return tok_peek(0); }
static void   tok_advance(void){ if (tpos < ntoks) tpos++; }

static Token *tok_consume(void) {
    Token *t = tok_cur();
    tok_advance();
    return t;
}

static Token *tok_expect(TKind k, const char *val) {
    Token *t = tok_cur();
    if (t->kind != k && !(k == TK_PUNCT && t->val[0] == val[0])) {
        fprintf(stderr, "ace: line %d: expected '%s', got '%s'\n",
                t->line, val, t->val);
        exit(1);
    }
    return tok_consume();
}

static int tok_is(const char *v) {
    return strcmp(tok_cur()->val, v) == 0;
}
static int tok_try(const char *v) {
    if (tok_is(v)) { tok_advance(); return 1; }
    return 0;
}

/* ---- tokenizer ---- */
static char *src;
static int   slen, spos, sline;

static void lex_error(const char *msg) {
    fprintf(stderr, "ace: lex error at line %d: %s\n", sline, msg);
    exit(1);
}

static char lex_ch(void)  { return spos < slen ? src[spos] : 0; }
static char lex_ch2(void) { return spos+1 < slen ? src[spos+1] : 0; }
static char lex_advance(void) {
    char c = src[spos++];
    if (c == '\n') sline++;
    return c;
}

static char *buf;
static int   blen, bpos;
static void  buf_reset(void) { bpos = 0; }
static void  buf_add(char c) {
    if (bpos >= blen-1) { blen *= 2; buf = realloc(buf, blen); }
    buf[bpos++] = c; buf[bpos] = 0;
}
static char *buf_dup(void) { return strdup(buf); }

static struct { const char *word; TKind kind; } keywords[] = {
    {"if",       TK_IF},       {"else",     TK_ELSE},
    {"for",      TK_FOR},      {"while",    TK_WHILE},
    {"do",       TK_DO},       {"return",   TK_RETURN},
    {"switch",   TK_SWITCH},   {"case",     TK_CASE},
    {"default",  TK_DEFAULT},  {"break",    TK_BREAK},
    {"continue", TK_CONTINUE}, {"goto",     TK_GOTO},
    {"struct",   TK_STRUCT},   {"union",    TK_UNION},
    {"enum",     TK_ENUM},     {"typedef",  TK_TYPEDEF},
    {"sizeof",   TK_SIZEOF},
    {"int",      TK_TYPESPEC}, {"char",     TK_TYPESPEC},
    {"short",    TK_TYPESPEC}, {"long",     TK_TYPESPEC},
    {"float",    TK_TYPESPEC}, {"double",   TK_TYPESPEC},
    {"void",     TK_TYPESPEC}, {"unsigned", TK_TYPESPEC},
    {"signed",   TK_TYPESPEC}, {"register", TK_TYPESPEC},
    {"auto",     TK_TYPESPEC}, {"static",   TK_TYPESPEC},
    {"extern",   TK_TYPESPEC}, {"const",    TK_TYPESPEC},
    {"volatile", TK_TYPESPEC},
    {NULL, TK_EOF}
};

static struct { const char *word; TKind kind; } ace_kws[] = {
    {"$replace",      TK_REPLACE},
    {"$replaceafter", TK_REPLACEAFTER},
    {"$with",         TK_WITH},
    {"$defprefix",    TK_DEFPREFIX},
    {"$LET",          TK_LET},
    {"$let",          TK_LET},
    {"$trips",        TK_TRIPS},
    {"$tradeoff",     TK_TRADEOFF},
    {"$P",            TK_PROB},
    {"$pullout",      TK_PULLOUT},
    {"$assume",       TK_ASSUME},
    {NULL, TK_EOF}
};

static void add_tok(TKind k, const char *v, int ival, int line) {
    if (ntoks >= MAX_TOKENS) lex_error("too many tokens");
    toks[ntoks].kind = k;
    toks[ntoks].val  = strdup(v);
    toks[ntoks].ival = ival;
    toks[ntoks].line = line;
    ntoks++;
}

static void lex_all(const char *source) {
    src  = (char*)source;
    slen = strlen(source);
    spos = 0;
    sline = 1;
    blen = 4096; buf = malloc(blen); buf[0] = 0;

    /* EOF sentinel */
    while (spos < slen) {
        int line = sline;
        char c = lex_ch();

        /* whitespace */
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            lex_advance(); continue;
        }

        /* line comment */
        if (c == '/' && lex_ch2() == '/') {
            while (spos < slen && lex_ch() != '\n') lex_advance();
            continue;
        }

        /* block comment */
        if (c == '/' && lex_ch2() == '*') {
            lex_advance(); lex_advance();
            while (spos < slen) {
                if (lex_ch() == '*' && lex_ch2() == '/') {
                    lex_advance(); lex_advance(); break;
                }
                lex_advance();
            }
            continue;
        }

        /* preprocessor line */
        if (c == '#') {
            buf_reset(); buf_add(c); lex_advance();
            while (spos < slen && lex_ch() != '\n')
                buf_add(lex_advance());
            add_tok(TK_PP, buf_dup(), 0, line);
            continue;
        }

        /* string literal */
        if (c == '"') {
            buf_reset(); buf_add(c); lex_advance();
            while (spos < slen && lex_ch() != '"') {
                if (lex_ch() == '\\') { buf_add(lex_advance()); }
                buf_add(lex_advance());
            }
            buf_add(lex_advance()); /* closing " */
            add_tok(TK_STR, buf_dup(), 0, line);
            continue;
        }

        /* char literal */
        if (c == '\'') {
            buf_reset(); buf_add(c); lex_advance();
            while (spos < slen && lex_ch() != '\'') {
                if (lex_ch() == '\\') { buf_add(lex_advance()); }
                buf_add(lex_advance());
            }
            buf_add(lex_advance());
            add_tok(TK_CHAR, buf_dup(), 0, line);
            continue;
        }

        /* number */
        if (isdigit(c) || (c == '.' && isdigit(lex_ch2()))) {
            buf_reset();
            if (c == '0' && (lex_ch2() == 'x' || lex_ch2() == 'X')) {
                buf_add(lex_advance()); buf_add(lex_advance());
                while (isxdigit(lex_ch())) buf_add(lex_advance());
            } else {
                while (isdigit(lex_ch()) || lex_ch() == '.') buf_add(lex_advance());
                if (lex_ch() == 'e' || lex_ch() == 'E') {
                    buf_add(lex_advance());
                    if (lex_ch() == '+' || lex_ch() == '-') buf_add(lex_advance());
                    while (isdigit(lex_ch())) buf_add(lex_advance());
                }
            }
            /* suffixes */
            while (lex_ch()=='u'||lex_ch()=='U'||lex_ch()=='l'||
                   lex_ch()=='L'||lex_ch()=='f'||lex_ch()=='F')
                buf_add(lex_advance());
            add_tok(TK_NUM, buf_dup(), 0, line);
            continue;
        }

        /* ace $ tokens */
        if (c == '$') {
            buf_reset(); buf_add(lex_advance());
            /* $f0..$f9 */
            if (lex_ch() == 'f' && isdigit(lex_ch2())) {
                lex_advance(); /* skip 'f' */
                int idx = lex_advance() - '0';
                char tmp[8]; snprintf(tmp,sizeof tmp,"$f%d",idx);
                add_tok(TK_META_FREE, tmp, idx, line);
                continue;
            }
            /* $0..$9 */
            if (isdigit(lex_ch())) {
                int idx = lex_advance() - '0';
                char tmp[8]; snprintf(tmp,sizeof tmp,"$%d",idx);
                add_tok(TK_META, tmp, idx, line);
                continue;
            }
            /* $ident */
            while (isalnum(lex_ch()) || lex_ch() == '_') buf_add(lex_advance());
            /* check ace keywords */
            TKind kk = TK_ACE_IDENT;
            for (int i=0; ace_kws[i].word; i++)
                if (strcmp(buf,ace_kws[i].word)==0) { kk=ace_kws[i].kind; break; }
            add_tok(kk, buf_dup(), 0, line);
            continue;
        }

        /* identifier / keyword */
        if (isalpha(c) || c == '_') {
            buf_reset();
            while (isalnum(lex_ch()) || lex_ch() == '_') buf_add(lex_advance());
            TKind kk = TK_IDENT;
            for (int i=0; keywords[i].word; i++)
                if (strcmp(buf,keywords[i].word)==0) { kk=keywords[i].kind; break; }
            add_tok(kk, buf_dup(), 0, line);
            continue;
        }

        /* multi-char operators */
        lex_advance();
#define PEEK2(a,b) (c==(a) && lex_ch()==(b))
        if PEEK2('<','<') { if(lex_ch2()=='='){lex_advance();lex_advance();add_tok(TK_PUNCT,"<<=",0,line);}
                             else {lex_advance(); add_tok(TK_LSHIFT,"<<",0,line);} continue; }
        if PEEK2('>','>') { if(lex_ch2()=='='){lex_advance();lex_advance();add_tok(TK_PUNCT,">>=",0,line);}
                             else {lex_advance(); add_tok(TK_RSHIFT,">>",0,line);} continue; }
        if PEEK2('<','=') { lex_advance(); add_tok(TK_LEQ,"<=",0,line); continue; }
        if PEEK2('>','=') { lex_advance(); add_tok(TK_GEQ,">=",0,line); continue; }
        if PEEK2('=','=') { lex_advance(); add_tok(TK_EQ,"==",0,line); continue; }
        if PEEK2('!','=') { lex_advance(); add_tok(TK_NEQ,"!=",0,line); continue; }
        if PEEK2('&','&') { lex_advance(); add_tok(TK_AND,"&&",0,line); continue; }
        if PEEK2('|','|') { lex_advance(); add_tok(TK_OR,"||",0,line); continue; }
        if PEEK2('+','+') { lex_advance(); add_tok(TK_INC,"++",0,line); continue; }
        if PEEK2('-','-') { lex_advance(); add_tok(TK_DEC,"--",0,line); continue; }
        if PEEK2('-','>') { lex_advance(); add_tok(TK_ARROW,"->",0,line); continue; }
        if PEEK2('+','=') { lex_advance(); add_tok(TK_ADDASSIGN,"+=",0,line); continue; }
        if PEEK2('-','=') { lex_advance(); add_tok(TK_SUBASSIGN,"-=",0,line); continue; }
        if PEEK2('*','=') { lex_advance(); add_tok(TK_MULASSIGN,"*=",0,line); continue; }
        if PEEK2('/','=') { lex_advance(); add_tok(TK_DIVASSIGN,"/=",0,line); continue; }
        if PEEK2('%','=') { lex_advance(); add_tok(TK_MODASSIGN,"%=",0,line); continue; }
        if PEEK2('&','=') { lex_advance(); add_tok(TK_ANDASSIGN,"&=",0,line); continue; }
        if PEEK2('|','=') { lex_advance(); add_tok(TK_ORASSIGN,"|=",0,line); continue; }
        if PEEK2('^','=') { lex_advance(); add_tok(TK_XORASSIGN,"^=",0,line); continue; }
        if (c=='.'&&lex_ch()=='.'&&lex_ch2()=='.') {
            lex_advance(); lex_advance();
            add_tok(TK_PUNCT,"...",0,line); continue;
        }

        /* single-char punct */
        char tmp[3]; tmp[0]=c; tmp[1]=0;
        add_tok(TK_PUNCT, tmp, 0, line);
    }
    /* EOF sentinel */
    add_tok(TK_EOF, "<EOF>", 0, sline);
}


/* ============================================================
 *  AST
 * ============================================================ */

typedef enum {
    N_NUM, N_STR, N_IDENT,
    N_METAVAR,          /* $0..$9   */
    N_METAVAR_FREE,     /* $f0..$f9 */
    N_BINOP,
    N_UNARY_PRE, N_UNARY_POST,
    N_TERNARY,
    N_CALL,
    N_CAST,
    N_INDEX,
    N_MEMBER_DOT, N_MEMBER_ARROW,
    N_SIZEOF_EXPR, N_SIZEOF_TYPE,
    N_EXPR_STMT,
    N_BLOCK,
    N_IF,
    N_FOR,
    N_WHILE,
    N_DO,
    N_RETURN,
    N_BREAK,
    N_CONTINUE,
    N_GOTO,
    N_SWITCH,
    N_CASE,
    N_DEFAULT,
    N_LABEL,
    N_DECL,
    N_EMPTY,
    N_RULE,
    N_DEFPREFIX,
    N_TRIPS,
    N_TRADEOFF,
    N_PROB,
    N_LET,
    N_LIST,
} NK;

#define MAX_CH 128

typedef struct Node {
    NK    kind;
    char *sval;
    int   ival;
    struct Node *ch[MAX_CH];
    int   nch;
} Node;

static Node *node_new(NK k) {
    Node *n = calloc(1, sizeof *n);
    n->kind = k;
    return n;
}
static void nadd(Node *p, Node *c) {
    if (!c) return;
    if (p->nch >= MAX_CH) { fprintf(stderr,"ace: too many children\n"); exit(1); }
    p->ch[p->nch++] = c;
}

static Node *leaf(NK k, const char *s) {
    Node *n = node_new(k);
    n->sval = strdup(s);
    return n;
}
static Node *ival_node(NK k, int v) {
    Node *n = node_new(k);
    n->ival = v;
    return n;
}
static Node *binop(const char *op, Node *l, Node *r) {
    Node *n = node_new(N_BINOP);
    n->sval = strdup(op);
    nadd(n,l); nadd(n,r);
    return n;
}
static Node *unary(NK k, const char *op, Node *e) {
    Node *n = node_new(k);
    n->sval = strdup(op);
    nadd(n,e);
    return n;
}
static Node *list_new(void) { return node_new(N_LIST); }

/* ============================================================
 *  RULE TABLE
 * ============================================================ */

#define MAX_RULES 512
typedef struct { Node *pat; Node *rep; int after; } Rule;
static Rule rules[MAX_RULES];
static int  nrules = 0;

static void rule_add(Node *p, Node *r, int a) {
    if (nrules >= MAX_RULES) { fprintf(stderr,"ace: rule table full\n"); exit(1); }
    rules[nrules].pat   = p;
    rules[nrules].rep   = r;
    rules[nrules].after = a;
    nrules++;
}

/* ---- PREFIX TABLE ---- */
#define MAX_PFX 64
typedef struct { char *name; char *func; } Prefix;
static Prefix pfxs[MAX_PFX];
static int npfxs = 0;

static void pfx_add(const char *nm, const char *fn) {
    pfxs[npfxs].name = strdup(nm);
    pfxs[npfxs].func = strdup(fn);
    npfxs++;
}
static const char *pfx_find(const char *nm) {
    for(int i=0;i<npfxs;i++)
        if(strcmp(pfxs[i].name,nm)==0) return pfxs[i].func;
    return NULL;
}

/* ============================================================
 *  PATTERN MATCHING
 * ============================================================ */

#define MAX_META 10
static Node *bindings[MAX_META];

static int is_sef(Node *n) {
    if (!n) return 1;
    switch (n->kind) {
    case N_NUM: case N_STR: case N_IDENT: return 1;
    case N_BINOP: {
        const char *op = n->sval;
        int last = op[strlen(op)-1];
        if (last == '=' && strcmp(op,"==") && strcmp(op,"!=") &&
            strcmp(op,"<=") && strcmp(op,">=")) return 0;
        return is_sef(n->ch[0]) && is_sef(n->ch[1]);
    }
    case N_UNARY_PRE: case N_UNARY_POST:
        if (strcmp(n->sval,"++")==0||strcmp(n->sval,"--")==0) return 0;
        return is_sef(n->ch[0]);
    case N_CALL: return 0;
    default:
        for (int i=0;i<n->nch;i++) if (!is_sef(n->ch[i])) return 0;
        return 1;
    }
}

static int node_eq(Node *a, Node *b);

static int match(Node *pat, Node *n) {
    if (!pat && !n) return 1;
    if (!pat || !n) return 0;

    if (pat->kind == N_METAVAR) {
        int i = pat->ival;
        if (bindings[i]) return node_eq(bindings[i], n);
        bindings[i] = n; return 1;
    }
    if (pat->kind == N_METAVAR_FREE) {
        if (!is_sef(n)) return 0;
        int i = pat->ival;
        if (bindings[i]) return node_eq(bindings[i], n);
        bindings[i] = n; return 1;
    }

    if (pat->kind != n->kind) return 0;

    /* compare sval */
    if (pat->sval && n->sval) { if (strcmp(pat->sval,n->sval)) return 0; }
    else if (pat->sval || n->sval) return 0;

    if (pat->ival != n->ival) return 0;
    if (pat->nch  != n->nch)  return 0;

    for (int i=0;i<pat->nch;i++)
        if (!match(pat->ch[i], n->ch[i])) return 0;
    return 1;
}

static int node_eq(Node *a, Node *b) {
    Node *sv[MAX_META];
    memcpy(sv, bindings, sizeof bindings);
    memset(bindings, 0, sizeof bindings);
    int r = match(a, b);
    memcpy(bindings, sv, sizeof bindings);
    return r;
}

/* Deep copy with meta-variable substitution */
static Node *subst(Node *n) {
    if (!n) return NULL;
    if (n->kind == N_METAVAR || n->kind == N_METAVAR_FREE) {
        Node *b = bindings[n->ival];
        return b ? b : n;
    }
    Node *c = node_new(n->kind);
    c->sval = n->sval ? strdup(n->sval) : NULL;
    c->ival = n->ival;
    for (int i=0;i<n->nch;i++) nadd(c, subst(n->ch[i]));
    return c;
}

/* ============================================================
 *  REWRITER
 * ============================================================ */

static Node *rewrite(Node *n);

static Node *apply_rules(Node *n, int after) {
    for (int i=0;i<nrules;i++) {
        if (rules[i].after != after) continue;
        memset(bindings, 0, sizeof bindings);
        if (match(rules[i].pat, n)) {
            Node *r = subst(rules[i].rep);
            return rewrite(r);
        }
    }
    return n;
}

/* Constant fold: evaluate simple numeric binary ops */
static Node *cfold(Node *n) {
    if (n->kind != N_BINOP) return n;
    Node *l = n->ch[0], *r = n->ch[1];
    if (!l||!r) return n;
    if (l->kind != N_NUM || r->kind != N_NUM) return n;
    long lv = strtol(l->sval, NULL, 0);
    long rv = strtol(r->sval, NULL, 0);
    long res; int ok=1;
    const char *op = n->sval;
    if (!strcmp(op,"+"))  res = lv+rv;
    else if (!strcmp(op,"-"))  res = lv-rv;
    else if (!strcmp(op,"*"))  res = lv*rv;
    else if (!strcmp(op,"/"))  res = rv ? lv/rv : (ok=0,0);
    else if (!strcmp(op,"%"))  res = rv ? lv%rv : (ok=0,0);
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
    return leaf(N_NUM, tmp);
}

/* Constant fold unary */
static Node *cfold_unary(Node *n) {
    if (n->kind != N_UNARY_PRE) return n;
    Node *e = n->ch[0];
    if (!e || e->kind != N_NUM) return n;
    long v = strtol(e->sval, NULL, 0);
    long res; int ok=1;
    const char *op = n->sval;
    if      (!strcmp(op,"!")) res = !v;
    else if (!strcmp(op,"-")) res = -v;
    else if (!strcmp(op,"~")) res = ~v;
    else ok=0, res=0;
    if (!ok) return n;
    char tmp[32]; snprintf(tmp,sizeof tmp,"%ld",res);
    return leaf(N_NUM, tmp);
}

/* Expand $LET: args is N_LIST with (a0,v0, a1,v1, ..., body) */
static Node *expand_let(Node *args) {
    int n = args->nch;
    if (n == 0) return leaf(N_NUM,"0");

    Node *body = args->ch[n-1];

    /* Save rule count, add temporary rules */
    int saved = nrules;
    for (int i=0; i+1 < n-1; i+=2) {
        rule_add(args->ch[i], args->ch[i+1], 0);
    }

    Node *result = rewrite(body);

    /* Remove temporary rules */
    nrules = saved;
    return result;
}

static Node *rewrite_children(Node *n) {
    for (int i=0;i<n->nch;i++)
        n->ch[i] = rewrite(n->ch[i]);
    return n;
}

static Node *rewrite(Node *n) {
    if (!n) return NULL;

    /* Process structural ace nodes first */
    if (n->kind == N_RULE) {
        rule_add(n->ch[0], n->ch[1], n->ival);
        return leaf(N_EMPTY,"");
    }
    if (n->kind == N_DEFPREFIX) {
        pfx_add(n->sval, n->ch[0]->sval);
        return leaf(N_EMPTY,"");
    }
    if (n->kind == N_LET) {
        return expand_let(n->ch[0]);
    }

    /* Try pre-reduction rules */
    n = apply_rules(n, 0);

    /* Recurse into children */
    rewrite_children(n);

    /* Try post-reduction rules */
    n = apply_rules(n, 1);

    /* Constant folding */
    n = cfold(n);
    n = cfold_unary(n);

    /* Dead-code elimination: if (constant) */
    if (n->kind == N_IF) {
        Node *cond = n->ch[0];
        if (cond && cond->kind == N_NUM) {
            long v = strtol(cond->sval, NULL, 0);
            if (v) return rewrite(n->ch[1]);
            else   return (n->nch >= 3) ? rewrite(n->ch[2]) : leaf(N_EMPTY,"");
        }
    }

    /* Ternary with constant condition */
    if (n->kind == N_TERNARY) {
        Node *cond = n->ch[0];
        if (cond && cond->kind == N_NUM) {
            long v = strtol(cond->sval, NULL, 0);
            return rewrite(v ? n->ch[1] : n->ch[2]);
        }
    }

    /* Eliminate no-effect statements: pure-expr with no side effects */
    if (n->kind == N_EXPR_STMT) {
        Node *e = n->ch[0];
        if (e && is_sef(e) && e->kind != N_CALL) {
            /* side-effect-free expression statement can be dropped */
            return leaf(N_EMPTY,"");
        }
    }

    return n;
}


/* ============================================================
 *  RECURSIVE-DESCENT PARSER
 * ============================================================ */

static Node *parse_stmt(void);
static Node *parse_expr(void);
static Node *parse_assign(void);

static int is_typespec(void) {
    Token *t = tok_cur();
    return t->kind == TK_TYPESPEC || t->kind == TK_STRUCT ||
           t->kind == TK_UNION   || t->kind == TK_ENUM   ||
           t->kind == TK_TYPEDEF;
}

/* Try to detect "typespec [*] ident" as the start of a declaration */
static int is_decl_start(void) {
    if (!is_typespec()) return 0;
    /* peek ahead: typespec (star*) ident (not just a typespec alone) */
    int save = tpos;
    tok_advance(); /* skip typespec */
    /* allow unsigned long int etc. */
    while (tok_cur()->kind == TK_TYPESPEC) tok_advance();
    while (tok_is("*")) tok_advance();
    int r = (tok_cur()->kind == TK_IDENT);
    tpos = save;
    return r;
}

/* Parse a declaration statement (simplified: stops at ;) */
static Node *parse_decl(void) {
    /* Collect tokens until ';' (handling nested parens for function ptrs) */
    char buf2[4096]; int bp=0;
    int depth = 0;
    while (1) {
        Token *t = tok_cur();
        if (t->kind == TK_EOF) break;
        if (t->kind == TK_PUNCT && t->val[0]=='{') break;
        if (t->kind == TK_PUNCT && t->val[0]==')') { if(depth>0) depth--; else break; }
        if (depth==0 && t->kind == TK_PUNCT && t->val[0]==';') {
            tok_advance();
            if (bp < (int)sizeof buf2-2) buf2[bp++]=';';
            break;
        }
        if (t->kind == TK_PUNCT && t->val[0]=='(') depth++;
        int vl = strlen(t->val);
        if (bp + vl + 2 < (int)sizeof buf2) {
            memcpy(buf2+bp, t->val, vl); bp+=vl; buf2[bp++]=' ';
        }
        tok_advance();
    }
    buf2[bp] = 0;
    return leaf(N_DECL, buf2);
}

static Node *parse_block(void) {
    tok_expect(TK_PUNCT, "{");
    Node *b = list_new();
    b->kind = N_BLOCK;
    while (!tok_is("}") && tok_cur()->kind != TK_EOF)
        nadd(b, parse_stmt());
    tok_expect(TK_PUNCT, "}");
    return b;
}

static Node *parse_stmt(void) {
    Token *t = tok_cur();

    /* ace $replace / $replaceafter */
    if (t->kind == TK_REPLACE || t->kind == TK_REPLACEAFTER) {
        int after = (t->kind == TK_REPLACEAFTER);
        tok_advance();
        Node *pat = parse_expr();
        tok_expect(TK_PUNCT, ";");
        tok_expect(TK_WITH, "$with");
        Node *rep = parse_expr();
        tok_expect(TK_PUNCT, ";");
        Node *r = node_new(N_RULE);
        r->ival = after;
        nadd(r, pat); nadd(r, rep);
        return rewrite(r);  /* register immediately */
    }

    /* $defprefix($name, $fn); */
    if (t->kind == TK_DEFPREFIX) {
        tok_advance();
        tok_expect(TK_PUNCT, "(");
        char *nm = strdup(tok_cur()->val); tok_advance();
        tok_expect(TK_PUNCT, ",");
        char *fn = strdup(tok_cur()->val); tok_advance();
        tok_expect(TK_PUNCT, ")");
        tok_expect(TK_PUNCT, ";");
        pfx_add(nm, fn);
        free(nm); free(fn);
        return leaf(N_EMPTY,"");
    }

    /* $trips(n) stmt */
    if (t->kind == TK_TRIPS) {
        tok_advance();
        tok_expect(TK_PUNCT,"(");
        Node *cnt = parse_expr();
        tok_expect(TK_PUNCT,")");
        Node *body = parse_stmt();
        Node *r = node_new(N_TRIPS);
        nadd(r,cnt); nadd(r,body);
        return r;
    }

    /* ace prefix macro: $ident(args) stmt */
    if (t->kind == TK_ACE_IDENT) {
        const char *fn = pfx_find(t->val);
        char *nm = strdup(t->val);
        tok_advance();
        if (tok_is("(")) {
            tok_advance();
            Node *args = list_new();
            while (!tok_is(")") && tok_cur()->kind != TK_EOF) {
                nadd(args, parse_assign());
                if (tok_is(",")) tok_advance();
            }
            tok_expect(TK_PUNCT,")");
            /* If there's a following statement (not ; ), it becomes last arg */
            if (!tok_is(";") && !tok_is("}") && tok_cur()->kind != TK_EOF) {
                nadd(args, parse_stmt());
            } else {
                tok_try(";");
            }
            /* Build call node using the resolved function name */
            Node *call = node_new(N_CALL);
            call->sval = fn ? strdup(fn) : nm;
            if (!fn) free(nm);
            nadd(call, args);
            return rewrite(call);
        }
        /* not a call – treat as expression-statement */
        /* re-parse as expression */
        tpos--; /* back up */
        free(nm);
    }

    /* block */
    if (tok_is("{")) return parse_block();

    /* if */
    if (t->kind == TK_IF) {
        tok_advance();
        tok_expect(TK_PUNCT,"(");
        Node *cond = parse_expr();
        tok_expect(TK_PUNCT,")");
        Node *then = parse_stmt();
        Node *n = node_new(N_IF);
        nadd(n,cond); nadd(n,then);
        if (tok_cur()->kind == TK_ELSE) {
            tok_advance();
            nadd(n, parse_stmt());
        }
        return n;
    }

    /* for */
    if (t->kind == TK_FOR) {
        tok_advance();
        tok_expect(TK_PUNCT,"(");
        Node *init = tok_is(";") ? leaf(N_EMPTY,"") : parse_expr();
        tok_expect(TK_PUNCT,";");
        Node *cond = tok_is(";") ? leaf(N_EMPTY,"") : parse_expr();
        tok_expect(TK_PUNCT,";");
        Node *upd  = tok_is(")") ? leaf(N_EMPTY,"") : parse_expr();
        tok_expect(TK_PUNCT,")");
        Node *body = parse_stmt();
        Node *n = node_new(N_FOR);
        nadd(n,init); nadd(n,cond); nadd(n,upd); nadd(n,body);
        return n;
    }

    /* while */
    if (t->kind == TK_WHILE) {
        tok_advance();
        tok_expect(TK_PUNCT,"(");
        Node *cond = parse_expr();
        tok_expect(TK_PUNCT,")");
        Node *body = parse_stmt();
        Node *n = node_new(N_WHILE);
        nadd(n,cond); nadd(n,body);
        return n;
    }

    /* do */
    if (t->kind == TK_DO) {
        tok_advance();
        Node *body = parse_stmt();
        tok_expect(TK_WHILE,"while");
        tok_expect(TK_PUNCT,"(");
        Node *cond = parse_expr();
        tok_expect(TK_PUNCT,")");
        tok_expect(TK_PUNCT,";");
        Node *n = node_new(N_DO);
        nadd(n,body); nadd(n,cond);
        return n;
    }

    /* return */
    if (t->kind == TK_RETURN) {
        tok_advance();
        Node *n = node_new(N_RETURN);
        if (!tok_is(";")) nadd(n, parse_expr());
        tok_expect(TK_PUNCT,";");
        return n;
    }

    /* break / continue */
    if (t->kind == TK_BREAK)    { tok_advance(); tok_try(";"); return node_new(N_BREAK); }
    if (t->kind == TK_CONTINUE) { tok_advance(); tok_try(";"); return node_new(N_CONTINUE); }

    /* goto */
    if (t->kind == TK_GOTO) {
        tok_advance();
        Node *n = node_new(N_GOTO);
        n->sval = strdup(tok_cur()->val); tok_advance();
        tok_try(";");
        return n;
    }

    /* switch */
    if (t->kind == TK_SWITCH) {
        tok_advance();
        tok_expect(TK_PUNCT,"(");
        Node *cond = parse_expr();
        tok_expect(TK_PUNCT,")");
        Node *body = parse_stmt();
        Node *n = node_new(N_SWITCH);
        nadd(n,cond); nadd(n,body);
        return n;
    }

    /* case */
    if (t->kind == TK_CASE) {
        tok_advance();
        Node *e = parse_expr();
        tok_expect(TK_PUNCT,":");
        Node *n = node_new(N_CASE);
        nadd(n,e);
        return n;
    }

    /* default */
    if (t->kind == TK_DEFAULT) {
        tok_advance();
        tok_expect(TK_PUNCT,":");
        return node_new(N_DEFAULT);
    }

    /* label: ident followed by ':' */
    if (t->kind == TK_IDENT && tok_peek(1)->kind == TK_PUNCT &&
        tok_peek(1)->val[0] == ':') {
        Node *n = node_new(N_LABEL);
        n->sval = strdup(t->val);
        tok_advance(); tok_advance(); /* ident : */
        return n;
    }

    /* empty statement */
    if (tok_is(";")) { tok_advance(); return leaf(N_EMPTY,""); }

    /* declaration */
    if (is_decl_start()) return parse_decl();

    /* expression statement */
    Node *e = parse_expr();
    tok_try(";");
    Node *es = node_new(N_EXPR_STMT);
    nadd(es,e);
    return es;
}

/* ---- Expression parser (recursive descent, standard C precedence) ---- */

static Node *parse_primary(void) {
    Token *t = tok_cur();

    /* parenthesised expression or cast */
    if (tok_is("(")) {
        tok_advance();
        /* cast: (type) expr */
        if (is_typespec()) {
            /* collect type text */
            char tbuf[512]; int tp2=0;
            while (!tok_is(")") && tok_cur()->kind != TK_EOF) {
                if (tp2 < (int)sizeof tbuf-2) {
                    memcpy(tbuf+tp2, tok_cur()->val, strlen(tok_cur()->val));
                    tp2 += strlen(tok_cur()->val);
                    tbuf[tp2++] = ' ';
                }
                tok_advance();
            }
            tbuf[tp2] = 0;
            tok_expect(TK_PUNCT,")");
            Node *e = parse_assign();
            Node *n = node_new(N_CAST);
            n->sval = strdup(tbuf);
            nadd(n,e);
            return n;
        }
        Node *e = parse_expr();
        tok_expect(TK_PUNCT,")");
        return e;
    }

    /* $LET(…) */
    if (t->kind == TK_LET) {
        tok_advance();
        tok_expect(TK_PUNCT,"(");
        Node *args = list_new();
        while (!tok_is(")") && tok_cur()->kind != TK_EOF) {
            nadd(args, parse_assign());
            if (tok_is(",")) tok_advance();
        }
        tok_expect(TK_PUNCT,")");
        Node *n = node_new(N_LET);
        nadd(n,args);
        return rewrite(n);
    }

    /* $tradeoff(time_expr, space_expr) */
    if (t->kind == TK_TRADEOFF) {
        tok_advance();
        tok_expect(TK_PUNCT,"(");
        Node *te = parse_assign();
        tok_expect(TK_PUNCT,",");
        Node *se = parse_assign();
        tok_expect(TK_PUNCT,")");
        /* default: pick time (first) */
        (void)se;
        return te;
    }

    /* $P(expr) – probability annotation, strip it */
    if (t->kind == TK_PROB) {
        tok_advance();
        tok_expect(TK_PUNCT,"(");
        Node *e = parse_expr();
        tok_expect(TK_PUNCT,")");
        return e;
    }

    /* meta-variables */
    if (t->kind == TK_META)      { Node *n=ival_node(N_METAVAR,      t->ival); tok_advance(); return n; }
    if (t->kind == TK_META_FREE) { Node *n=ival_node(N_METAVAR_FREE, t->ival); tok_advance(); return n; }

    /* sizeof */
    if (t->kind == TK_SIZEOF) {
        tok_advance();
        if (tok_is("(") && is_typespec()) {
            tok_advance();
            char tbuf[256]; int tp2=0;
            while (!tok_is(")") && tok_cur()->kind != TK_EOF) {
                memcpy(tbuf+tp2,tok_cur()->val,strlen(tok_cur()->val));
                tp2+=strlen(tok_cur()->val); tbuf[tp2++]=' '; tok_advance();
            }
            tbuf[tp2]=0;
            tok_expect(TK_PUNCT,")");
            Node *n=node_new(N_SIZEOF_TYPE); n->sval=strdup(tbuf); return n;
        }
        Node *e = parse_assign();
        return unary(N_UNARY_PRE,"sizeof",e);
    }

    /* literals */
    if (t->kind == TK_NUM)  { Node *n=leaf(N_NUM,  t->val); tok_advance(); return n; }
    if (t->kind == TK_STR)  { Node *n=leaf(N_STR,  t->val); tok_advance(); return n; }
    if (t->kind == TK_CHAR) { Node *n=leaf(N_STR,  t->val); tok_advance(); return n; }

    /* identifier or function call */
    if (t->kind == TK_IDENT || t->kind == TK_ACE_IDENT ||
        t->kind == TK_TYPESPEC) {
        Node *id = leaf(N_IDENT, t->val); tok_advance();
        return id;
    }

    fprintf(stderr,"ace: parse error at line %d: unexpected '%s'\n",
            t->line, t->val);
    tok_advance();
    return leaf(N_NUM,"0");
}

static Node *parse_postfix(void) {
    Node *e = parse_primary();
    while (1) {
        if (tok_is("++")) { e=unary(N_UNARY_POST,"++",e); tok_advance(); }
        else if (tok_is("--")) { e=unary(N_UNARY_POST,"--",e); tok_advance(); }
        else if (tok_is("[")) {
            tok_advance();
            Node *idx = parse_expr();
            tok_expect(TK_PUNCT,"]");
            Node *n=node_new(N_INDEX); nadd(n,e); nadd(n,idx); e=n;
        }
        else if (tok_is(".")) {
            tok_advance();
            Node *n=node_new(N_MEMBER_DOT);
            n->sval = strdup(tok_cur()->val); tok_advance();
            nadd(n,e); e=n;
        }
        else if (tok_cur()->kind == TK_ARROW) {
            tok_advance();
            Node *n=node_new(N_MEMBER_ARROW);
            n->sval = strdup(tok_cur()->val); tok_advance();
            nadd(n,e); e=n;
        }
        else if (tok_is("(")) {
            /* function call – e must be N_IDENT */
            tok_advance();
            Node *args = list_new();
            while (!tok_is(")") && tok_cur()->kind != TK_EOF) {
                nadd(args, parse_assign());
                if (tok_is(",")) tok_advance();
            }
            tok_expect(TK_PUNCT,")");
            Node *call = node_new(N_CALL);
            call->sval = e->sval ? strdup(e->sval) : strdup("?");
            nadd(call, args);
            e = call;
        }
        else break;
    }
    return e;
}

static Node *parse_unary(void) {
    Token *t = tok_cur();
    if (tok_cur()->kind == TK_INC) { tok_advance(); return unary(N_UNARY_PRE,"++",parse_unary()); }
    if (tok_cur()->kind == TK_DEC) { tok_advance(); return unary(N_UNARY_PRE,"--",parse_unary()); }
    if (tok_is("&")) { tok_advance(); return unary(N_UNARY_PRE,"&",parse_unary()); }
    if (tok_is("*")) { tok_advance(); return unary(N_UNARY_PRE,"*",parse_unary()); }
    if (tok_is("+")) { tok_advance(); return unary(N_UNARY_PRE,"+",parse_unary()); }
    if (tok_is("-")) { tok_advance(); return unary(N_UNARY_PRE,"-",parse_unary()); }
    if (tok_is("~")) { tok_advance(); return unary(N_UNARY_PRE,"~",parse_unary()); }
    if (tok_is("!")) { tok_advance(); return unary(N_UNARY_PRE,"!",parse_unary()); }
    (void)t;
    return parse_postfix();
}

#define BINARY_LEVEL(name, next, ...) \
static Node *name(void) { \
    Node *l = next(); \
    while (1) { \
        const char *ops[] = {__VA_ARGS__, NULL}; \
        int matched = 0; \
        for (int i=0; ops[i]; i++) { \
            if (tok_is(ops[i]) || \
                (strlen(ops[i])>1 && strcmp(tok_cur()->val,ops[i])==0)) { \
                char *op = strdup(tok_cur()->val); tok_advance(); \
                Node *r = next(); \
                l = binop(op, l, r); free(op); matched=1; break; \
            } \
        } \
        if (!matched) break; \
    } \
    return l; \
}

BINARY_LEVEL(parse_mul,    parse_unary,  "*", "/", "%")
BINARY_LEVEL(parse_add,    parse_mul,    "+", "-")

static Node *parse_shift(void) {
    Node *l = parse_add();
    while (tok_cur()->kind==TK_LSHIFT || tok_cur()->kind==TK_RSHIFT) {
        char *op = strdup(tok_cur()->val); tok_advance();
        l = binop(op, l, parse_add()); free(op);
    }
    return l;
}

static Node *parse_rel(void) {
    Node *l = parse_shift();
    while (tok_is("<")||tok_is(">")||
           tok_cur()->kind==TK_LEQ||tok_cur()->kind==TK_GEQ) {
        char *op = strdup(tok_cur()->val); tok_advance();
        l = binop(op, l, parse_shift()); free(op);
    }
    return l;
}

static Node *parse_eq(void) {
    Node *l = parse_rel();
    while (tok_cur()->kind==TK_EQ||tok_cur()->kind==TK_NEQ) {
        char *op = strdup(tok_cur()->val); tok_advance();
        l = binop(op, l, parse_rel()); free(op);
    }
    return l;
}

BINARY_LEVEL(parse_bitand, parse_eq,     "&")
BINARY_LEVEL(parse_bitxor, parse_bitand, "^")
BINARY_LEVEL(parse_bitor,  parse_bitxor, "|")

static Node *parse_logand(void) {
    Node *l = parse_bitor();
    while (tok_cur()->kind==TK_AND) {
        tok_advance(); l=binop("&&",l,parse_bitor());
    }
    return l;
}
static Node *parse_logor(void) {
    Node *l = parse_logand();
    while (tok_cur()->kind==TK_OR) {
        tok_advance(); l=binop("||",l,parse_logand());
    }
    return l;
}

static Node *parse_ternary(void) {
    Node *c = parse_logor();
    if (tok_is("?")) {
        tok_advance();
        Node *t2 = parse_expr();
        tok_expect(TK_PUNCT,":");
        Node *e2 = parse_ternary();
        Node *n = node_new(N_TERNARY);
        nadd(n,c); nadd(n,t2); nadd(n,e2);
        return n;
    }
    return c;
}

static Node *parse_assign(void) {
    Node *l = parse_ternary();
    static const char *aops[] = {
        "=","+=","-=","*=","/=","%=","&=","|=","^=","<<=",">>=",NULL
    };
    for (int i=0; aops[i]; i++) {
        if (strcmp(tok_cur()->val, aops[i])==0) {
            char *op = strdup(tok_cur()->val); tok_advance();
            Node *r = parse_assign();
            Node *n = binop(op, l, r); free(op);
            return n;
        }
    }
    return l;
}

static Node *parse_expr(void) {
    Node *l = parse_assign();
    while (tok_is(",")) {
        tok_advance();
        l = binop(",", l, parse_assign());
    }
    return l;
}


/* ============================================================
 *  PRETTY PRINTER
 * ============================================================ */

static int indent = 0;
static void pi(void) { for(int i=0;i<indent;i++) printf("    "); }

static int op_prec(const char *op) {
    if (!op) return 99;
    if (!strcmp(op,","))  return 1;
    /* assignment ops */
    const char *aops[] = {"=","+=","-=","*=","/=","%=","&=","|=","^=","<<=",">>=",NULL};
    for(int i=0;aops[i];i++) if(!strcmp(op,aops[i])) return 2;
    if (!strcmp(op,"?:")) return 3;
    if (!strcmp(op,"||")) return 4;
    if (!strcmp(op,"&&")) return 5;
    if (!strcmp(op,"|"))  return 6;
    if (!strcmp(op,"^"))  return 7;
    if (!strcmp(op,"&"))  return 8;
    if (!strcmp(op,"==") || !strcmp(op,"!=")) return 9;
    if (!strcmp(op,"<") || !strcmp(op,">") ||
        !strcmp(op,"<=") || !strcmp(op,">=")) return 10;
    if (!strcmp(op,"<<") || !strcmp(op,">>")) return 11;
    if (!strcmp(op,"+")  || !strcmp(op,"-")) return 12;
    if (!strcmp(op,"*")  || !strcmp(op,"/") || !strcmp(op,"%")) return 13;
    return 15;
}

static void print_expr(Node *n, int pprec);

static void print_args(Node *list) {
    if (!list) return;
    for (int i=0;i<list->nch;i++) {
        if (i) printf(", ");
        print_expr(list->ch[i], 2);
    }
}

static void print_expr(Node *n, int pprec) {
    if (!n) return;
    switch (n->kind) {
    case N_NUM:          printf("%s", n->sval); break;
    case N_STR:          printf("%s", n->sval); break;
    case N_IDENT:        printf("%s", n->sval); break;
    case N_METAVAR:      printf("$%d", n->ival); break;
    case N_METAVAR_FREE: printf("$f%d", n->ival); break;
    case N_EMPTY:        break;

    case N_BINOP: {
        int p = op_prec(n->sval);
        if (p < pprec) printf("(");
        print_expr(n->ch[0], p);
        printf(" %s ", n->sval);
        print_expr(n->ch[1], p+1);
        if (p < pprec) printf(")");
        break;
    }
    case N_UNARY_PRE:
        if (!strcmp(n->sval,"sizeof")) {
            printf("sizeof("); print_expr(n->ch[0],1); printf(")");
        } else {
            printf("%s", n->sval);
            print_expr(n->ch[0], 14);
        }
        break;
    case N_UNARY_POST:
        print_expr(n->ch[0], 14);
        printf("%s", n->sval);
        break;
    case N_TERNARY:
        if (pprec > 3) printf("(");
        print_expr(n->ch[0], 4); printf(" ? ");
        print_expr(n->ch[1], 3); printf(" : ");
        print_expr(n->ch[2], 3);
        if (pprec > 3) printf(")");
        break;
    case N_CALL:
        printf("%s(", n->sval ? n->sval : "?");
        if (n->nch > 0) print_args(n->ch[0]);
        printf(")");
        break;
    case N_CAST:
        printf("(%s)", n->sval);
        print_expr(n->ch[0], 14);
        break;
    case N_INDEX:
        print_expr(n->ch[0], 15); printf("["); print_expr(n->ch[1],1); printf("]");
        break;
    case N_MEMBER_DOT:
        print_expr(n->ch[0], 15); printf(".%s", n->sval); break;
    case N_MEMBER_ARROW:
        print_expr(n->ch[0], 15); printf("->%s", n->sval); break;
    case N_SIZEOF_TYPE:
        printf("sizeof(%s)", n->sval); break;
    default:
        printf("/* expr? */"); break;
    }
}

static void print_stmt(Node *n) {
    if (!n) return;
    switch (n->kind) {
    case N_EMPTY: break;
    case N_DECL:
        pi(); printf("%s\n", n->sval); break;
    case N_EXPR_STMT:
        pi(); print_expr(n->ch[0],1); printf(";\n"); break;
    case N_BLOCK:
        pi(); printf("{\n");
        indent++;
        for(int i=0;i<n->nch;i++) print_stmt(n->ch[i]);
        indent--;
        pi(); printf("}\n");
        break;
    case N_IF:
        pi(); printf("if ("); print_expr(n->ch[0],1); printf(")\n");
        indent++; print_stmt(n->ch[1]); indent--;
        if (n->nch >= 3 && n->ch[2]->kind != N_EMPTY) {
            pi(); printf("else\n");
            indent++; print_stmt(n->ch[2]); indent--;
        }
        break;
    case N_FOR:
        pi(); printf("for (");
        if (n->ch[0]->kind != N_EMPTY) print_expr(n->ch[0],1);
        printf("; ");
        if (n->ch[1]->kind != N_EMPTY) print_expr(n->ch[1],1);
        printf("; ");
        if (n->ch[2]->kind != N_EMPTY) print_expr(n->ch[2],1);
        printf(")\n");
        indent++; print_stmt(n->ch[3]); indent--;
        break;
    case N_WHILE:
        pi(); printf("while ("); print_expr(n->ch[0],1); printf(")\n");
        indent++; print_stmt(n->ch[1]); indent--;
        break;
    case N_DO:
        pi(); printf("do\n");
        indent++; print_stmt(n->ch[0]); indent--;
        pi(); printf("while ("); print_expr(n->ch[1],1); printf(");\n");
        break;
    case N_RETURN:
        pi(); printf("return");
        if (n->nch > 0 && n->ch[0]->kind != N_EMPTY) {
            printf(" "); print_expr(n->ch[0],1);
        }
        printf(";\n");
        break;
    case N_BREAK:
        pi(); printf("break;\n"); break;
    case N_CONTINUE:
        pi(); printf("continue;\n"); break;
    case N_GOTO:
        pi(); printf("goto %s;\n", n->sval); break;
    case N_SWITCH:
        pi(); printf("switch ("); print_expr(n->ch[0],1); printf(")\n");
        print_stmt(n->ch[1]);
        break;
    case N_CASE:
        indent--;
        pi(); printf("case "); print_expr(n->ch[0],1); printf(":\n");
        indent++;
        break;
    case N_DEFAULT:
        indent--;
        pi(); printf("default:\n");
        indent++;
        break;
    case N_LABEL:
        indent--;
        pi(); printf("%s:\n", n->sval);
        indent++;
        break;
    case N_TRIPS:
        /* strip annotation, just emit the body */
        print_stmt(n->ch[1]);
        break;
    case N_LIST:
        for(int i=0;i<n->nch;i++) print_stmt(n->ch[i]);
        break;
    case N_RULE: case N_DEFPREFIX:
        break;  /* consumed */
    default:
        pi(); printf("/* stmt node %d */\n", n->kind); break;
    }
}


/* ============================================================
 *  TOP-LEVEL PARSE: handle function definitions + global decls
 * ============================================================ */

/* Check if we're at a function definition:
   [typespec*] [*] ident '(' ... ')' [param-decls] '{' */
static int at_function_def(void) {
    if (!is_typespec() && tok_cur()->kind != TK_IDENT) return 0;
    int save = tpos;
    /* skip return type */
    while (is_typespec() || tok_cur()->kind == TK_IDENT) {
        tok_advance();
        if (tok_is("*")) while(tok_is("*")) tok_advance();
        /* look for ident ( */
        if (tok_cur()->kind == TK_IDENT && tok_peek(1)->kind == TK_PUNCT &&
            tok_peek(1)->val[0] == '(') {
            tok_advance(); tok_advance(); /* ident ( */
            /* scan to matching ) */
            int depth=1;
            while (depth>0 && tok_cur()->kind!=TK_EOF) {
                if (tok_is("(")) depth++;
                else if (tok_is(")")) depth--;
                tok_advance();
            }
            /* skip old-style param decls */
            while (!tok_is("{") && !tok_is(";") && tok_cur()->kind!=TK_EOF)
                tok_advance();
            int r = tok_is("{");
            tpos = save;
            return r;
        }
        break;
    }
    tpos = save;
    return 0;
}

static void parse_and_emit_function(void) {
    /* Collect return type */
    char sig[2048]; int sp=0;
    while (is_typespec() || (tok_cur()->kind==TK_IDENT && !at_function_def())) {
        int vl = strlen(tok_cur()->val);
        if (sp+vl+2<(int)sizeof sig) {
            memcpy(sig+sp,tok_cur()->val,vl); sp+=vl; sig[sp++]=' ';
        }
        tok_advance();
        while (tok_is("*")) { sig[sp++]='*'; sig[sp++]=' '; tok_advance(); }
        /* stop when we see ident ( */
        if (tok_cur()->kind==TK_IDENT && tok_peek(1)->kind==TK_PUNCT &&
            tok_peek(1)->val[0]=='(') break;
    }
    /* function name */
    if (sp+strlen(tok_cur()->val)+2<sizeof sig) {
        int vl=strlen(tok_cur()->val);
        memcpy(sig+sp,tok_cur()->val,vl); sp+=vl; sig[sp++]=' ';
    }
    tok_advance();
    /* parameter list */
    sig[sp++]='('; sig[sp]=0;
    tok_advance(); /* ( */
    int depth=1;
    while (depth>0 && tok_cur()->kind!=TK_EOF) {
        const char *v=tok_cur()->val;
        if (tok_is("(")) depth++;
        else if (tok_is(")")) { depth--; if(depth==0) { tok_advance(); break; } }
        if (sp+(int)strlen(v)+2<(int)sizeof sig) {
            memcpy(sig+sp,v,strlen(v)); sp+=strlen(v); sig[sp++]=' ';
        }
        tok_advance();
    }
    if (sp < (int)sizeof sig-2) sig[sp++]=')';
    sig[sp]=0;

    /* old-style parameter declarations */
    char olddecls[2048]; int odp=0;
    while (!tok_is("{") && !tok_is(";") && tok_cur()->kind!=TK_EOF) {
        const char *v=tok_cur()->val;
        if (odp+(int)strlen(v)+2<(int)sizeof olddecls) {
            memcpy(olddecls+odp,v,strlen(v)); odp+=strlen(v); olddecls[odp++]=' ';
        }
        tok_advance();
    }
    olddecls[odp]=0;

    if (tok_is(";")) { /* declaration, not definition */
        tok_advance();
        printf("%s%s;\n", sig, odp?olddecls:"");
        return;
    }

    /* Print signature */
    printf("%s%s\n", sig, odp?olddecls:"");

    /* Parse and rewrite body */
    Node *body = parse_block();
    Node *rb = rewrite(body);
    print_stmt(rb);
    printf("\n");
}

static char *read_file(const char *path) {
    FILE *f = path ? fopen(path,"r") : stdin;
    if (!f) { perror(path); exit(1); }
    size_t cap=65536, len=0;
    char *buf2 = malloc(cap);
    int c;
    while ((c=fgetc(f)) != EOF) {
        if (len+2>=cap) { cap*=2; buf2=realloc(buf2,cap); }
        buf2[len++]=c;
    }
    buf2[len]=0;
    if (path) fclose(f);
    return buf2;
}

int main(int argc, char **argv) {
    const char *path = argc>1 ? argv[1] : NULL;
    char *source = read_file(path);

    /* tokenise */
    lex_all(source);
    free(source);

    /* Add EOF sentinel */
    if (ntoks == 0 || toks[ntoks-1].kind != TK_EOF)
        add_tok(TK_EOF,"<EOF>",0,0);

    /* Parse and emit */
    while (tok_cur()->kind != TK_EOF) {
        Token *t = tok_cur();

        /* Preprocessor lines pass through */
        if (t->kind == TK_PP) {
            printf("%s\n", t->val);
            tok_advance();
            continue;
        }

        /* ace $replace / $replaceafter directives at top level */
        if (t->kind == TK_REPLACE || t->kind == TK_REPLACEAFTER) {
            Node *r = parse_stmt();
            (void)r;
            continue;
        }

        /* ace $defprefix */
        if (t->kind == TK_DEFPREFIX) {
            Node *r = parse_stmt();
            (void)r;
            continue;
        }

        /* function definition */
        if (at_function_def()) {
            parse_and_emit_function();
            continue;
        }

        /* global declaration */
        if (is_decl_start() || is_typespec()) {
            Node *d = parse_decl();
            print_stmt(d);
            continue;
        }

        /* top-level statement (e.g. ace prefix calls) */
        Node *s = parse_stmt();
        Node *r = rewrite(s);
        print_stmt(r);
    }

    return 0;
}
