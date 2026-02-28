%{
/*
 * ace.l  --  flex lexer for the ace C preprocessor
 * James Gosling, "Ace: a syntax-driven C preprocessor", 1989.
 *
 * Tokenises C extended with ace's $ syntax:
 *   $replace … $with …      rewrite rule
 *   $replaceafter … $with … rewrite rule (applied post-subnode reduction)
 *   $defprefix($name, $fn)  prefix-statement macro
 *   $0…$9                   meta-variables (match any expression)
 *   $f0…$f9                 meta-variables (match side-effect-free exprs)
 *   $LET(a,v,…,body)        local substitution
 *   $trips(n) stmt          annotate loop trip count
 *   $tradeoff(t,s)          time/space trade-off selector
 *   $P(expr)                probability annotation
 */

#include "ace_grammar.tab.h"   /* bison-generated token defines */
#include <string.h>
#include <stdlib.h>

int yyline = 1;

/* helper: duplicate yytext into heap */
static char *savetext(void) { return strdup(yytext); }
%}

%option noyywrap
%option yylineno

DIGIT    [0-9]
LETTER   [a-zA-Z_]
ALNUMUS  [a-zA-Z0-9_]
HEX      [0-9a-fA-F]

%%

  /* ---- whitespace / comments ---- */
[ \t\r]+              { /* skip */ }
\n                    { yyline++; }
"/*"([^*]|\*+[^/])*\*+"/"  { /* block comment – count newlines */
                          for (char *p = yytext; *p; p++) if (*p == '\n') yyline++;
                      }
"//"[^\n]*            { /* line comment */ }

  /* ---- preprocessor pass-through ---- */
"#"[^\n]*             { yylval.str = savetext(); return PP_LINE; }

  /* ---- ace keywords ---- */
"$replace"            { return KW_REPLACE; }
"$replaceafter"       { return KW_REPLACEAFTER; }
"$with"               { return KW_WITH; }
"$defprefix"          { return KW_DEFPREFIX; }
"$LET"                { return KW_LET; }
"$let"                { return KW_LET; }
"$trips"              { return KW_TRIPS; }
"$tradeoff"           { return KW_TRADEOFF; }
"$P"                  { return KW_PROB; }
"$pullout"            { return KW_PULLOUT; }
"$assume"             { return KW_ASSUME; }

  /* ---- ace meta-variables ---- */
"$f"[0-9]             { yylval.ival = yytext[2]-'0'; return META_FREE; }
"$"[0-9]              { yylval.ival = yytext[1]-'0'; return META_VAR;  }

  /* ---- ace user-defined $identifiers (e.g. $pullout, $scanshape …) ---- */
"$"{LETTER}{ALNUMUS}* { yylval.str = savetext(); return ACE_IDENT; }

  /* ---- C keywords ---- */
"if"                  { return KW_IF; }
"else"                { return KW_ELSE; }
"for"                 { return KW_FOR; }
"while"               { return KW_WHILE; }
"do"                  { return KW_DO; }
"return"              { return KW_RETURN; }
"switch"              { return KW_SWITCH; }
"case"                { return KW_CASE; }
"default"             { return KW_DEFAULT; }
"break"               { return KW_BREAK; }
"continue"            { return KW_CONTINUE; }
"goto"                { return KW_GOTO; }
"struct"              { return KW_STRUCT; }
"union"               { return KW_UNION; }
"enum"                { return KW_ENUM; }
"typedef"             { return KW_TYPEDEF; }
"sizeof"              { return KW_SIZEOF; }

  /* storage / qualifier keywords – lump into TYPESPEC for simplicity */
"auto"|"register"|"static"|"extern"|"const"|"volatile"  {
    yylval.str = savetext(); return TYPESPEC;
}

  /* primitive types */
"void"|"char"|"short"|"int"|"long"|"float"|"double"|"unsigned"|"signed" {
    yylval.str = savetext(); return TYPESPEC;
}

  /* ---- identifiers ---- */
{LETTER}{ALNUMUS}*    { yylval.str = savetext(); return IDENT; }

  /* ---- numeric literals ---- */
0[xX]{HEX}+[uUlL]*   { yylval.str = savetext(); return NUM_LIT; }
{DIGIT}+("."?{DIGIT}*([eE][+-]?{DIGIT}+)?)?[uUlLfF]* {
                        yylval.str = savetext(); return NUM_LIT; }

  /* ---- string / char literals ---- */
\"([^\"\\]|\\.)*\"    { yylval.str = savetext(); return STR_LIT; }
\'([^\'\\]|\\.)*\'    { yylval.str = savetext(); return CHAR_LIT; }

  /* ---- multi-char operators ---- */
"<<"                  { return OP_LSHIFT; }
">>"                  { return OP_RSHIFT; }
"<="                  { return OP_LEQ; }
">="                  { return OP_GEQ; }
"=="                  { return OP_EQ; }
"!="                  { return OP_NEQ; }
"&&"                  { return OP_AND; }
"||"                  { return OP_OR; }
"++"                  { return OP_INC; }
"--"                  { return OP_DEC; }
"->"                  { return OP_ARROW; }
"..."                 { return OP_ELLIPSIS; }
"+="                  { return OP_ADDASSIGN; }
"-="                  { return OP_SUBASSIGN; }
"*="                  { return OP_MULASSIGN; }
"/="                  { return OP_DIVASSIGN; }
"%="                  { return OP_MODASSIGN; }
"&="                  { return OP_ANDASSIGN; }
"|="                  { return OP_ORASSIGN; }
"^="                  { return OP_XORASSIGN; }
"<<="                 { return OP_LSHASSIGN; }
">>="                 { return OP_RSHASSIGN; }

  /* ---- single-char tokens ---- */
"+"  { return '+'; }
"-"  { return '-'; }
"*"  { return '*'; }
"/"  { return '/'; }
"%"  { return '%'; }
"&"  { return '&'; }
"|"  { return '|'; }
"^"  { return '^'; }
"~"  { return '~'; }
"!"  { return '!'; }
"<"  { return '<'; }
">"  { return '>'; }
"="  { return '='; }
"?"  { return '?'; }
":"  { return ':'; }
","  { return ','; }
";"  { return ';'; }
"."  { return '.'; }
"("  { return '('; }
")"  { return ')'; }
"["  { return '['; }
"]"  { return ']'; }
"{"  { return '{'; }
"}"  { return '}'; }

.    { fprintf(stderr, "ace: unknown char '%s' at line %d\n", yytext, yyline); }

%%
