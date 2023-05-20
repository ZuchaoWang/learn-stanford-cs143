/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

extern IdTable idtable;
extern IntTable inttable;
extern StrTable stringtable;

int comment2_nesting = 0;

int make_error(char* msg);
%}

%x COMMENT2
%x STRING

/*
 * Define names for regular expressions here.
 */

ASSIGN              <-
DARROW              =>
LE                  <=

SPACE               [ \t\f\v\r]+
COMMENT1            --.*
COMMENT2START       "(*"
COMMENT2END         "*)"

CLASS               [Cc][Ll][Aa][Ss][Ss]
ELSE                [Ee][Ll][Ss][Ee]
FI                  [Ff][Ii]
IF                  [Ii][Ff]
IN                  [Ii][Nn]
INHERITS            [Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]
LET                 [Ll][Ee][Tt]
LOOP                [Ll][Oo][Oo][Pp]
POOL                [Pp][Oo][Oo][Ll]
THEN                [Tt][Hh][Ee][Nn]
WHILE               [Ww][Hh][Ii][Ll][Ee]
CASE                [Cc][Aa][Ss][Ee]
ESAC                [Ee][Ss][Aa][Cc]
OF                  [Oo][Ff]
NEW                 [Nn][Ee][Ww]
ISVOID              [Ii][Ss][Vv][Oo][Ii][Dd]
NOT                 [Nn][Oo][Tt]

TRUE                t[Rr][Uu][Ee]
FALSE               f[Aa][Ll][Ss][Ee]

INT_CONST           [0-9]+
OBJECTID            [a-z][a-zA-Z0-9_]*
TYPEID              [A-Z][a-zA-Z0-9_]*

%%

 /*
  *  Space
  */

{SPACE}             {}
\n                  { curr_lineno++; }


 /*
  *  Single-line comment
  */

{COMMENT1}          {}

 /*
  *  Nested comments
  */

{COMMENT2START}     { comment2_nesting = 1; BEGIN(COMMENT2); }
{COMMENT2END}       { return make_error("Unmatched *)"); }

<COMMENT2>{
  {COMMENT2START}   { ++comment2_nesting; }
  {COMMENT2END}     { if (--comment2_nesting == 0) BEGIN(INITIAL); }
  .                 {}
  \n                { curr_lineno++; }
  <<EOF>>           { BEGIN(INITIAL); return make_error("EOF in comment"); }
}

 /*
  *  The single-character operators.
  */

"+"                 { return '+'; }
"-"                 { return '-'; }
"*"                 { return '*'; }
"/"                 { return '/'; }
"<"                 { return '<'; }
"="                 { return '='; }
"~"                 { return '~'; }
","                 { return ','; }
"."                 { return '.'; }
"@"                 { return '@'; }
"{"                 { return '{'; }
"}"                 { return '}'; }
"("                 { return '('; }
")"                 { return ')'; }
":"                 { return ':'; }
";"                 { return ';'; }

 /*
  *  The multiple-character operators.
  */
{ASSIGN}            { return (ASSIGN); }
{DARROW}            { return (DARROW); }
{LE}                { return (LE); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS}             { return (CLASS); }
{ELSE}              { return (ELSE); }
{FI}                { return (FI); }
{IF}                { return (IF); }
{IN}                { return (IN); }
{INHERITS}          { return (INHERITS); }
{LET}               { return (LET); }
{LOOP}              { return (LOOP); }
{POOL}              { return (POOL); }
{THEN}              { return (THEN); }
{WHILE}             { return (WHILE); }
{CASE}              { return (CASE); }
{ESAC}              { return (ESAC); }
{OF}                { return (OF); }
{NEW}               { return (NEW); }
{ISVOID}            { return (ISVOID); }
{NOT}               { return (NOT); }

{TRUE}              { yylval.boolean = true; return (BOOL_CONST); }
{FALSE}             { yylval.boolean = false; return (BOOL_CONST); }

 /*
  *  Indetifiers must be after keywords
  */

{OBJECTID}          {
  Symbol s = idtable.add_string(yytext, yyleng);
  yylval.symbol = s;
  return (OBJECTID);
}

{TYPEID}            {
  Symbol s = idtable.add_string(yytext, yyleng);
  yylval.symbol = s;
  return (TYPEID);
}

 /*
  *  Non-negative Integer constants
  */

{INT_CONST}         {
  Symbol s = inttable.add_string(yytext, yyleng);
  yylval.symbol = s;
  return (INT_CONST);
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\"                  { string_buf_ptr = string_buf; BEGIN(STRING); }
<STRING>{
  [^\\\"\n\0]       {
    *string_buf_ptr = yytext[0];
    string_buf_ptr++;
    if (string_buf_ptr - string_buf == MAX_STR_CONST) return make_error("String constant too long");
  }
  \\.               {
    switch(yytext[1]) {
      case 'n': *string_buf_ptr = '\n'; break;
      case 't': *string_buf_ptr = '\t'; break;
      case 'b': *string_buf_ptr = '\b'; break;
      case 'f': *string_buf_ptr = '\f'; break;
      case '0': *string_buf_ptr = '\0'; break;
      default: *string_buf_ptr = yytext[1]; break;
    }
    string_buf_ptr++;
    if (string_buf_ptr - string_buf == MAX_STR_CONST) return make_error("String constant too long");
  }
  \"                {
    BEGIN(INITIAL);
    *string_buf_ptr = '\0';
    Symbol s = stringtable.add_string(string_buf, string_buf_ptr-string_buf);
    yylval.symbol = s;
    return (STR_CONST);
  }
  \n                { BEGIN(INITIAL); curr_lineno++; return make_error("Unterminated string constant"); }
  \0                { BEGIN(INITIAL); return make_error("String contains null character"); }
  <<EOF>>           { BEGIN(INITIAL); return make_error("EOF in string constant"); }
}

 /* 
  *  Unmatched characters
  */

.                   { return make_error(strdup(yytext)); }

%%

int make_error(char* msg) {
  yylval.error_msg = msg;
  return (ERROR);
}