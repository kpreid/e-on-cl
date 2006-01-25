header {
//package org.erights.e.elang.syntax.antlr;
}

// UPDOC
//The E grammar specifically treats updoc test sequences as comments.  Any line
//whose first non-whitespace character is '?' is considered by the lexer to be
//the start of an UPDOC test case.  The test case will include any continguous
//lines that are whitespace, or that begin with '#' or '>'.

// TODO:
// updoc in the midst of quasi or string
// support doubled '`'
// make keywords (literals) case insensitive
// only do literal resolution for IDENT and funky punctuation

//----------------------------------------------------------------------------
// The E Lexer
//----------------------------------------------------------------------------
//class EALexer extends Lexer("antlr.AstroLexer");
class EALexer extends Lexer("antlr.SwitchingLexer");

options {
    importVocab=E;
    exportVocab=EALexer;
    //testLiterals=false;    // don't automatically test for literals
    k=3;                   // four characters of lookahead
    charVocabulary='\3'..'\377';
    //charVocabulary='\u0003'..'\u7FFE';
    // without inlining some bitset tests, couldn't do unicode;
    // I need to make ANTLR generate smaller bitsets; see
    // bottom of JavaLexer.java
    codeGenBitsetTestThreshold=20;
    caseSensitiveLiterals = false;
}
tokens {
    SR;
    GE;
    SR_ASSIGN;
}
{
    // set isFirstInLine whenever we produce a token, and reset it at the
    // beginning of every line
    protected boolean isFirstInLine = true;
    protected Token lastToken = null;
    protected Token makeToken(int t) {
        if (t != LINESEP) { isFirstInLine = false; }
        return lastToken = super.makeToken(t);
    }
    public void newline() {
        isFirstInLine = true;
        lastToken = null;
        super.newline();
    }
    //public void traceIn(String rname) throws CharStreamException {    }
    private String unicodeChar(Token s) {
        return String.valueOf((char) Integer.parseInt(s.getText(), 16));
    }
    private String octalChar(Token s) {
        return String.valueOf((char) Integer.parseInt(s.getText(), 8));
    }
}

// OPERATORS
QUASIOPEN:        '`' {selector.push("quasi");}  ;
LPAREN:              '('    BR ;
RPAREN:              ')'    ;
LBRACK:              '['    BR ;
RBRACK:              ']'    ;
LCURLY:              '{'    BR  {selector.enterBrace();} ;
RCURLY:              '}'    {selector.exitBrace();} ;
AT:                  '@'    ;
ATCURLY:             "@{"   ;
DOLLARCURLY:         "${"   ;
// a question at the beginning of a line indicates an updoc line, and the line
// is ignored.
QUESTION:            '?'    ({isFirstInLine}? UPDOC {$setType(Token.SKIP);}
                          | BR) ;
COLON:               ':'    BR ;
COMMA:               ','    BR ;
DOT:                 '.'    BR ;
THRU:                ".."   BR ;
TILL:                "..!"  BR ;
SAME:                "=="   BR ;
EQ:                  '='    BR ;
LNOT:                '!'    BR ;
BNOT:                '~'    BR ;
NOTSAME:             "!="   BR ;
DIV:                 '/'    BR ;
FLOORDIV:            "//"   BR ;
PLUS:                '+'    BR ;
MINUS:               '-'    BR ;
INC:                 "++"   ;
DEC:                 "--"   ;
STAR:                '*'    BR ;
REM:                 '%'    BR ;
MOD:                 "%%"   BR ;
SL:                  "<<"   BR ;
LE:                  "<="   BR ;
ABA:                 "<=>"  BR ;
BXOR:                '^'    BR ;
BOR:                 '|'    BR ;
LOR:                 "||"   BR ;
BAND:                '&'    BR ;
BUTNOT:              "&!"   BR ;
LAND:                "&&"   BR ;
SEMI:                ';'    ;
POW:                 "**"   BR ;

ASSIGN:              ":="   BR ;
FLOORDIV_ASSIGN:     "//="  BR ;
DIV_ASSIGN:          "/="   BR ;
PLUS_ASSIGN:         "+="   BR ;
MINUS_ASSIGN:        "-="   BR ;
STAR_ASSIGN:         "*="   BR ;
REM_ASSIGN:          "%="   BR ;
MOD_ASSIGN:          "%%="  BR ;
POW_ASSIGN:          "**="  BR ;
SL_ASSIGN:           "<<="  BR ;
BXOR_ASSIGN:         "^="   BR ;
BOR_ASSIGN:          "|="   BR ;
BAND_ASSIGN:         "&="   BR ;

// Other tokes
SEND:                "<-"   BR ;
WHEN:                "->"   BR ;
MAPSTO:              "=>"   BR ;
MATCHBIND:           "=~"   BR ;
MISMATCH:            "!~"   BR ;
SCOPE:               "::"   BR ;
SCOPESLOT:           "::&"  BR ;

//SR:                  ">>"   BR ;
//GE:                  ">="   BR ;
//SR_ASSIGN       :    ">>="  BR ;
GT:                  '>'    ( // {isFirstInLine}? SKIPLINE
                              // {$setType(Token.SKIP);} |
                              '>'  BR {$setType(SR);}
                            | '='  BR {$setType(GE);}
                            | ">=" BR {$setType(SR_ASSIGN);}
                            | ); // should have BR, except for terminating a
                                 // URI

LT:                  ('<'  IDENT ('>' | ':')) =>
                      '<'! IDENT ( '>'! {$setType(URIGetter);}
                                 | ':' (    (ANYWS)=> BR {$setType(URIStart);}
                                       |   URI '>'!  {$setType(URI);}))
                |    '<' BR ;

// Whitespace -- ignored
WS:         (' '|'\t'|'\f'|ESCWS)+    {$setType(Token.SKIP);} ;

protected
ESCWS:      '\\' (' '|'\t'|'\f')* EOL    ;

protected
ANYWS:      ' '|'\t'|'\f'|'\r'|'\n' ;

LINESEP:        (EOL)+    ;

// Single-line comments
SL_COMMENT  :   "#" (~('\n'|'\r'))*  {$setType(Token.SKIP);} ;

protected
SKIPLINE:       (~('\n'|'\r'))* EOL ;

protected
UPDOC :         (~('\n'|'\r'))* (EOL // must be optional to deal with EOF
                    (   ' '|'\t'|'\f'
                    // TODO don't include a ? line in the same updoc
                    |   ('?'|'#'|'>') (options{greedy=true;}:~('\n'|'\r'))*
                    |   EOL )* )?;

// multiple-line comments
DOC_COMMENT
    :    "/**"
        (    //    '\r' '\n' can be matched in one alternative or by matching
            //    '\r' in one iteration and '\n' in another.  I am trying to
            //    handle any flavor of newline that comes in, but the language
            //    that allows both "\r\n" and "\r" and "\n" to all be valid
            //    newline is ambiguous.  Consequently, the resulting grammar
            //    must be ambiguous.  I'm shutting this warning off.
            options {
                generateAmbigWarnings=false;
            }
        :   { LA(2)!='/' }? '*'
        |   EOL
        |   ~('*'|'\n'|'\r')
        )*
        '*' '/'
        BR
        {$setText("**comment hidden**");}  // TODO: stop suppressing comments
        //{$setType(Token.SKIP);}
    ;


// character literals
CHAR_LITERAL options {testLiterals=false;} :
        '\''! ( ESC | ~('\''|'\n'|'\r'|'\\') ) '\''!   ;

// string literals
STRING options {testLiterals=false;} :
        '"'! (   ESC
            |   EOL
            |    ~('"'|'\\'|'\n'|'\r')
        )* '"'!  ;

// escape sequence -- note that this is protected; it can only be called
//   from another lexer rule -- it will not ever directly return a token to
//   the parser
// There are various ambiguities hushed in this rule.  The optional
// '0'...'9' digit matches should be matched here rather than letting
// them go back to STRING to be matched.  ANTLR does the
// right thing by matching immediately; hence, it's ok to shut off
// the FOLLOW ambig warnings.
protected
ESC:   '\\'!
        (  (!    'n'        {$setText("\n");}
            |    'r'        {$setText("\r");}
            |    't'        {$setText("\t");}
            |    'b'        {$setText("\b");}
            |    'f'        {$setText("\f");}
            |    '"'        {$setText("\"");}
            |    '?'        {$setText("?");}
            |    '\''       {$setText("'");}
            |    '\\'       {$setText("\\");}
            )
        |!  u:ESC_UNICODE   {$setText(unicodeChar(u));}
        |   o:ESC_OCTAL     {$setText(octalChar(o));}
        |! (' '|'\t'|'\f')* EOL
        )
;

protected
ESC_UNICODE:    ('u')+! HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT ;

protected
ESC_OCTAL:  '0'..'3'
                ( options { warnWhenFollowAmbig = false; }
                :    '0'..'7'
                    (
                        options {
                            warnWhenFollowAmbig = false;
                        }
                    :    '0'..'7'
                    )?
                )?
            |   '4'..'7'
                ( options { warnWhenFollowAmbig = false; }
                :    '0'..'7'
                )?
            ;

// hexadecimal digit
protected
HEX_DIGIT   :    ('0'..'9'|'A'..'F'|'a'..'f')  ;

// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer
IDENT       options {testLiterals=true;}
            :    ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')*
            ;

// a numeric literal
INT:            ("0x") => "0x"! (HEX_DIGIT)+ { $setType(HEX); }
            |   ('0' ('0'..'9')) => '0'! ('0'..'7')+  { $setType(OCTAL); }
            |   (FLOAT64) => FLOAT64  { $setType(FLOAT64); }
            |   POSINT
            ;

// an integer
protected
POSINT:         ('0'..'9') ('0'..'9'|'_'!)* ;

protected
FLOAT64     :   POSINT ('.' POSINT | ('e' | 'E') EXPONENT)  ;

protected
EXPONENT:       ('+'|'-')? POSINT  ;

protected
BR:      (   {_saveIndex=text.length();}:)
             (' ' | '\t' | "#" (options {greedy=true;}:(~('\n'|'\r'|'#')!))*
                  | EOL
         )*
         ({text.setLength(_saveIndex);}:)
         ;

protected
EOL:     (options {generateAmbigWarnings=false;} : "\r\n" | '\r'
                                                   | '\n' ) { newline(); } ;


protected
URI:            (  'a'..'z'|'A'..'Z'|'_'|'0'..'9'
                |';'|'/'|'?'|':'|'@'|'&'|'='|'+'|'$'|','|'-'
                |'.'|'!'|'~'|'*'|'\''|'('|')'|'%'|'\\'|'|'|'#'  )+
            ;
/**/
