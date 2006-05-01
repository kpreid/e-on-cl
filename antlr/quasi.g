header {
//package org.erights.e.elang.syntax.antlr;
}

//class QuasiLexer extends Lexer("antlr.AstroLexer");
class QuasiLexer extends Lexer("antlr.SwitchingLexer");
options {
    importVocab=Common;
    testLiterals=false;    // don't automatically test for literals
    k=3;                   // four characters of lookahead

    // XXX where does \u0003 come from?
    // XXX should allow Unicode U+10000..U+10FFFF, but can't express that in Java
    charVocabulary='\u0003'..'\uFFFE';

    // without inlining some bitset tests, couldn't do unicode;
    // I need to make ANTLR generate smaller bitsets; see
    // bottom of JavaLexer.java
    codeGenBitsetTestThreshold=20;
    caseSensitiveLiterals = false;
}

{
    private String unicodeChar(Token s) {
        return String.valueOf((char) Integer.parseInt(s.getText(), 16));
    }
    private String octalChar(Token s) {
        return String.valueOf((char) Integer.parseInt(s.getText(), 8));
    }
}

//QUASICLOSE: '`'  ('`' QUASIn {$setType(QUASIBODY);} | {selector.pop();})  ;

QUASIBODY:      "${"            {$setType(DOLLARCURLY); selector.push("e");}
            |   '$'! IDENT_S    {$setType(DOLLARHOLE);}
            |   "@{"            {$setType(ATCURLY); selector.push("e");}
            |   '@'! IDENT_S    {$setType(ATHOLE);}
            |   '$''$'! QUASIn
            |   "$\\"! ESC QUASIn
            |   '@''@'! QUASIn
            |   "@\\"! ESC QUASIn
            |   QUASI1 QUASIn
            |   ("``")=> '`''`'! QUASIn  //lookahead is needed to not conflict
            |   '`' {$setType(QUASICLOSE);} {selector.pop();}
            ;

protected
QUASIn:     (QUASI1 | '$''$'! | "$\\"! ESC | '@''@'! | "@\\"! ESC | ("``") => '`''`'!)* ;

protected
QUASI1:         ~('`'|'$'|'@'|'\r'|'\n')
            |   EOL
            ;

// escape sequence -- note that this is protected; it can only be called
//   from another lexer rule -- it will not ever directly return a token to
//   the parser
// There are various ambiguities hushed in this rule.  The optional
// '0'...'9' digit matches should be matched here rather than letting
// them go back to STRING to be matched.  ANTLR does the
// right thing by matching immediately; hence, it's ok to shut off
// the FOLLOW ambig warnings.
protected
ESC:    (!   'n'        {$setText("\n");}
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
;

// ----------------------------------------------------------------------------
// common suffix with elex.g
// XXX figure out if we can avoid this duplication

// XXX need to extend this to cover Unicode
protected
IDENT_S:    ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')* ;

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

// XXX extend to Unicode's line/paragraph separators
protected
EOL:     (options {generateAmbigWarnings=false;} : "\r\n" | '\r'
                                                   | '\n' ) { newline(); } ;
