header {
//package org.erights.e.elang.syntax.antlr;
}

//class QuasiLexer extends Lexer("antlr.AstroLexer");
class QuasiLexer extends Lexer("antlr.SwitchingLexer");
options {
    importVocab=Common;
    testLiterals=false;    // don't automatically test for literals
    k=3;                   // four characters of lookahead
    charVocabulary='\3'..'\377';
    // unicode: charVocabulary='\u0003'..'\u7FFE';
    // without inlining some bitset tests, couldn't do unicode;
    // I need to make ANTLR generate smaller bitsets; see
    // bottom of JavaLexer.java
    codeGenBitsetTestThreshold=20;
    caseSensitiveLiterals = false;
}


//QUASICLOSE: '`'  ('`' QUASIn {$setType(QUASIBODY);} | {selector.pop();})  ;

QUASIBODY:      "${"            {$setType(DOLLARCURLY); selector.push("e");}
            |   '$'! IDENT      {$setType(DOLLARHOLE);}
            |   "@{"            {$setType(ATCURLY); selector.push("e");}
            |   '@'! IDENT      {$setType(ATHOLE);}
            |   "$$" QUASIn
            |   "$\\" QUASIn
            |   "@@" QUASIn
            |   "@\\" QUASIn
            |   QUASI1 QUASIn
            |   ("``")=> "``" QUASIn  //lookahead is needed to not conflict
            |   '`' {$setType(QUASICLOSE);} {selector.pop();}
            ;

protected
QUASIn:     ( QUASI1 | "$$" | "$\\" | "@@" | "@\\" | ("``") => "``")* ;

protected
QUASI1:         ~('`'|'$'|'@'|'\r'|'\n')
            |   (options {generateAmbigWarnings=false;} :
                    "\r\n" | '\r' | '\n')   {newline();}
            ;

protected
IDENT:      ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')*  ;

protected
ESC:    '\\'
        (    'n'
        |    'r'
        |    't'
        |    'b'
        |    'f'
        |    '"'
        |    '\''
        |    '@'
        |    '$'
        |    '`'
        |    '\\'
        |    ('u')+ HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
        |    '0'..'3'
            (
                options {
                    warnWhenFollowAmbig = false;
                }
            :    '0'..'7'
                (
                    options {
                        warnWhenFollowAmbig = false;
                    }
                :    '0'..'7'
                )?
            )?
        |    '4'..'7'
            (
                options {
                    warnWhenFollowAmbig = false;
                }
            :    '0'..'7'
            )?
        )
;

// hexadecimal digit
protected
HEX_DIGIT:       ('0'..'9'|'A'..'F'|'a'..'f')  ;
