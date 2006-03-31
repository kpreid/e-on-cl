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
    testLiterals=false;    // don't automatically test for literals
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
    // quasi-E-source hole marker indexes
    
    static final int NO_HOLE_INDEX = -1;
    protected int[] valueHoles = {};
    protected int[] patternHoles = {};
    public void setHoles(int[] vh, int[] ph) {
      valueHoles = vh;
      patternHoles = ph;
    }

    protected int holeThere(String key, int offset) {
      int[] table = "$".equals(key) ? valueHoles : "@".equals(key) ? patternHoles : null;
      if (inputState instanceof CountingLexerSharedInputState) {
        int pos = ((CountingLexerSharedInputState)inputState).getPosition()
                  + offset;
        // XXX use a hash table or something
        for (int i = 0; i < table.length; i++) {
          if (table[i] == pos) {
            return i;
          }
        }
        return NO_HOLE_INDEX;
      } else {
        if (table.length == 0) {
          return NO_HOLE_INDEX;
        } else {
          throw new RuntimeException("E lexer provided with nonempty hole table but no CountingLexerSharedInputState; cannot proceed.");
        }
      }
    }

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
RCURLY:              '}'    {selector.exitBrace();} ;

OPERATOR options {testLiterals=true;} :
//        '`' {selector.push("quasi");}  |
       '('    BR
    |   ')'
    |   '['    BR
    |   ']'
    |   '{'    BR  {selector.enterBrace();}
//    |   '}'    {selector.exitBrace();}

    // quasi-in-E-source
    | ('$'|'@') ( {holeThere($getText, -1) != NO_HOLE_INDEX}?
                  {int i = holeThere($getText, NO_HOLE_INDEX);
                   int type = "$".equals($getText) ? SOURCE_VALUE_HOLE : 
                              "@".equals($getText) ? SOURCE_PATTERN_HOLE :
                              -1; // XXX most appropriate bad-token value?
                   $setType(type);
                   $setText(Integer.toString(i));}
                | {throw new TokenStreamException("A literal " + $getText + " is not meaningful in E source."); /*XXX most appropriate exception type?*/}
                )
    // XXX todo: everywhere a $ can appear, make sure that holeness is checked

	// a question at the beginning of a line indicates an updoc line, and the line
    // is ignored.
    |   '?'    ({isFirstInLine}? UPDOC {$setType(Token.SKIP);} | BR)
    |   ':'    BR
    |   ','    BR
    |   '.'    BR
    |   ".."   BR
    |   "..!"  BR
    |   "=="   BR
    |   '='    BR
    |   '!'    BR
    |   '~'    BR
    |   "!="   BR
    |   '/'    BR
    |   "//"   BR
    |   '+'    BR
    |   '-'    BR
    |   "++"
    |   "--"
    |   '*'    BR
    |   '%'    BR
    |   "%%"   BR
    |   "<<"   BR
    |   "<="   BR
    |   "<=>"  BR
    |   '^'    BR
    |   '|'    BR
    |   "||"   BR
    |   '&'    BR
    |   "&!"   BR
    |   "&&"   BR
    |   ';'
    |   "**"   BR
    // Assign
    |   ":="   BR
    |   "//="  BR
    |   "/="   BR
    |   "+="   BR
    |   "-="   BR
    |   "*="   BR
    |   "%="   BR
    |   "%%="  BR
    |   "**="  BR
    |   "<<="  BR
    |   "^="   BR
    |   "|="   BR
    |   "&="   BR
// Other tokens
    |   DOC_COMMENT        {$setType(DOC_COMMENT);}
    |   "<-"   BR
    |   "->"   BR
    |   "=>"   BR
    |   "=~"   BR
    |   "!~"   BR
    |   "::"   BR
    //SR:                  ">>"   BR
	//GE:                  ">="   BR
	//SR_ASSIGN       :    ">>="  BR
    |   '>'    ( // {isFirstInLine}? SKIPLINE
                              // {$setType(Token.SKIP);} |
                              '>'  BR {$setType(SR);}
                            | '='  BR {$setType(GE);}
                            | ">=" BR {$setType(SR_ASSIGN);}
                            | ) // should have BR, except for terminating a
                                 // URI
    |   ('<'  URISCHEME ('>' | ':')) =>
                      '<'! URISCHEME ( '>'! {$setType(URIGetter);}
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
protected
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
// XXX need to extend this to cover Unicode
IDENT       options {testLiterals=true;}
            :    ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')*
            ;

// the scheme component of a URI literal
// NOTE: this should not be extended to Unicode; RFC 2396 lists this set specifically.
protected
URISCHEME   options {testLiterals=true;}
            :    ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'+'|'-'|'.')*
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
