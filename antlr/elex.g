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

    // XXX where does \u0003 come from?
    // XXX should allow Unicode U+10000..U+10FFFF, but can't express that in Java
    charVocabulary='\u0003'..'\uFFFE';

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
    |   "^="   BR
    |   "|="   BR
    |   "&="   BR
// Other tokens
    |   DOC_COMMENT        {$setType(DOC_COMMENT);}
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
    ;

LT:   ('<'  URISCHEME ('>' | ':')) =>
                      '<'! URISCHEME ( '>'! {$setType(URIGetter);}
                                 | ':' (    (ANYWS)=> BR {$setType(URIStart);}
                                       |   URI '>'!  {$setType(URI);}))
                |    a:LT2 {$setToken(a);} ;

protected
LT2 options {testLiterals=true;} :
        "<"    BR
    |   "<<"   BR
    |   "<="   BR
    |   "<=>"  BR
    |   "<<="  BR
    |   "<-"   BR
;

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


// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer
// IDENT_S is protected; this isn't
IDENT       options {testLiterals=true;}
            :   IDENT_S ;

// the scheme component of a URI literal
// NOTE: this should not be extended to Unicode; RFC 2396 lists this set specifically.
protected
URISCHEME   options {testLiterals=false;}
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
URI:            (  'a'..'z'|'A'..'Z'|'_'|'0'..'9'
                |';'|'/'|'?'|':'|'@'|'&'|'='|'+'|'$'|','|'-'
                |'.'|'!'|'~'|'*'|'\''|'('|')'|'%'|'\\'|'|'|'#'  )+
            ;

// ----------------------------------------------------------------------------
// common suffix with elex.g
// XXX figure out if we can avoid this duplication

protected
IDENT_S     :    (XML10Letter | '_')
                 (XML10Letter | '_' | XML10Digit | XML10CombiningChar 
                  | XML10Extender)*
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

// XXX extend to Unicode's line/paragraph separators
protected
EOL:     (options {generateAmbigWarnings=false;} : "\r\n" | '\r'
                                                   | '\n' ) { newline(); } ;

// ANTLR does not provide access to Unicode character categories. For now, we'll borrow the XML 1.0 definition of identifier characters, so as not to invent something uniquely-broken.

protected
XML10Letter: XML10BaseChar | XML10Ideographic ;

protected
XML10BaseChar: '\u0041'..'\u005A' | '\u0061'..'\u007A' | '\u00C0'..'\u00D6' | '\u00D8'..'\u00F6' | '\u00F8'..'\u00FF' | '\u0100'..'\u0131' | '\u0134'..'\u013E' | '\u0141'..'\u0148' | '\u014A'..'\u017E' | '\u0180'..'\u01C3' | '\u01CD'..'\u01F0' | '\u01F4'..'\u01F5' | '\u01FA'..'\u0217' | '\u0250'..'\u02A8' | '\u02BB'..'\u02C1' | '\u0386' | '\u0388'..'\u038A' | '\u038C' | '\u038E'..'\u03A1' | '\u03A3'..'\u03CE' | '\u03D0'..'\u03D6' | '\u03DA' | '\u03DC' | '\u03DE' | '\u03E0' | '\u03E2'..'\u03F3' | '\u0401'..'\u040C' | '\u040E'..'\u044F' | '\u0451'..'\u045C' | '\u045E'..'\u0481' | '\u0490'..'\u04C4' | '\u04C7'..'\u04C8' | '\u04CB'..'\u04CC' | '\u04D0'..'\u04EB' | '\u04EE'..'\u04F5' | '\u04F8'..'\u04F9' | '\u0531'..'\u0556' | '\u0559' | '\u0561'..'\u0586' | '\u05D0'..'\u05EA' | '\u05F0'..'\u05F2' | '\u0621'..'\u063A' | '\u0641'..'\u064A' | '\u0671'..'\u06B7' | '\u06BA'..'\u06BE' | '\u06C0'..'\u06CE' | '\u06D0'..'\u06D3' | '\u06D5' | '\u06E5'..'\u06E6' | '\u0905'..'\u0939' | '\u093D' | '\u0958'..'\u0961' | '\u0985'..'\u098C' | '\u098F'..'\u0990' | '\u0993'..'\u09A8' | '\u09AA'..'\u09B0' | '\u09B2' | '\u09B6'..'\u09B9' | '\u09DC'..'\u09DD' | '\u09DF'..'\u09E1' | '\u09F0'..'\u09F1' | '\u0A05'..'\u0A0A' | '\u0A0F'..'\u0A10' | '\u0A13'..'\u0A28' | '\u0A2A'..'\u0A30' | '\u0A32'..'\u0A33' | '\u0A35'..'\u0A36' | '\u0A38'..'\u0A39' | '\u0A59'..'\u0A5C' | '\u0A5E' | '\u0A72'..'\u0A74' | '\u0A85'..'\u0A8B' | '\u0A8D' | '\u0A8F'..'\u0A91' | '\u0A93'..'\u0AA8' | '\u0AAA'..'\u0AB0' | '\u0AB2'..'\u0AB3' | '\u0AB5'..'\u0AB9' | '\u0ABD' | '\u0AE0' | '\u0B05'..'\u0B0C' | '\u0B0F'..'\u0B10' | '\u0B13'..'\u0B28' | '\u0B2A'..'\u0B30' | '\u0B32'..'\u0B33' | '\u0B36'..'\u0B39' | '\u0B3D' | '\u0B5C'..'\u0B5D' | '\u0B5F'..'\u0B61' | '\u0B85'..'\u0B8A' | '\u0B8E'..'\u0B90' | '\u0B92'..'\u0B95' | '\u0B99'..'\u0B9A' | '\u0B9C' | '\u0B9E'..'\u0B9F' | '\u0BA3'..'\u0BA4' | '\u0BA8'..'\u0BAA' | '\u0BAE'..'\u0BB5' | '\u0BB7'..'\u0BB9' | '\u0C05'..'\u0C0C' | '\u0C0E'..'\u0C10' | '\u0C12'..'\u0C28' | '\u0C2A'..'\u0C33' | '\u0C35'..'\u0C39' | '\u0C60'..'\u0C61' | '\u0C85'..'\u0C8C' | '\u0C8E'..'\u0C90' | '\u0C92'..'\u0CA8' | '\u0CAA'..'\u0CB3' | '\u0CB5'..'\u0CB9' | '\u0CDE' | '\u0CE0'..'\u0CE1' | '\u0D05'..'\u0D0C' | '\u0D0E'..'\u0D10' | '\u0D12'..'\u0D28' | '\u0D2A'..'\u0D39' | '\u0D60'..'\u0D61' | '\u0E01'..'\u0E2E' | '\u0E30' | '\u0E32'..'\u0E33' | '\u0E40'..'\u0E45' | '\u0E81'..'\u0E82' | '\u0E84' | '\u0E87'..'\u0E88' | '\u0E8A' | '\u0E8D' | '\u0E94'..'\u0E97' | '\u0E99'..'\u0E9F' | '\u0EA1'..'\u0EA3' | '\u0EA5' | '\u0EA7' | '\u0EAA'..'\u0EAB' | '\u0EAD'..'\u0EAE' | '\u0EB0' | '\u0EB2'..'\u0EB3' | '\u0EBD' | '\u0EC0'..'\u0EC4' | '\u0F40'..'\u0F47' | '\u0F49'..'\u0F69' | '\u10A0'..'\u10C5' | '\u10D0'..'\u10F6' | '\u1100' | '\u1102'..'\u1103' | '\u1105'..'\u1107' | '\u1109' | '\u110B'..'\u110C' | '\u110E'..'\u1112' | '\u113C' | '\u113E' | '\u1140' | '\u114C' | '\u114E' | '\u1150' | '\u1154'..'\u1155' | '\u1159' | '\u115F'..'\u1161' | '\u1163' | '\u1165' | '\u1167' | '\u1169' | '\u116D'..'\u116E' | '\u1172'..'\u1173' | '\u1175' | '\u119E' | '\u11A8' | '\u11AB' | '\u11AE'..'\u11AF' | '\u11B7'..'\u11B8' | '\u11BA' | '\u11BC'..'\u11C2' | '\u11EB' | '\u11F0' | '\u11F9' | '\u1E00'..'\u1E9B' | '\u1EA0'..'\u1EF9' | '\u1F00'..'\u1F15' | '\u1F18'..'\u1F1D' | '\u1F20'..'\u1F45' | '\u1F48'..'\u1F4D' | '\u1F50'..'\u1F57' | '\u1F59' | '\u1F5B' | '\u1F5D' | '\u1F5F'..'\u1F7D' | '\u1F80'..'\u1FB4' | '\u1FB6'..'\u1FBC' | '\u1FBE' | '\u1FC2'..'\u1FC4' | '\u1FC6'..'\u1FCC' | '\u1FD0'..'\u1FD3' | '\u1FD6'..'\u1FDB' | '\u1FE0'..'\u1FEC' | '\u1FF2'..'\u1FF4' | '\u1FF6'..'\u1FFC' | '\u2126' | '\u212A'..'\u212B' | '\u212E' | '\u2180'..'\u2182' | '\u3041'..'\u3094' | '\u30A1'..'\u30FA' | '\u3105'..'\u312C' | '\uAC00'..'\uD7A3' ;

protected
XML10Ideographic: '\u4E00'..'\u9FA5' | '\u3007' | '\u3021'..'\u3029' ;

protected
XML10CombiningChar: '\u0300'..'\u0345' | '\u0360'..'\u0361' | '\u0483'..'\u0486' | '\u0591'..'\u05A1' | '\u05A3'..'\u05B9' | '\u05BB'..'\u05BD' | '\u05BF' | '\u05C1'..'\u05C2' | '\u05C4' | '\u064B'..'\u0652' | '\u0670' | '\u06D6'..'\u06DC' | '\u06DD'..'\u06DF' | '\u06E0'..'\u06E4' | '\u06E7'..'\u06E8' | '\u06EA'..'\u06ED' | '\u0901'..'\u0903' | '\u093C' | '\u093E'..'\u094C' | '\u094D' | '\u0951'..'\u0954' | '\u0962'..'\u0963' | '\u0981'..'\u0983' | '\u09BC' | '\u09BE' | '\u09BF' | '\u09C0'..'\u09C4' | '\u09C7'..'\u09C8' | '\u09CB'..'\u09CD' | '\u09D7' | '\u09E2'..'\u09E3' | '\u0A02' | '\u0A3C' | '\u0A3E' | '\u0A3F' | '\u0A40'..'\u0A42' | '\u0A47'..'\u0A48' | '\u0A4B'..'\u0A4D' | '\u0A70'..'\u0A71' | '\u0A81'..'\u0A83' | '\u0ABC' | '\u0ABE'..'\u0AC5' | '\u0AC7'..'\u0AC9' | '\u0ACB'..'\u0ACD' | '\u0B01'..'\u0B03' | '\u0B3C' | '\u0B3E'..'\u0B43' | '\u0B47'..'\u0B48' | '\u0B4B'..'\u0B4D' | '\u0B56'..'\u0B57' | '\u0B82'..'\u0B83' | '\u0BBE'..'\u0BC2' | '\u0BC6'..'\u0BC8' | '\u0BCA'..'\u0BCD' | '\u0BD7' | '\u0C01'..'\u0C03' | '\u0C3E'..'\u0C44' | '\u0C46'..'\u0C48' | '\u0C4A'..'\u0C4D' | '\u0C55'..'\u0C56' | '\u0C82'..'\u0C83' | '\u0CBE'..'\u0CC4' | '\u0CC6'..'\u0CC8' | '\u0CCA'..'\u0CCD' | '\u0CD5'..'\u0CD6' | '\u0D02'..'\u0D03' | '\u0D3E'..'\u0D43' | '\u0D46'..'\u0D48' | '\u0D4A'..'\u0D4D' | '\u0D57' | '\u0E31' | '\u0E34'..'\u0E3A' | '\u0E47'..'\u0E4E' | '\u0EB1' | '\u0EB4'..'\u0EB9' | '\u0EBB'..'\u0EBC' | '\u0EC8'..'\u0ECD' | '\u0F18'..'\u0F19' | '\u0F35' | '\u0F37' | '\u0F39' | '\u0F3E' | '\u0F3F' | '\u0F71'..'\u0F84' | '\u0F86'..'\u0F8B' | '\u0F90'..'\u0F95' | '\u0F97' | '\u0F99'..'\u0FAD' | '\u0FB1'..'\u0FB7' | '\u0FB9' | '\u20D0'..'\u20DC' | '\u20E1' | '\u302A'..'\u302F' | '\u3099' | '\u309A' ;

protected
XML10Digit: '\u0030'..'\u0039' | '\u0660'..'\u0669' | '\u06F0'..'\u06F9' | '\u0966'..'\u096F' | '\u09E6'..'\u09EF' | '\u0A66'..'\u0A6F' | '\u0AE6'..'\u0AEF' | '\u0B66'..'\u0B6F' | '\u0BE7'..'\u0BEF' | '\u0C66'..'\u0C6F' | '\u0CE6'..'\u0CEF' | '\u0D66'..'\u0D6F' | '\u0E50'..'\u0E59' | '\u0ED0'..'\u0ED9' | '\u0F20'..'\u0F29' ;

protected
XML10Extender: '\u00B7' | '\u02D0' | '\u02D1' | '\u0387' | '\u0640' | '\u0E46' | '\u0EC6' | '\u3005' | '\u3031'..'\u3035' | '\u309D'..'\u309E' | '\u30FC'..'\u30FE' ;
