// Copyright 2004-2005 Dean Tribble under the terms of the MIT X license
// found at http://www.opensource.org/licenses/mit-license.html

// @author Dean Tribble

header {
//package org.erights.e.elang.syntax.antlr;
}

// A pattern to produce a possibly empty list of xItem separated and optionally
// followed by commas, we need to use a special construction:
//   xItemList   :   (xItem (","! xItemList)?)? ;
// note that this cannot be an empty list with a comma.

// A caret after a terminal pulls that terminal out and makes it the parent
// node of the tree currently being built. An exclamation after a element
// suppresses generation of a node for that terminal

// Actions in the parse are enclosed in curlies.  Antlr provides some special
// syntax for tree construcion:
// ## refers to the result node of this production.
// Assigning to ## specifies the tree that will be produced.  All following
// terminals will be appended as siblings to the children of that node.
// #name refers to the named AST node

// Two patterns are commonly used to create the tree below:
// 1) add a caret to a terminal that would otherwise be suppressed, then change
//    the resulting root's type:
//           assign : cond ":="^ assign   {##.setType(AssignExpr);} ;
//    elevates the ":=" to be the parent of the "cond" and "assign" nodes, then
//    sets its type to AssignExpr
// 2) create a new node using Antlr's creation syntax:
//           assign : cond ":="! assign   {##=#([AssignExpr],##);} ;
//    suppressed the ":=" node, then assigns the result node to a newly created
//    tree with a newly created AssignExpr node at the root, and all the
//    previous nodes (cond and assign) as children

// '\' followed by only whitespace suppressed the whitespace and the newline
//      ? should this also apply to quasiquote
// all updoc test case are ignored before E lexing.  See the lexer for details.

// TODO:
// . plurals
// anonymous lambda syntax

// TODO add doco for matcher?

// MARK: Trailing comma in implements and extends lists would be ambiguous
// with HideExpr.  Perhaps HideExpr should not be allowed as an
// arbitrary expression?

class EParser extends Parser;

options {
    k = 2;                           // number of token lookahead
    importVocab=Common;
    exportVocab=E;
    codeGenMakeSwitchThreshold = 2;  // Some optimizations
    codeGenBitsetTestThreshold = 3;
    buildAST = true;
}

tokens {
    // XXX organize this list

    // various expressions
    NKAssignExpr;
    UpdateExpr;
    CallExpr;
    DefrecExpr;
    ForwardExpr;
    EscapeExpr;
    HideExpr;
    IfExpr;
    If1Expr;
    ForExpr;
    WhenExpr;
    CoerceExpr;
    CompareExpr;
    SameExpr;
    ConditionalExpr;
    PrefixExpr;
    LiteralExpr;
    MatchBindExpr;
    MismatchExpr;
    NounExpr;
    ObjectHeadExpr;
    InterfaceExpr;
    QuasiExpr;           // XXX unconfuse names: this is the `...` syntax, the following two are the holes-in-source-code syntax. same fix for *Pattern.
    QuasiLiteralExpr;
    QuasiPatternExpr;
    QuasiParserExpr;
    MetaStateExpr;
    MetaContextExpr;
    SeqExpr;
    SlotExpr;
    MetaExpr;
    CatchExpr;
    FinallyExpr;

    // non-kernel expressions
    FunCallExpr;
    FunSendExpr;
    GetExpr;
    ExitExpr;
    PropertyExpr;
    PropertySlotExpr;
    WhileExpr;
    SwitchExpr;
    TryExpr;
    MapPattern;
    MapPatternAssoc;
    MapPatternImport;
    MapPatternOptional;
    MapPatternRequired;
    ListExpr;
    MapExpr;
    BindPattern;
    SendExpr;
    CurryExpr;
    BinaryExpr;
    ModPowExpr;
    AccumExpr;
    AccumPlaceholderExpr;

    // patterns
    FinalPattern;
    VarPattern;
    SlotPattern;
    ListPattern;
    TailPattern;
    IgnorePattern;
    SamePattern;
    SuchThatPattern;
    QuasiPattern;
    QuasiLiteralPattern;
    QuasiPatternPattern;

    URI;
    URIStart;
    URIGetter;
    URIExpr;
    ThunkExpr;
    FunctionExpr;
    IndexExpr;

    // object parts
    EScript;
    EMethod;
    ETo;
    EMatcher;
    MethodObject;
    PlumbingObject;
    FunctionObject;
    FunctionVerb;

    // quasi parts
    QuasiExprHole;
    QuasiPatternHole;

    // expression nodes that only occur as part of other syntaxes
    MessageDescExpr;
    ParamDescExpr;
    WhenFnExpr;

    // miscellaneous structure
    List;
    Assoc;
    Export;
    Absent;
    True;
    False;
    DocComment;

    //for lexer
    HEX;
    OCTAL;
    WS;
    LINESEP;
    SL_COMMENT;
    DOC_COMMENT;
    CHAR_LITERAL;
    STRING;
    ESC;
    HEX_DIGIT;
    IDENT;
    INT;
    POSINT;
    FLOAT64;
    EXPONENT;
}

{
// overrides inherited warning method
public void reportWarning(String s) {
    try {
        int line = LT(0).getLine();
        int col = LT(0).getColumn();
        if (getFilename() == null) {
            System.err.println("@" + line + "warning: " + s);
        }
        else {
            System.err.println(getFilename() + "@" + line + ": warning: " + s);
        }
    } catch (TokenStreamException t) {
        System.err.println("warning: " + s);
    }
}
public void reportError(RecognitionException ex) {
    super.reportError(ex);
    throw new RuntimeException(ex);
}

// pocket mechanisms: add a boolean, and test in the grammar with {foo}?
private java.util.Properties myPocket = new java.util.Properties();

public java.util.Properties getPockets() {
    return myPocket;
}

protected boolean is(String v) {
    return returnAST.getText().equals(v);
}

protected boolean isPocket(String key) {
    // XXX clean this up
    String v = myPocket.getProperty(key, "disable");
    if ("disable".equals(v)) {
        return false;
    } else if ("enable".equals(v)) {
        return true;
    } else {
        throw new RuntimeException("can't convert " + v + " to boolean for: " + key);
    }
}

}


// HELPER PRODUCTIONS
// Grammar productions to avoid requiring implementation-language-specific
// action code.

warn![String msg]: {
    reportWarning(msg);
    if (false) {throw new RecognitionException("warn");}
} ;

pocket![String key]: {
    String v = myPocket.getProperty(key, "disable");
    if ("warn".equals(v)) {
        reportWarning(key + " in pocket");
    } else if (!v.equals("enable")) {
        String error = "The optional \"" + key + "\" syntax is currently off.";
        reportError(error);
        throw new RecognitionException(error);
    }
} ;

setPocket![Token key, String value]: {
    //myPocket.put(key.getText().substring(1, key.getText().length()-1), value);
    myPocket.put(key.getText(), value);
    if (false) {throw new RecognitionException("warn");}
} ;

getPocket![String key]:
                {isPocket(key)}? {##=#([True],##);}
            |                    {##=#([False],##);} ;



start:     br (topSeq)? ;
//start:     (module seqSep | LINESEP!)* (topSeq)? ;
//module: !;// placeholder for expressions that must be at the top of the file

topSeq:         topExpr ((seqSep)+ (topSeqMore {##=#([SeqExpr],##);})? )? ;
topSeqMore:     topExpr ((seqSep)+ (topSeqMore)?)? ;

topExpr:    eExpr | pragma ;

pragma!:     "pragma"^ "."! verb
            ( {is("enable")}? "("! en:STRING ")"!    setPocket[en, "enable"]!
            | {is("disable")}? "("! dis:STRING ")"!  setPocket[dis, "disable"]!
            | {is("warn")}? "("! wn:STRING ")"!      setPocket[wn, "warn"]!
            | {is("syntax")}? pocket["syntax"]!  );

metaExpr:  "meta"^ "."! verb
            (   {is("getState")}? {##=#([MetaStateExpr,"MetaScope"]);}
            |   {is("scope")}?    {##=#([MetaStateExpr,"MetaScope"]);}
            |   {is("context")}?  {##=#([MetaContextExpr,"MetaContext"]);} )
            "("! ")"! ;

br:         (LINESEP!)* ;
filler:     {false}? | {##=#([Absent]);} ;


seq:        eExpr ((seqSep)+ (seqMore {##=#([SeqExpr],##);})? )? ;
seqMore:    eExpr ((seqSep)+ (seqMore)?)? ;

seqSep:     (";"! | LINESEP!);

eExpr:      assign | ejector ;

basic:      ifExpr | forExpr | whileExpr | switchExpr | tryExpr
            |   escapeExpr | whenExpr | metaExpr | accumExpr
            |   docoDef | fnExpr ;

ifExpr:     "if"^ parenExpr br body  // MARK should BR before block be allowed?
            ("else"! (ifExpr | body ) {##.setType(IfExpr);}
             |                        {##.setType(If1Expr);})
            ;

forExpr:    "for"^ forPatt "in"! br assign body (catcher)?
                                               {##.setType(ForExpr);}  ;

// XXX rewrite this so it produces the right tree without lookahead
forPatt:        (pattern br "=>") => pattern br "=>"! pattern
            |   {##=#([Absent], ##);} pattern
            ;

accumExpr:      "accum"^ call accumulator pocket["accumulator"]!
                {##.setType(AccumExpr);};

accumulator:
        "for"^ forPatt "in"! logical accumBody {##.setType(ForExpr);}
 |      "if"^ parenExpr            accumBody   {##.setType(If1Expr);}
 |      "while"^ parenExpr         accumBody   {##.setType(WhileExpr);}
 ;

accumBody:                      // XXX full set of binary ops
        "{"! ( accumPlaceholder (("+"^ | "*"^ | "&"^ | "|"^) assign
                                                      {##.setType(BinaryExpr);}
                                | "."^ verb parenArgs {##.setType(CallExpr);})
            | accumulator
            ) br "}"!
 ;

accumPlaceholder: "_" {##.setType(AccumPlaceholderExpr);} ;

whenExpr:       "when"^ parenArgsList br "->"!  whenFn   {##.setType(WhenExpr);} ;

whenFn:         objName params optGuard body whenCatchers optFinally getPocket["easy-return"]  {##=#([WhenFnExpr],##);}  ;

whenCatchers:   (catcher)+ {##=#([List],##);}
            |   pocket["easy-when"] {##=#([List],##);} ;

whileExpr:      "while"^ parenExpr body  {##.setType(WhileExpr);}  (catcher)? ;

escapeExpr:     "escape"^ pattern body optCatchClause {##.setType(EscapeExpr);} ;

thunkExpr:      "thunk"^ body    {##.setType(ThunkExpr);}  ;

fnExpr:         "fn"^ patternsAsList body pocket["anon-lambda"]
                    {##.setType(FunctionExpr);}  ;

switchExpr:     "switch"^ parenExpr
                "{"! (matcher br)* "}"!    {##.setType(SwitchExpr);}
            ;

tryExpr:        "try"^ body
                (catcher)*
                ("finally"! body)?                      {##.setType(TryExpr);}
            ;

bindPatt:       "bind"^ nounExpr optGuard {##.setType(BindPattern);} ;
varPatt:        "var"^  nounExpr optGuard {##.setType(VarPattern);}  ;
slotPatt:       "&"^    nounExpr optGuard {##.setType(SlotPattern);} ;

// those patterns which may replace a "def" keyword
keywordPatt:    varPatt | bindPatt ;

// should forward declaration allow types?
//docoDef:    doco (defExpr | interfaceExpr | thunkExpr) ;
docoDef:    (DOC_COMMENT {##=#([DocComment],##);})?
            (objectExpr | interfaceExpr | thunkExpr) ;

objectExpr:     "def"^ objName objectTail           {##.setType(ObjectHeadExpr);}
                | keywordPatt objectTail    {##=#([ObjectHeadExpr],##);}
            ;

defExpr:    "def"^  ( (nounExpr ":=") => pattern ":="! defRightSide  {##.setType(DefrecExpr);}
                    | (nounExpr) => nounExpr {##.setType(ForwardExpr);}
                    | pattern ":="! defRightSide  {##.setType(DefrecExpr);}
                    )
            | keywordPatt ":="! defRightSide {##=#([DefrecExpr],##);}
            ;

// trinary-define support
defRightSide:  ( "("! seq ","! ) =>
                 "("! seq ","! seq ")"! pocket["trinary-define"]!
               | assign filler
               ;

// minimize the look-ahead for objectTail
objectPredict:    ("def" objName | keywordPatt)
                  ("extends" | "implements" | "match" | "{"| "(" ) ;
objectTail:     //(typeParams)?
                //(":"! guard)?
            extender
            oImplements
            scriptPair {##=#([MethodObject],##);}
        |   oImplements
            (   scriptPair {##=#([MethodObject], #([Absent]), ##);}
            |   matcher pocket["plumbing"]!
                {##=#([PlumbingObject], ##);}
            )
        |   params optGuard fImplements body getPocket["easy-return"]
            {##=#([FunctionObject], ##);}
    ;

extender:    "extends"! br order ;
oExtends:    extender
             |                   {##=#([Absent],##);}  ;

oImplements: "implements"^ br order (","! order)* {##.setType(List);}
             | {##=#([List],##);} ;
            // trailing comma would be ambiguous with HideExpr

// this can be replaced with oImplements when function-implements becomes official
fImplements:    "implements"^ br order (","! order)*
                pocket["function-implements"] {##.setType(List);}
             |  {##=#([List],##);} ;


// used in interface expressions
multiExtends: "extends"^ br order (","! order)* {##.setType(List);}
             | {##=#([List],##);} ;
            // trailing comma would be ambiguous with HideExpr

objName:        nounExpr        filler {##=#([FinalPattern],##);}
            |   "_"^                   {##.setType(IgnorePattern);}
            |   "bind"^ noun    filler {##.setType(BindPattern);}
            |   "var"^ nounExpr filler {##.setType(VarPattern);}
            |   "&"^ nounExpr   filler {##.setType(SlotPattern);}
            |   STRING
            ;

//TODO MARK: what is typeParams for?  it appears to come right after "def name"
typeParams:     "[" typePatternList br "]" ; // should have a br before the "]"

typePatternList:    (nounExpr (":"! guard)? ("," typePatternList)?)? ;

scriptPair:  "{"! methodList matcherList  "}"! ;

methodList:     ( method br )* {##=#([List], ##);} ;
matcherList:    ( matcher br )* {##=#([List], ##);} ;

// (not used) methodPredict:  doco ("to"|"method"|"on") ;

method:         doco ( "method"^ methodTail {##.setType(EMethod);}
                     | "to"^ methodTail getPocket["easy-return"] {##.setType(ETo);}              ) ;
methodTail:     optVerb params optGuard body ;

optVerb:        verb | {##=#([FunctionVerb]);} ;

matcher:        "match"^ pattern body  {##.setType(EMatcher);} ;

params:         "("! patterns br ")"!  {##=#([List],##);} ;
patternsAsList:   patterns {##=#([List],##);} ; // XXX can we avoid needing this as a separate rule?
patterns:       (pattern (","! patterns)?)? ;

optFinally:     "finally"! body
            |   {##=#([Absent],##);}
            ;

// ("throws" guardList)? ;
optGuard:    ":"! guard | {##=#([Absent]);} ;

guardList:      guard (","! guard)* ;    // requires at least one guard. cannot
                                         // end with comma

interfaceExpr:  "interface"! objName
                //(":"! guard)?
                (   iguards
                    multiExtends
                    oImplements
                    iscript
                |   // NOTE this greedily consumes a following guard, thereby
                    // accepting things e.y cannot
                    mtypes ((":") => ":"! guard | ) // function -- XXX wrong tree
                )
                {##=#([InterfaceExpr],##);}
            ;
// XXX NOTE: Placing 'iguards' outside the alternation above, allowing
// it for function interfaces, creates a pseudo-ambiguity:
//   e`interface a guards b ? c () {}` could be either:
//     a function-interface with 'guards b ? c' followed by a syntax error
//     or a general interface with 'guards b ? c()'
// E-on-Java's parser does not allow 'guards' in function interfaces, so
// I have imitated it, but I know of no /semantic/ reason to exclude it.

iguards:        ("guards"! pattern)
            |   ({##=#([Absent]);})
            ;

iscript:    "{"^ (imethod br)* "}"! {##.setType(List);} ;

imethod:    doco ("to"^ | "method"^ | "on"^) optVerb mtypes optGuard
                 {##.setType(MessageDescExpr);}
            ;

ptype:          (justNoun | "_" {##.setType(Absent);}) optGuard    
                {##=#([ParamDescExpr],##);}
            ;
typeList:   (ptype (","! typeList)?)? ;
mtypes:     "("! typeList br ")"!   {##=#([List],##);} ;

// The current E grammar only let's you put these in a few places.
doco:       DOC_COMMENT | {##=#([DOC_COMMENT]);} ;

body:       "{"! (seq | {##=#([SeqExpr],##);}) "}"! ;

// rules for expressions follow the pattern:
//   thisLevelExpression :  nextHigherPrecedenceExpression
//   (OPERATOR nextHigherPrecedenceExpression)*
// which is a standard recursive definition for a parsing an expression.
// The legal assignment syntax are:
//  x := ...
//  x op= ...       converts to x."op"(...)
//  x verb= ...     converts to x.verb(...)
//  x[i...] := ...  converts to x.put(i..., ...)
//  x::name := ...  converts to x.setName(...)
//  x(i...) := ...  converts to x.setRun(i..., ...)
// Based on the above patterns, only nounExpr, nounExpr
assign:     (("def"|"var"|"bind") => objectPredict|) => cond (  ":="^ assign  {##.setType(NKAssignExpr);}
                |   assignOp assign               {##=#([UpdateExpr], ##);}
                |   verb "="!   // deal with deprecated single case
                    ( ("(")=> parenArgs
                     | assign warn["Parentheses expected on verb= argument"]!)
                    {##=#([UpdateExpr], ##);}
                )?
            |   defExpr
            ;

assignOp:       "//=" | "+=" | "-=" | "*=" | "/="
            |   "%=" | "%%=" | "**=" | ">>=" | "<<=" | "&="
            |   "^=" | "|="
            ;

ejector:        (   "break"^
                |   "continue"^
                |   "return"^
                ) ejectorArg         {##.setType(ExitExpr);}
            |   "^"^ assign          {##.setType(ExitExpr);##.setText("return");}
                                     warn["Smalltalk-style '^' deprecated"]!
            ;

ejectorArg: ("(" ")") => "("! ")"! {##=#([Absent], ##);}
            | assign
            | {##=#([Absent],##);}
            ;

// || is don't-care associative
cond:           condAnd ("||"^ condAnd  {##.setType(ConditionalExpr);})*
            ;
// && is don't-care associative
condAnd:        logical ("&&"^ logical  {##.setType(ConditionalExpr);})*
            ;

// ==, !=, &, |, ^, =~, and !~ are all non associative with each
// other.  & and |, normally used for associative operations, are each
// made associative with themselves. None of the others even associate
// with themselves. Perhaps ^ should?
// SameExpr, BinaryExpr, CompareExpr, CoerceExpr
logical:        order
                (   "=="^ order      {##.setType(SameExpr);}
                |   "!="^ order      {##.setType(SameExpr);}
                |   "&!"^ order      {##.setType(BinaryExpr);}
                |   "=~"^ pattern    {##.setType(MatchBindExpr);}
                |   "!~"^ pattern    {##.setType(MismatchExpr);}
                |   ("^"^ {##.setType(BinaryExpr);} order)+
                |   ("&"^ {##.setType(BinaryExpr);} order)+
                |   ("|"^ {##.setType(BinaryExpr);} order)+
                )?   //optional
            ;

order:          interval
                (   o:compareOp! interval
                      {##=#(o, ##);}{##.setType(CompareExpr);}
                |   ":"^ guard      {##.setType(CoerceExpr);}
                |   )  // empty
            ;

// The br for ">" is because it is used to close URIs, where it cannot have a
// br.
compareOp:        "<" | "<=" | "<=>" | ">=" | ">" br ;

// .. and ..! are both non-associative (no plural form)
interval:       shift ((".."^ | "..!"^) {##.setType(BinaryExpr);} shift)?  ;

// << and >> are left-associative (no plural form)
shift:          add (("<<"^ | ">>"^) {##.setType(BinaryExpr);} add)*   ;

//+ and - are left associative
add:            mult (("+"^ | "-"^) {##.setType(BinaryExpr);} mult)*   ;

// *, /, //, %, and %% are left associative
mult:           (prefix "**" prefix "%%") => 
                prefix "**"! prefix "%%"^ pow {##.setType(ModPowExpr);}
            |   pow ( ("*"^ | "/"^ | "//"^ | "%"^ | "%%"^) pow
                      {##.setType(BinaryExpr);}                )*;
                

// ** is non-associative
pow:            prefix ("**"^ prefix     {##.setType(BinaryExpr);}   )?  ;

// Unary prefix !, ~, &, *, and - are non-associative.
// Unary prefix !, ~, &, and * bind less tightly than unary postfix.
// Unary prefix -, because it will often be mistaken for part of a literal
// number rather than an operator, is not combinable with unary postfix, in
// order to avoid the following surprise:
//      -3.pow(2) ==> -9
// If -3 were a literal, the answer would be 9. So, in E, you must say either
//      (-3).pow(2)  or -(3.pow(2))
// to disambiguate which you mean.
prefix:         postfix
            |  slotExpr
            |  ("!" | "~" | "*" | "+")  postfix    {##=#([PrefixExpr],##);}
            |  "-" prim                            {##=#([PrefixExpr],##);}
            ;

// extracted because of its use in 'map'
slotExpr: "&"!  postfix                        {##=#([SlotExpr],##);} ;

// Calls and sends are left associative.
postfix:        call
            // TODO deal with properties
            ;

call:   prim
        (   { ##=#([FunCallExpr], ##); } parenArgs
        // XXX reformat this to be clearer
        |   "."^ verb  { ##.setType(CallExpr); }
                       (("(") => parenArgs
                       | pocket["verb-curry"]! { ##=#([CurryExpr], ##);})
        |   "["^ argList "]"! {##.setType(GetExpr);}
        |   "<-"^ { ##.setType(SendExpr); }
                  ( parenArgs { ##.setType(FunSendExpr); }
                  | verb (("(") => parenArgs // )
                         | pocket["verb-curry"]! { ##=#([CurryExpr], ##);})
                  | "::"^ ("&")? prop // XXX bad AST
                  )
        |   "::"^ ( "&"! prop {##.setType(PropertySlotExpr);}
                  | prop      {##.setType(PropertyExpr);} )
        )*
    ;

parenArgs:      "("! argList ")"!  ;
lambdaArgs:      "("! argList ")"! (sepword! body)?  ; //(body)? | body  ;

parenArgsList:  parenArgs {##=#([List],##);} ;

sepword:    IDENT | reserved | "else" | "catch" | "finally"
            |  "try" | "->" ;

argList:        (seq (","! argList)?)? ;

prim:           literal
            |   basic
            |   (IDENT QUASIOPEN) =>
                quasiParser quasiString  {##=#([QuasiExpr],##);}
            |   nounExpr
            |   parenExpr (quasiString   {##=#([QuasiExpr],##);}  )?
            |   quasiString              {##=#([QuasiExpr,"simple"],
                                               [Absent],##);}
            |   URI                      {##=#([URIExpr],##);}
            |   URIStart add ">"!        {##=#([URIExpr],##);}
                                         warn["computed URIExpr is deprecated"]!
            //|   "<"^ nounExpr (":"! add)? ">"! {##.setType(URIExpr);}
            |   "["^
                (   (seq "=>" | "=>") => mapList
                                                {##.setType(MapExpr);}
                |   argList                     {##.setType(ListExpr);}
                )  "]"!
            |   body          {##=#([HideExpr],##);} warn["hide deprecated"]!
            ;

mapList:    (map (","! mapList)?)?   ;

map:            seq "=>"^ seq {##.setType(Assoc);}
            |   "=>"^ (nounExpr
                      | slotExpr
                      // | "def" nounExpr // XXX should be an explicit error; EoJ says: reserved: Forward exporter
                      ) {##.setType(Export);}
            ;

//Property names for use e.g., with the :: syntax.
prop:           pocket["dot-props"]! ( IDENT | STRING ) ;

// a method selector
verb:           IDENT | STRING  ;

literal:    (STRING | CHAR_LITERAL | INT | FLOAT64 | HEX | OCTAL)
            {##=#([LiteralExpr],##);} ;


// a valid guard is a nounExpr or parenExpr, optionally followed by [argList]*
guard:
    (nounExpr | parenExpr)
    // NOTE the greedy consumption of getters.  This accepts more than e.y
    (options{greedy=true;}: "["^ argList "]"! {##.setType(GetExpr);})*
    ;

// XXX remove duplication of syntax
catcher:        "catch"^ pattern body ;

optCatchClause: "catch"! pattern body 
            |            filler  filler
            ;

// Patterns
pattern:        listPatt ("?"^ order  {##.setType(SuchThatPattern);}  )?   ;

listPatt:
    eqPatt
    |   "["^
        (   ((key br)? "=>") => mapPatts br "]"! {##.setType(MapPattern);}
            ("|"^ listPatt {##.setType(TailPattern);} )?

        |   patterns br "]"! {##.setType(ListPattern);}
             ("+"^  listPatt {##.setType(TailPattern);} )?
        )
    ;

eqPatt:         (IDENT QUASIOPEN) =>
                quasiParser quasiString {##=#([QuasiPattern],##);}
            |   nounExpr optGuard       {##=#([FinalPattern],##);}
            |   // "_"^ optGuard        {##.setType(IgnorePattern);} // not yet
                "_"^                    {##.setType(IgnorePattern);}
            |   "=="^ prim              {##.setType(SamePattern);}
            |   "!="^ prim              {##.setType(SamePattern);}
            |   compareOp prim
            |   quasiString             {##=#([QuasiPattern,"simple"],
                                              [Absent],##);}
            |   parenExpr quasiString   {##=#([QuasiPattern],##);}
            |   keywordPatt
            |   slotPatt
            ;

// namePatts are patterns that bind at most one name.
// this is expanded inline into eqPatt, but used directly elsewhere
// Note that this is not a simple subset of eqPatt.
namePatt:       nounExpr optGuard    {##=#([FinalPattern],##);}
            |   // "_"^ optGuard     {##.setType(IgnorePattern);} // not yet
                "_"^                 {##.setType(IgnorePattern);}
            |   keywordPatt
            |   slotPatt
            ;

justNoun:       IDENT
            |   "::"^ pocket["noun-string"]!
                (STRING | IDENT)
            |   URIGetter
            ;

// XXX rename sensibly
noun:       justNoun {##=#([NounExpr],##);} ;

// XXX rename sensibly
nounExpr:       noun
            |   dollarHole                   {##=#([QuasiLiteralExpr],##);}
            |   atHole                       {##=#([QuasiPatternExpr],##);}
            ;

// returns the index; the parent must take care of wrapping the quasi node
dollarHole:     SOURCE_VALUE_HOLE   {##.setType(INT);} ;
atHole:         SOURCE_PATTERN_HOLE {##.setType(INT);} ;

key:            parenExpr | literal  ;

parenExpr:      "("! seq ")"!  ;
//args        :   "("! br seq ")"!  ;

//mapPattList:    mapPatts {##=#([List],##);} ;
mapPatts:       (mapPattern (","! mapPatts)?)? ;

mapPattern:       mapPatternAddressing (":="^ order {##.setType(MapPatternOptional);}
                                       |            {##=#([MapPatternRequired],##);}) ;

mapPatternAddressing: key br "=>"^ pattern {##.setType(MapPatternAssoc);}
                    | "=>"^ namePatt       {##.setType(MapPatternImport);}
                    ;

// QUASI support
quasiParser:    parenExpr
            |   IDENT {##=#([QuasiParserExpr],##);}
            ;

quasiString:    QUASIOPEN!
                (   exprHole
                |   pattHole
                |   QUASIBODY
                )* // {##=#([QuasiContent],##);}
                QUASICLOSE!  // NOTE: '`' is the QUASICLOSE token in the quasi
                             // lexer
            ;

exprHole:       DOLLARCURLY^
                seq {##.setType(QuasiExprHole);}
                "}"!
            |   DOLLARHOLE {##.setType(STRING);##=#([QuasiExprHole],#([NounExpr],##));}
            ;

pattHole:       ATCURLY^
                br pattern br {##.setType(QuasiPatternHole);}
                "}"!
            |   ATHOLE {##.setType(STRING);##=#([QuasiPatternHole],#([FinalPattern],#([NounExpr], ##),#([Absent])));}
            ;

// makes grammar compilation take too long
reserved:     "fn" | "fun" | "datatype" | "guards" | "interface" | "meta"
            | "pragma" | "thunk"
            | "accum" | "delegate" | "module" | "select" | "throws"
            | "abstract" | "an" | "as" | "assert"
            | "attribute" | "be" | "begin" | "behalf" | "belief" | "believe"
            | "believes" | "case" | "class"
            | "const" | "constructor" | "declare" | "default" | "define"
            | "defmacro" | "delicate" | "deprecated"
            | "dispatch" | "do" | "encapsulate" | "encapsulated"
            | "encapsulates" | "end" | "ensure" | "enum"
            | "eventual" | "eventually" | "export" | "facet" | "forall"
            | "function" | "given" | "hidden"
            | "hides" | "inline" | "is" | "know" | "knows" | "lambda" | "let"
            | "methods" | "namespace"
            | "native" | "obeys" | "octet" | "oneway" | "operator" | "package"
            | "private" | "protected"
            | "public" | "raises" | "reliance" | "reliant" | "relies" | "rely"
            | "reveal" | "sake" | "signed"
            | "static" | "struct" | "suchthat" | "supports" | "suspect"
            | "suspects" | "synchronized" | "this"
            | "transient" | "truncatable" | "typedef" | "unsigned" | "unum"
            | "uses" | "using" | "utf8"
            | "utf16" | "virtual" | "volatile" | "wstring" ;
/*/**/
