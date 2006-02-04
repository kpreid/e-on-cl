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
    AssignExpr;
    UpdateExpr;
    CallExpr;
    DefineExpr;
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
    ObjectExpr;
    InterfaceExpr;
    QuasiLiteralExpr;
    QuasiPatternExpr;
    MetaStateExpr;
    MetaContextExpr;
    SeqExpr;
    SlotExpr;
    MetaExpr;
    CatchExpr;
    FinallyExpr;

    ReturnExpr;
    ContinueExpr;
    BreakExpr;
    WhileExpr;
    SwitchExpr;
    TryExpr;
    MapPattern;
    LiteralPattern;
    ListExpr;
    MapExpr;
    BindPattern;
    SendExpr;
    CurryExpr;
    BinaryExpr;
    AccumExpr;
    AccumPlaceholderExpr;

    FinalPattern;
    VarPattern;
    SlotPattern;
    ListPattern;
    TailPattern;
    IgnorePattern;
    SamePattern;
    SuchThatPattern;
    QuasiLiteralPattern;
    QuasiPatternPattern;
    URI;
    URIStart;
    URIGetter;
    URIExpr;
    ThunkExpr;
    IndexExpr;

    EScript;
    EMethod;
    EMatcher;
    MethodObject;
    PlumbingObject;
    FunctionObject;
    List;
    WhenFn;
    Implements;
    Extends;
    Absent;
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
        reportError(key + " not allowed");
        throw new RecognitionException(key + " not allowed");
    }
} ;

setPocket![Token key, String value]: {
    //myPocket.put(key.getText().substring(1, key.getText().length()-1), value);
    myPocket.put(key.getText(), value);
    if (false) {throw new RecognitionException("warn");}
} ;


//;; GRUMBLE: an empty body makes the parser return only the pattern node
// it should return an "empty-body node" or some such instead
//
//that's from parsing "escape foo {}"
//;; GRUMBLE: updates should have a separate node type
//about AssignExpr

start:     br (topSeq)? ;
//start:     (module (";"! | LINESEP!) | LINESEP!)* (topSeq)? ;
//module: !;// placeholder for expressions that must be at the top of the file

topSeq:     topExpr (((";"! | LINESEP!) (topExpr)?)+ {##=#([SeqExpr],##);}  )? ;

topExpr:    eExpr | pragma ;

pragma!:     "pragma"^ "."!
            ( "enable" "("! en:STRING ")"!    setPocket[en, "enable"]!
            | "disable" "("! dis:STRING ")"!  setPocket[dis, "disable"]!
            | "warn" "("! wn:STRING ")"!      setPocket[wn, "warn"]!
            | "syntax" pocket["syntax"]!  );

metaExpr:  "meta"^ "."!
            (("getState"! | "scope"!) {##=#([MetaStateExpr,"MetaScope"]);}
             | "context"! {##=#([MetaContextExpr,"MetaContext"]);} ) "("! ")"! ;

br:         (LINESEP!)* ;

seq:        eExpr (((";"! | LINESEP!) (eExpr)?)+ {##=#([SeqExpr],##);}  )? ;

eExpr:      assign | ejector ;

basic:      ifExpr | forExpr | whileExpr | switchExpr | tryExpr
            |   escapeExpr | whenExpr | metaExpr | accumExpr ;

ifExpr:     "if"^ parenExpr br body  // MARK should BR before block be allowed?
            ("else"! (ifExpr | body ) {##.setType(IfExpr);}
             |                        {##.setType(If1Expr);})              
            ;

forExpr:    "for"^ forPatt "in"! br assign body (catcher)?
                                               {##.setType(ForExpr);}  ;
// the first pattern is actually the optional one. If it is missing, include an
// empty ignore pattern for it.
forPatt:        pattern br
                ("=>"! pattern    {##=#([ListPattern,"=>"], ##);}
                |   {##=#([ListPattern,"=>"], [IgnorePattern, ""], ##);})
            ;

accumExpr:      "accum"^ call accumulator pocket["accumulator"]! 
                {##.setType(AccumExpr);};

accumulator:
        "for"^ forPatt "in"! logical accumBody {##.setType(ForExpr);}
 |      "if"^ parenExpr            accumBody   {##.setType(If1Expr);}
 |      "while"^ parenExpr         accumBody   {##.setType(WhileExpr);}
 ;

accumBody:
        "{"! ( accumPlaceholder (("+"^ | "*"^ | "&"^ | "|"^) assign 
                                                      {##.setType(BinaryExpr);}
                                | "."^ verb parenArgs {##.setType(CallExpr);})
            | accumulator
            ) br "}"!
 ;

accumPlaceholder: "_" {##.setType(AccumPlaceholderExpr);} ;

whenExpr:       "when"^ parenArgs br "->"!  whenFn
                (catcher)* ("finally" body)?   {##.setType(WhenExpr);}
            ;

whenFn:         objName params (":"! guard)? body   {##=#([WhenFn],##);}  ;

whileExpr:      "while"^ parenExpr body  {##.setType(WhileExpr);}  (catcher)? ;

escapeExpr:     "escape"^ pattern body (catcher)?   {##.setType(EscapeExpr);} ;

thunkExpr:     //doco
                "thunk"^ doco body    {##.setType(ThunkExpr);}  ;

switchExpr:     "switch"^ parenExpr
                "{"! (matcher br)* "}"!    {##.setType(SwitchExpr);}
            ;

tryExpr:        "try"^ body
                (catcher)*
                ("finally"! body)?                      {##.setType(TryExpr);}
            ;

binder:         "bind"^ noun (":"! guard)?      {##.setType(BindPattern);}  ;

varNamer:       "var"^ nounExpr (":"! guard)?   {##.setType(VarPattern);}  ;

slotNamer:      "&"^ nounExpr (":"! guard )?    {##.setType(SlotPattern);} ;

// should forward declaration allow types?
//docoDef:    doco (defExpr | interfaceExpr | thunkExpr) ;
docoDef:    (DOC_COMMENT {##=#([DocComment],##);})?
            defExpr | interfaceExpr | thunkExpr ;

//so ObjectExpr(doc, fqn, auditors, script|method|matcher)
//<kpreid> 'matcher' in the plumbing, def foo match ... {}, case
// var x := ... should produce a DefineExpr with a VarPattern
// bind x := ... should produce a DefineExpr with a BindPattern
defExpr:    //doco
            "def"^  (  (objectPredict) => doco objName objectTail
                                                      {##.setType(ObjectExpr);}
                    |  (pattern ":=") => pattern ":="! defRightSide
                                                      {##.setType(DefineExpr);}
                    |  nounExpr {##.setType(ForwardExpr);}
                    )
            | (binder | varNamer)
                    (   ":="! defRightSide {##=#([DefineExpr],##);}
                    |   objectTail  {##=#([ObjectExpr],##);}
                    )
            ;

// trinary-define support
defRightSide:  ( "("! eExpr ","! ) =>
                 "("! eExpr ","! eExpr ")"! pocket["trinary-define"]!
               | assign
               ;

// minimize the look-ahead for objectTail
objectPredict:    objName ("extends" | "implements" | "match" | "{"| "(" ) ;
objectTail:     //(typeParams)?
                //(":"! guard)?
            extender
            oImplements
            script {##=#([MethodObject],##);}
        |   oImplements
            (   script {##=#([MethodObject], #([Extends]), ##);}
            |   matcher pocket["plumbing"]!
                {##=#([PlumbingObject], ##);}
            )
        |   params resultGuard body      // function
            {##=#([FunctionObject], ##);}
    ;

extender:    "extends"^ br order {##.setType(Extends);} ;
oExtends:    extender
             |                   {##=#([Extends],##);}  ;

oImplements: "implements"^ br order (","! order)* {##.setType(Implements);}
             | {##=#([Implements],##);} ;
            // trailing comma would be ambiguous with HideExpr

objName:        nounExpr         {##=#([FinalPattern],##);}
            |   "_"^             {##.setType(IgnorePattern);}
            |   "bind"^ noun     {##.setType(BindPattern);}
            |   "var"^ nounExpr  {##.setType(VarPattern);}
            |   "&"^ nounExpr    {##.setType(SlotPattern);}
            |   STRING
            ;

//TODO MARK: what is typeParams for?  it appears to come right after "def name"
typeParams:     "[" typePatternList br "]" ; // should have a br before the "]"

typePatternList:    (nounExpr (":"! guard)? ("," typePatternList)?)? ;

//script:  "{"^ (method br)* (matcher br)* "}"!  {##.setType(List);} ;
script:  "{"^ methods  "}"! {##.setType(EScript);} ;

methods: (methodPredict) => method br methods | (matcher br )* ;
methodPredict: doco ("to"|"method"|"on") ;
method: doco  ("to"^ | "method"^ | "on"^) optVerb params resultGuard body
                {##.setType(EMethod);}
    ;

optVerb:      verb | {##=#([IDENT,""]);} ;

matcher:      doco "match"^ pattern body  {##.setType(EMatcher);} ;

params:         "("! patterns br ")"!  {##=#([List],##);} ;
patterns:    (pattern (","! patterns)?)? ;

// ("throws" guardList)? ;
 //TODO what's the right default for a missing guard?
resultGuard:    ":"! guard | {##=#([Absent]);} ;

guardList:      guard (","! guard)* ;    // requires at least one guard. cannot
                                         // end with comma

interfaceExpr:  //doco below is a hack.  it should be here...
                "interface"^  doco objName
                //(":"! guard)?
                (   ("guards" pattern)?
                    ("extends" br order ("," order)*)?     // trailing comma
                                            // would be ambiguous with HideExpr
                    ("implements" br order ("," order)*)?  // trailing comma
                                            // would be ambiguous with HideExpr
                    "{"^ (imethod br)* "}"!
                |   mtypes (":"! guard)?   // function
                )
                {##.setType(InterfaceExpr);}
            ;

imethod:    doco ("to"^ | "method"^ | "on"^) optVerb mtypes resultGuard
                 {##.setType(EMethod);}
            ;

ptype:      nounExpr (":"! guard)?    {##=#([FinalPattern],##);}
            |   "_"^ (":"! guard)?    {##.setType(IgnorePattern);}
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
assign:         cond
                (   ":="^ assign                  {##.setType(AssignExpr);}
                |   assignOp assign               {##=#([UpdateExpr], ##);}
                |   verb "="!   // deal with deprecated single case
                    ( ("(")=> parenArgs
                     | assign warn["Parentheses expected on verb= argument"]!)
                    {##=#([UpdateExpr], ##);}
                )?
            |   docoDef
            ;

assignOp:       "//=" | "+=" | "-=" | "*=" | "/="
            |   "%=" | "%%=" | "**=" | ">>=" | "<<=" | "&="
            |   "^=" | "|="
            ;

ejector:        (   "break"^         {##.setType(BreakExpr);}
                |   "continue"^      {##.setType(ContinueExpr);}
                |   "return"^        {##.setType(ReturnExpr);}
                ) ejectorArg
            |   "^"^ assign          {##.setType(ReturnExpr);}
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
mult:           pow (("*"^ | "/"^ | "//"^ | "%"^ | "%%"^) pow
                    {##.setType(BinaryExpr);}   )* ;

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
            |  "&"!  postfix                        {##=#([SlotExpr],##);}
            |  ("!" | "~" | "*" | "+")  postfix    {##=#([PrefixExpr],##);}
            |  "-" prim                            {##=#([PrefixExpr],##);}
            ;

// Calls and sends are left associative.
postfix:        call
            // TODO deal with properties
            ;

call:   p:prim
        (!  a:parenArgs         { ##=#([CallExpr,"run"], p, [STRING,"run"], a); }
        |   "."^ message        { ##.setType(CallExpr); }
        |!  "["^ l:argList "]"! { ##=#([CallExpr,"get"], p, [STRING,"get"], l); }
        |   "<-"^ (parenArgs | message | "::" ("&")? prop) { ##.setType(SendExpr); }
        |   "::" ("&")? prop
        )*
    ;

message:        verb (("(") => parenArgs
                      | {##.setType(CurryExpr);})   // curry
            ;

parenArgs:      "("! argList ")"!  ;
lambdaArgs:      "("! argList ")"! (sepword! body)?  ; //(body)? | body  ;

sepword:    IDENT | reserved | "else" | "catch" | "finally"
            |  "try" | "->" ;

argList:        (eExpr br (","! argList)?)? ;

prim:           literal
            |   basic
            |   nounExpr  (quasiString   {##=#([QuasiLiteralExpr],##);}  )?
            |   parenExpr (quasiString   {##=#([QuasiLiteralExpr],##);}  )?
            |   quasiString              {##=#([QuasiLiteralExpr,"simple"],
                                               [STRING,"simple"],##);}
            |   URI                      {##=#([URIExpr],##);}
            |   URIStart add ">"!        {##=#([URIExpr],##);}
                                         warn["computed URIExpr is deprecated"]!
            //|   "<"^ nounExpr (":"! add)? ">"! {##.setType(URIExpr);}
            |   "["^
                (   (eExpr br "=>") => mapList  {##.setType(MapExpr);}
                |   argList                     {##.setType(ListExpr);}
                )  "]"!
            |   body          {##=#([HideExpr],##);} warn["hide deprecated"]!
            ;

mapList:    (map br (","! mapList)?)?   ;

map:            eExpr br "=>"^ eExpr
            |   "=>"^ (nounExpr
                      | "&"nounExpr
                      | "def" nounExpr )
            ;

//Property names for use e.g., with the :: syntax.
// Should the "&" handling be here?
prop:           IDENT | STRING  ;

// a method selector
verb:           IDENT | STRING  ;

literal:    (STRING | CHAR_LITERAL | INT | FLOAT64 | HEX | OCTAL)
            {##=#([LiteralExpr],##);} ;


// a valid guard is a nounExpr or parenExpr, optionally followed by [argList]*
guard:
    (nounExpr | parenExpr)
    ("["! {##=#([CallExpr,"get"], ##, [STRING,"get"]);} argList "]"! )*
    ;

catcher:        "catch"^ pattern body ;

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

eqPatt:         nounExpr
                (   (":"! guard)?       {##=#([FinalPattern],##);}
                |   quasiString         {##=#([QuasiLiteralPattern],##);}
                                                       // was IDENT quasiString
                )
            |   "_"^ (":"! guard)?      {##.setType(IgnorePattern);}
            |   "=="^ prim              {##.setType(SamePattern);}
            |   "!="^ prim              {##.setType(SamePattern);}
            |   compareOp prim
            |   quasiString             {##=#([QuasiLiteralPattern,"simple"],
                                              [STRING,"simple"],##);}
            |   parenExpr quasiString   {##=#([QuasiLiteralPattern],##);}
            |   binder
            |   varNamer
            |   slotNamer
            ;

// namePatts are patterns that bind at most one name.
// this is expanded inline into eqPatt, but used directly elsewhere
// Note that this is not a simple subset of eqPatt.
namePatt:       nounExpr (":"! guard)?    {##=#([FinalPattern],##);}
            |   "_"^ (":"! guard)?        {##.setType(IgnorePattern);}
            |   binder
            |   varNamer
            |   slotNamer
            ;

noun:       IDENT                           {##=#([NounExpr],##);}
            |  "::"^ pocket["noun-string"]!
                (STRING | IDENT)            {##=#([NounExpr],##);}
            |   URIGetter                   {##=#([NounExpr],##);}
            ;

nounExpr:       noun
            |   dollarHole                   {##.setType(QuasiLiteralPattern);}
            |   atHole                       {##.setType(QuasiPatternPattern);}
            ;

dollarHole:     "${" POSINT "}"
            |   "$" POSINT
            |   "$$"
            ;

atHole:         "@{"^ POSINT "}"!
            |   "@" POSINT
            ;

key:            parenExpr | literal  ;

parenExpr:      "("! seq ")"!  ;
//args        :   "("! br seq ")"!  ;

//mapPattList:    mapPatts {##=#([List],##);} ;
mapPatts:       (mapPattern (","! mapPatts)?)? ;

mapPattern:     key br "=>"^ pattern (":=" order)?
            |   "=>"^ namePatt (":=" order)? // BLECH
            ;

// QUASI support
quasiString:    QUASIOPEN!
                (   exprHole
                |   pattHole
                |   QUASIBODY
                |   DOLLARHOLE
                |   ATHOLE
                )* {##=#([QuasiContent],##);}
                QUASICLOSE!  // NOTE: '`' is the QUASICLOSE token in the quasi
                             // lexer
            ;

exprHole:       DOLLARCURLY^
                br eExpr br {##.setType(DOLLARHOLE);}
                "}"!
            ;

pattHole:       ATCURLY!
                br pattern br {##.setType(ATHOLE);}
                "}"!
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
