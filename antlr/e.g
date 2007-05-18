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
// XXX what does this todo item mean? -- kpreid

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
    // expressions
    AccumExpr;
    BinaryExpr;
    CallExpr;
    CatchExpr;
    CoerceExpr;
    CompareExpr;
    ConditionalExpr;
    CurryExpr;
    DefrecExpr;
    EscapeExpr;
    ExitExpr;
    FinallyExpr;
    ForExpr;
    ForwardExpr;
    FunCallExpr;
    FunctionExpr;
    FunSendExpr;
    GetExpr;
    HideExpr;
    If1Expr;
    IfExpr;
    IndexExpr;
    InterfaceExpr;
    ListExpr;
    LiteralExpr;
    MapExpr;
    MatchBindExpr;
    MetaContextExpr;
    MetaExpr;
    MetaStateExpr;
    MismatchExpr;
    ModPowExpr;
    NKAssignExpr;
    NounExpr;
    ObjectHeadExpr;
    PrefixExpr;
    PropertyExpr;
    PropertySlotExpr;
    QuasiExpr;           // XXX unconfuse names: this is the `...` syntax, QLiteralE an QPatternE are the holes-in-source-code syntax. same fix for *Pattern.
    QuasiLiteralExpr;
    QuasiParserExpr;
    QuasiPatternExpr;
    SameExpr;
    SendExpr;
    SeqExpr;
    SlotExpr;
    SwitchExpr;
    ThunkExpr;
    TryExpr;
    UpdateExpr;
    URIExpr;
    WhenExpr;
    WhileExpr;
    
    // expression nodes that only occur as part of other syntaxes
    AccumPlaceholderExpr;
    MessageDescExpr;
    ParamDescExpr;
    WhenBlockExpr;
    WhenFnExpr;

    // patterns
    BindPattern;
    CallPattern;
    FinalPattern;
    FunCallPattern;
    GetPattern;
    GuardTestPattern;
    IgnorePattern;
    ListPattern;
    MapPattern;
    QuasiLiteralPattern;
    QuasiPattern;
    QuasiPatternPattern;
    SamePattern;
    SlotPattern;
    SuchThatPattern;
    TailPattern;
    VarPattern;
    ViaPattern;

    // special: map pattern parts
    MapPatternAssoc;
    MapPatternImport;
    MapPatternOptional;
    MapPatternRequired;

    // special: object parts
    EMatcher;
    EMethod;
    EScript;
    ETo;
    FunctionObject;
    FunctionVerb;
    MethodObject;
    OneMethodObject;
    PlumbingObject;

    // special: quasiliteral parts
    QuasiExprHole;
    QuasiPatternHole;

    // miscellaneous structure
    Absent;
    Assoc;
    DocComment;
    Export;
    False;
    List;
    True;

    //for lexer
    CHAR_LITERAL;
    DOC_COMMENT;
    ESC;
    EXPONENT;
    FLOAT64;
    HEX;
    HEX_DIGIT;
    IDENT;
    INT;
    LINESEP;
    OCTAL;
    DEC_NATURAL;
    SL_COMMENT;
    STRING;
    URI;
    URIGetter;
    WS;
}

{
// XXX Capability violation - these warnings should not be going to stderr, they should go only to something specified by the invoker of the parser
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


private void throwSemanticHere(String text) throws RecognitionException, TokenStreamException {
    Token pos = LT(0);
    throw new SemanticException(text, getFilename(), pos.getLine(), pos.getColumn());
}


private java.util.Properties myPocket = new java.util.Properties();

public java.util.Properties getPockets() {
    return myPocket;
}

protected boolean is(String v) {
    return returnAST.getText().equals(v);
}

protected boolean isPocket(String key) throws RecognitionException, TokenStreamException {
    // XXX clean this up
    String v = myPocket.getProperty(key, "disable");
    if ("disable".equals(v)) {
        return false;
    } else if ("enable".equals(v)) {
        return true;
    } else {
        throwSemanticHere(quoteForMessage(key) + " must be \"enable\" or \"disable\" here, not " + quoteForMessage(v));
        throw new RuntimeException("can't happen");
    }
}

private String quoteForMessage(String v) {
    // XXX do this properly
    return "\"" + v + "\"";
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
        throwSemanticHere("The optional " + quoteForMessage(key) + " syntax is currently off.");
    }
} ;

reversePocket![String key, String warnmsg, String errmsg]: {
    String v = myPocket.getProperty(key, "disable");
    if ("warn".equals(v)) {
        reportWarning(warnmsg);
    } else if (v.equals("enable")) {
        throwSemanticHere(errmsg);
    }
} ;

setPocket![Token key, String value]: {
    //myPocket.put(key.getText().substring(1, key.getText().length()-1), value);
    myPocket.put(key.getText(), value);
    if (false) {throw new RecognitionException("warn");}
} ;

setSyntax![Token arg]: {
    String syntaxName = arg.getText(); // string literal token
    // XXX these should be in syntax-definition files; this is a quick hack
    if ("0.8".equals(syntaxName)) {
        myPocket.put("easy-return", "disable");
        myPocket.put("easy-when", "disable");
        myPocket.put("hard-when", "enable");
        myPocket.put("explicit-result-guard", "enable");
        myPocket.put("anon-lambda", "enable");
        myPocket.put("plumbing", "enable");
        myPocket.put("noun-string", "disable");
        myPocket.put("thunk", "enable");
        myPocket.put("trinary-define", "disable");
        myPocket.put("verb-curry", "disable");
    } else if ("0.9".equals(syntaxName)) {
        myPocket.put("easy-return", "enable");
        myPocket.put("easy-when", "enable");
        myPocket.put("hard-when", "disable");
        myPocket.put("explicit-result-guard", "disable");
        myPocket.put("anon-lambda", "enable");
        myPocket.put("plumbing", "enable");
        myPocket.put("noun-string", "enable");
        myPocket.put("thunk", "disable");
        myPocket.put("trinary-define", "enable");
        myPocket.put("verb-curry", "enable");
    } else {
        throwSemanticHere("Unknown syntax version " + quoteForMessage(syntaxName) + ".");
    }
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
            ( {is("enable")}?  "("! e:STRING ")"! setPocket[e, "enable"]!
            | {is("disable")}? "("! d:STRING ")"! setPocket[d, "disable"]!
            | {is("warn")}?    "("! w:STRING ")"! setPocket[w, "warn"]!
            | {is("syntax")}?  "("! s:STRING ")"! setSyntax[s]!
            | { throwSemanticHere("Unknown pragma "
                                  + quoteForMessage(returnAST.getText())
                                  + "."); }
              parenParamList // for antlr's lookahead
            ) ;

// XXX what's the current status of meta.scope()?
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

ifExpr:     "if"^ parenExpr br block  // MARK should BR before block be allowed?
            ("else"! (ifExpr | block ) {##.setType(IfExpr);}
             |                        {##.setType(If1Expr);})
            ;

forExpr:    "for"^ forPatt "in"! br assign block optCatch {##.setType(ForExpr);} ;

// XXX rewrite this so it produces the right tree without lookahead
forPatt:        (pattern br "=>") => pattern br "=>"! pattern
            |   {##=#([Absent], ##);} pattern
            ;

accumExpr:      "accum"^ call accumulator pocket["accumulator"]!
                {##.setType(AccumExpr);};

accumulator:
        "for"^ forPatt "in"! logical
                           accumBody optCatch {##.setType(ForExpr);}
 |      "if"^ parenExpr    accumBody          {##.setType(If1Expr);}
 |      "while"^ parenExpr accumBody optCatch {##.setType(WhileExpr);}
 ;

accumBody:                      // XXX full set of binary ops
        "{"! ( accumPlaceholder (("+"^ | "*"^ | "&"^ | "|"^) assign
                                                      {##.setType(BinaryExpr);}
                                | "."^ verb parenArgs {##.setType(CallExpr);})
            | accumulator
            ) br "}"!
 ;

accumPlaceholder: "_" {##.setType(AccumPlaceholderExpr);} ;

whenExpr:       "when"^ parenArgsList br "->"!  whenBody   {##.setType(WhenExpr);} ;

whenBody:       objName pocket["hard-when"] parenParamList optGuard
                  block whenCatcherList optFinally getPocket["easy-return"]
                  {##=#([WhenFnExpr],##);}
            |   block whenCatcherList optFinally {##=#([WhenBlockExpr],##);}
            ;

whenCatcherList:   
                (catcher)+ {##=#([List],##);}
            |   pocket["easy-when"] {##=#([List],##);} ;

whileExpr:      "while"^ parenExpr block optCatch {##.setType(WhileExpr);} ;

escapeExpr:     "escape"^ pattern block ( "catch"! pattern block 
                                        |          filler  filler)
                {##.setType(EscapeExpr);} ;

thunkExpr:      "thunk"^ pocket["thunk"] block {##.setType(ThunkExpr);} ;

fnExpr:         "fn"^ pocket["anon-lambda"] patternList block
                    {##.setType(FunctionExpr);}  ;

switchExpr:     "switch"^ parenExpr
                "{"! (matcher br)* "}"!    {##.setType(SwitchExpr);}
            ;

tryExpr:        "try"^ block
                (catcher)*
                ("finally"! block)?                      {##.setType(TryExpr);}
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

defExpr:    "def"^  ( (nounExpr (":="|"exit")) => pattern defExit ":="! assign  {##.setType(DefrecExpr);}
                    | (nounExpr) => nounExpr {##.setType(ForwardExpr);}
                    | pattern defExit ":="! assign  {##.setType(DefrecExpr);}
                    )
            | keywordPatt defExit ":="! assign {##=#([DefrecExpr],##);}
            ;

defExit:        "exit"! order pocket["trinary-define"]!
            |   filler
            ;

// minimize the look-ahead for objectTail
objectPredict:    ("def" objName | keywordPatt)
                  ("extends" | "match" | ("." verb)? (parenParams optGuard)? ("{" | "implements") ) ;
objectTail:     //(typeParamList)?
                //optGuard
            extender
            oImplements
            scriptPair {##=#([MethodObject],##);}
        |   oImplements
            (   scriptPair {##=#([MethodObject], #([Absent]), ##);}
            |   matcher pocket["plumbing"]!
                {##=#([PlumbingObject], ##);}
            )
        |   functionTail {##=#([FunctionObject], ##);}
        |   "."^ pocket["one-method-object"]! verb 
            functionTail {##.setType(OneMethodObject);}
    ;

functionTail: parenParamList optResultGuard fImplements block getPocket["easy-return"] ;

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

objName:        nounExpr         filler {##=#([FinalPattern],##);}
            |   "_"^                    {##.setType(IgnorePattern);}
            |   "bind"^ nounExpr filler {##.setType(BindPattern);}
            |   "var"^ nounExpr  filler {##.setType(VarPattern);}
            |   "&"^ nounExpr    filler {##.setType(SlotPattern);}
            ;

//TODO MARK: what is typeParamList for?  it appears to come right after "def name"
typeParamList:  "["^ typePatterns br "]" ; // should have a br before the "]"

typePatterns:   (nounExpr optGuard ("," typePatterns)?)? ;

scriptPair:     "{"! methodList matcherList  "}"! ;

methodList:     ( method br )* {##=#([List], ##);} ;
matcherList:    ( matcher br )* {##=#([List], ##);} ;

method:         doco ( "method"^ methodTail {##.setType(EMethod);}
                     | "to"^ methodTail getPocket["easy-return"] {##.setType(ETo);}
                     ) ;
methodTail:     optVerb parenParamList optResultGuard block ;

optVerb:        verb | {##=#([FunctionVerb]);} ;

matcher:        "match"^ pattern block  {##.setType(EMatcher);} ;

patterns:       (pattern (","! patterns)?)? ;
parenParams:    "("! patterns br ")"! ;
patternList:    patterns {##=#([List],##);} ;
parenParamList: parenParams {##=#([List],##);} ;

optFinally:     "finally"! block
            |   {##=#([Absent],##);}
            ;

optGuard:       ":"! guard | {##=#([Absent]);} ;
optResultGuard: ":"! guard 
            |   reversePocket["explicit-result-guard",
                              "Missing result guard.",
                              "You must specify a result guard or disable \"explicit-result-guard\"."]
                {##=#([Absent]);} ;

interfaceExpr:  "interface"! (objName | STRING)
                //optGuard
                (   iguards
                    multiExtends
                    oImplements
                    iscript
                |   // NOTE this greedily consumes a following guard, thereby
                    // accepting things e.y cannot
                    parenParamDescList ((":") => ":"! guard | ) // function -- XXX wrong tree
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

iscript:        "{"^ (messageDesc br)* "}"! {##.setType(List);} ;

messageDesc:    doco ("to"^ | "method"^ | "on"^) optVerb parenParamDescList optGuard
                 {##.setType(MessageDescExpr);}
            ;

paramDesc:      (justNoun | "_" {##.setType(Absent);}) optGuard    
                {##=#([ParamDescExpr],##);}
            ;
paramDescs:     (paramDesc (","! paramDescs)?)? ;
parenParamDescList:     
                "("! paramDescs br ")"!   {##=#([List],##);} ;

doco:       DOC_COMMENT | {##=#([DOC_COMMENT]);} ;

block:          "{"! (seq | {##=#([SeqExpr],##);}) "}"! ;

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

assignOp:       "//=" | "+="  | "-="  | "*="  | "/="
            |   "%="  | "%%=" | "**=" | ">>=" | "<<="
            |   "^="  | "|="  | "&="
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
        |   "["^ args "]"! {##.setType(GetExpr);}
        |   "<-"^ { ##.setType(SendExpr); }
                  ( parenArgs { ##.setType(FunSendExpr); }
                  | verb (("(") => parenArgs // )
                         | pocket["verb-curry"]! { ##=#([CurryExpr], ##);})
                  // | "::"^ ("&")? prop // XXX design tree
                  )
        |   "::"^ ( "&"! prop {##.setType(PropertySlotExpr);}
                  | prop      {##.setType(PropertyExpr);} )
        )*
    ;

parenArgs:      "("! args ")"!  ;
lambdaArgs:      "("! args ")"! (sepword! block)?  ; //(block)? | block  ;

parenArgsList:  parenArgs {##=#([List],##);} ;

sepword:    IDENT | reserved | "else" | "catch" | "finally"
            |  "try" | "->" ;

args:        (seq (","! args)?)? ;

prim:           literal
            |   basic
            |   (IDENT QUASIOPEN) =>
                quasiParser quasiString  {##=#([QuasiExpr],##);}
            |   nounExpr
            |   parenExpr (quasiString   {##=#([QuasiExpr],##);}  )?
            |   quasiString              {##=#([QuasiExpr,"simple"],
                                               [Absent],##);}
            |   URI                      {##=#([URIExpr],##);}
            |   "["^
                (   (seq "=>" | "=>") => assocs
                                                {##.setType(MapExpr);}
                |   args                        {##.setType(ListExpr);}
                )  "]"!
            |   block          {##=#([HideExpr],##);}
            ;

assocs:    (assoc (","! assocs)?)? ;

assoc:          seq "=>"^ seq {##.setType(Assoc);}
            |   "=>"^ ( nounExpr
                      | slotExpr
                      | "def" nounExpr
                        {throwSemanticHere("Reserved syntax: forward export");}
                      ) {##.setType(Export);}
            ;

//Property names for use e.g., with the :: syntax.
prop:           pocket["dot-props"]! ( IDENT | STRING ) ;

// a method selector
verb:           IDENT | STRING  ;

literal:    (STRING | CHAR_LITERAL | INT | FLOAT64 | HEX | OCTAL)
            {##=#([LiteralExpr],##);} ;


// a valid guard is a nounExpr or parenExpr, optionally followed by [args]*
guard:
    (nounExpr | parenExpr)
    // NOTE the greedy consumption of getters.  This accepts more than e.y
    (options{greedy=true;}: "["^ args "]"! {##.setType(GetExpr);})*
    ;

catcher:        "catch"^ pattern block {##.setType(EMatcher);} ;

optCatch:      catcher | filler ;

// Patterns
pattern:        listPatt ("?"^ order  {##.setType(SuchThatPattern);}  )? ;
// XXX allow nested SuchThatPatterns for completeness

listPatt:
        eqPatt
    |   "via"^ parenExpr listPatt {##.setType(ViaPattern);}
    |   "["^
        (   ((key br)? "=>") => mapPatts br "]"! {##.setType(MapPattern);}
            ("|"^ listPatt {##.setType(TailPattern);} )?

        |   patterns br "]"! {##.setType(ListPattern);}
             ("+"^  listPatt {##.setType(TailPattern);} )?
        )
    ;

eqPatt:         (IDENT QUASIOPEN) =>
                quasiParser quasiString          {##=#([QuasiPattern],##);}
            |   nounExpr ( parenParams           pocket["call-pattern"]
                                                 {##=#([FunCallPattern],##);}
                         | "["^ patterns "]"!    pocket["call-pattern"]
                                                 {##.setType(GetPattern);}
                         | "."! verb parenParams pocket["call-pattern"]
                                                 {##=#([CallPattern],##);}
                         |   optGuard            {##=#([FinalPattern],##);}
                         )
            |   "_"^ ( ":"! guard       {##.setType(GuardTestPattern);}
                     |                  {##.setType(IgnorePattern);}
                     )
            |   "=="^ prim              {##.setType(SamePattern);}
            |   "!="^ prim              {##.setType(SamePattern);}
            |   compareOp prim
            |   quasiString             {##=#([QuasiPattern,"simple"],
                                              [Absent],##);}
            |   parenExpr ( quasiString           {##=#([QuasiPattern],##);}
                          | parenParams           pocket["call-pattern"]
                                                  {##=#([FunCallPattern],##);}
                          | "."! verb parenParams pocket["call-pattern"]
                                                  {##=#([CallPattern],##);} 
                          )
            |   keywordPatt
            |   slotPatt
            ;

// namePatts are patterns that have an inherent name, and so can be used in
// map-pattern imports.
// eqPatt accepts everything this does, but has additional cases meaning this
// cannot be used as an alternative in eqPatt.
namePatt:       nounExpr optGuard    {##=#([FinalPattern],##);}
            |   keywordPatt
            |   slotPatt
            ;

justNoun:       IDENT
            |   "::"! pocket["noun-string"]! (STRING | IDENT)
            |   URIGetter
            ;

// nounExpr happens to be the leaf which it is handy to hang the source hole
// rule on
nounExpr:       justNoun {##=#([NounExpr],##);}
            |   sourceHole
            ;

sourceHole:     SOURCE_VALUE_HOLE
                  {##.setType(INT);##=#([QuasiLiteralExpr],##);}
            |   SOURCE_PATTERN_HOLE
                  {##.setType(INT);##=#([QuasiPatternExpr],##);}
            ;

key:            parenExpr | literal ;

parenExpr:      "("! seq ")"! ;

mapPatts:       (mapPattern (","! mapPatts)?)? ;

mapPattern:       mapPatternAddressing (":="^ order {##.setType(MapPatternOptional);}
                                       |            {##=#([MapPatternRequired],##);}) ;

mapPatternAddressing: key br "=>"^ pattern {##.setType(MapPatternAssoc);}
                    | "=>"^ namePatt       {##.setType(MapPatternImport);}
                    | "=>"^ "def" {throwSemanticHere("Reserved syntax: forward export");} nounExpr
                    ;

// QUASI support
quasiParser:    parenExpr
            |   IDENT {##=#([QuasiParserExpr],##);}
            ;

quasiString:    QUASIOPEN!
                (   exprHole
                |   pattHole
                |   QUASIBODY
                |   keywordError
                )*
                QUASICLOSE!  // NOTE: '`' is the QUASICLOSE token in the quasi
                             // lexer
            ;

exprHole:       DOLLAR_CURLY^
                seq {##.setType(QuasiExprHole);}
                "}"!
            |   DOLLAR_IGNORE {throwSemanticHere("Cannot have ignore as an expression hole.");}
            |   DOLLAR_IDENT {##.setType(STRING); ##=#([QuasiExprHole],#([NounExpr],##));}
            ;

pattHole:       AT_CURLY^
                br pattern br {##.setType(QuasiPatternHole);}
                "}"!
            |   AT_IGNORE  {##=#([QuasiPatternHole],#([IgnorePattern]));}
            |   AT_IDENT  {##.setType(STRING);##=#([QuasiPatternHole],#([FinalPattern],#([NounExpr], ##),#([Absent])));}
            ;

keywordError:   k:ERROR_QUASI_KEYWORD
                {throwSemanticHere("unexpected keyword " + quoteForMessage(k.getText()) + " in quasi hole");}
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
