// Copyright 2007 Kevin Reid, under the terms of the MIT X license
// found at http://www.opensource.org/licenses/mit-license.html ...............

import antlr.CommonToken;

/** Modified print behavior over CommonToken; in particular, does not print the numeric token type, which is not useful information to the user; also knows how to reverse text transformations for particular token types. */
public class ExtToken extends CommonToken {
    
    /** this is the constructor invoked by CharScanner */
    public ExtToken() { super(); }
    
    public String getOriginalText() {
        if (type == ETokenTypes.EOF) {
            return "<EOF>";
        } else if (type == ETokenTypes.SOURCE_VALUE_HOLE) {
            return "<$-hole #" + getText() + ">";
        } else if (type == ETokenTypes.SOURCE_PATTERN_HOLE) {
            return "<@-hole #" + getText() + ">";
        } else if (type == ETokenTypes.DOLLAR_IDENT) {
            return "$" + getText();
        } else if (type == ETokenTypes.AT_IDENT) {
            return "@" + getText();
        } else if (type == ETokenTypes.DOLLARESC) {
            return "$\\" + getText(); // XXX escape
        } else if (type == ETokenTypes.CHAR_LITERAL) {
            return "'" + getText() + "'"; // XXX escape
        } else if (type == ETokenTypes.STRING) {
            return "\"" + getText() + "\""; // XXX escape
        } else if (type == ETokenTypes.HEX) {
            return "0x" + getText();
        } else if (type == ETokenTypes.OCTAL) {
            return "0" + getText();
        } else if (type == ETokenTypes.URI) {
            return "<" + getText() + ">";
        } else if (type == ETokenTypes.URIGetter) {
            return "<" + getText() + ">";
        } else if (type == ETokenTypes.URIStart) {
            return "<" + getText();
        } else {
            return getText();
        }
    }
    
    public String toString() {
        return "<\"" + getOriginalText() + "\" @ " + getLine() + ":" + getColumn() + ">";
    }
}