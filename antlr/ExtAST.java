// Copyright 2006-2007 Kevin Reid, under the terms of the MIT X license
// found at http://www.opensource.org/licenses/mit-license.html ...............

import antlr.CommonAST;
import antlr.collections.AST;
import antlr.Token;

/** Keeps source position information. It seems strange that ANTLR does not do this automatically. */
public class ExtAST extends CommonAST {
  private int line   = -1;
  private int column = -1;
  
  public ExtAST() { super(); }
  public ExtAST(Token token) { super(token); }
  
  public void initialize(Token token) {
    super.initialize(token);
    line   = token.getLine();
    column = token.getColumn();
  }
  
  public int getLine() {
    if (line != -1) {
      return line;
    } else {
      AST c = getFirstChild();
      if (c != null)  {
        return c.getLine();
      } else {
        return 0;
      }
    }
  }
  public int getColumn() {
    if (column != -1) {
      return column;
    } else {
      AST c = getFirstChild();
      if (c != null)  {
        return c.getColumn();
      } else {
        return 0;
      }
    }
  }
}