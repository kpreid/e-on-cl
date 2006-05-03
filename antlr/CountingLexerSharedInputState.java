// Copyright 2006 Kevin Reid, under the terms of the MIT X license
// found at http://www.opensource.org/licenses/mit-license.html ...............

import java.io.InputStream;
import java.io.Reader;
import antlr.LexerSharedInputState;
import antlr.InputBuffer;

public class CountingLexerSharedInputState extends LexerSharedInputState {
  public CountingLexerSharedInputState(InputBuffer inbuf) {
    super((CountingCharBuffer)inbuf);
  }

  public CountingLexerSharedInputState(Reader in, int position) {
    this(new CountingCharBuffer(in, 0));
  }
  
  public int getPosition() { return ((CountingCharBuffer)input).getPosition(); }
}
