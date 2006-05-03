// Copyright 2006 Kevin Reid, under the terms of the MIT X license
// found at http://www.opensource.org/licenses/mit-license.html ...............

import java.io.Reader;
import antlr.CharBuffer;

/** Keeps track of the current character position from the beginning of the input. */
class CountingCharBuffer extends CharBuffer {
  private int position = 0;
  
  public CountingCharBuffer(Reader input, int initPosition) {
    super(input);
    position = initPosition;
  }
  
  public int getPosition() { return position; }
  
  public void consume() {
    super.consume();
    position++;
  }
  
  public void rewind(int mark) {
    syncConsume();
    // int markerOffset = mark(); rewind(markerOffset); <- this should work if we need to stop accessing the markerOffset field
    position -= markerOffset - mark;
    super.rewind(mark);
  }
  
  public void reset() {
    position = 0; // XXX is this correct? should it reset to initPosition instead?
  }
}