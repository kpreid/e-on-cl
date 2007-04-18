#!/bin/sh

# Copyright 2006-2007 Kevin Reid, under the terms of the MIT X license
# found at http://www.opensource.org/licenses/mit-license.html ................

set -e

rm -f *.class \
      {E,EALexer,QuasiLexer}TokenTypes.{txt,java} \
      {EALexer,QuasiLexer,EParser}.java
