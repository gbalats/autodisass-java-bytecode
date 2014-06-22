autodisass-java-bytecode
========================

This package enables the automatic disassembly of Java bytecode inside
Emacs buffers.

Disassembly can happen in two cases:

1. when opening a Java .class file
2. when extracting a .class file inside a jar

When `javap-mode` is available, it is automatically selected for the
current Java bytecode-containing buffer.

In any case, `javap` must be installed in the system for this
extension to have any effect, since that is the tool that actually
performs the disassembly.


To use, save [this .el file](autodisass-java-bytecode.el) to a
directory in your *load-path*, and add the following to your `.emacs`:

    (require 'autodisass-java-bytecode)
