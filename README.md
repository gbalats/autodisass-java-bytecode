[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]

autodisass-java-bytecode
========================

This package enables the automatic disassembly of Java bytecode inside
Emacs buffers. It was inspired by a
[blog post](http://nullprogram.com/blog/2012/08/01/) of
[Christopher Wellons](https://github.com/skeeto).

Disassembly can happen in two cases:

1. when opening a Java .class file
2. when extracting a .class file inside a jar

When `javap-mode` is available, it is automatically selected for the
current Java bytecode-containing buffer.

In any case, `javap` must be installed in the system for this
extension to have any effect, since that is the tool that actually
performs the disassembly.


## Installation

You can install this package using the `package.el` built-in package
manager in Emacs. It is available on [MELPA](http://melpa.org/#/) and
[MELPA Stable](http://stable.melpa.org/#/) repos.

If you have these enabled, simply run:

    M-x package-install [RET] autodisass-java-bytecode [RET]


Alternatively, you can save
[this .el file](autodisass-java-bytecode.el) to a directory in your
*load-path*, and add the following to your `.emacs`:

    (require 'autodisass-java-bytecode)

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[melpa-badge]: http://melpa.org/packages/autodisass-java-bytecode-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/autodisass-java-bytecode-badge.svg
[melpa-package]: http://melpa.org/#/autodisass-java-bytecode
[melpa-stable-package]: http://stable.melpa.org/#/autodisass-java-bytecode
