;;; ad-javap-mode.el --- Javap major mode
;;; Version: 9
;;; URL: http://github.com/hiredman/javap-mode

;; Copyright (C) 2011 Kevin Downey

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; This code is taken from
;; https://github.com/hiredman/javap-mode/blob/master/javap-mode.el. This
;; package provides a mode for code disassembled by `javap', but also
;; installs a hook for disassembling the files. However, this is not
;; sufficient (for instance, it does not disassemble class files
;; inside jars).
;;
;; So this file is essentially the same as the original, but the
;; unconditional installation of `javap-mode''s hook is removed, since
;; `autodisass-java-bytecode' offers the same functionality (and some more).

;;; Code:


(defconst ad-javap-font-lock-keywords
  (eval-when-compile
    `(
      ("line [0-9]+: [0-9]+" . font-lock-comment-face)
      ("\\<[a-zA-Z]+\\.[a-zA-Z0-9._]*[A-Z]+[a-zA-Z0-9/.$]*\\>" . font-lock-type-face) ;; borrowed from clojure-mode
      ("\\<[a-zA-Z]+/[a-zA-Z0-9/_]*[A-Z]+[a-zA-Z0-9/$]*\\>" . font-lock-type-face)
      ("[0-9]+:" . font-lock-comment-face)
      ("\\(#.+\\)" . font-lock-comment-face)
      ("\\(\\w\\|_\\)+(" . font-lock-preprocessor-face)
      (")" . font-lock-preprocessor-face)
      ("\\(invoke\\w+\\)" . font-lock-function-name-face)
      (,(regexp-opt '("boolean" "int" "void" "char"))
       . font-lock-type-face)
      (,(regexp-opt '("Exception table"
                      "LocalVariableTable"
                      "LineNumberTable")) . font-lock-warning-face)

      (".load_\\w+" . font-lock-keyword-face)

      (".load" . font-lock-keyword-face)

      (".store_\\w+" . font-lock-keyword-face)

      (".const_[0-9]+" . font-lock-keyword-face)

      (".return" . font-lock-keyword-face)

      (,(regexp-opt
         '("ifne" "athrow" "new" "dup" "aastore" "anewarray" "ifnull" "ifeq" "ifnonnull"
           "getstatic" "putfield" "getfield" "checkcast" "astore" "aload" "ldc" "goto" "putstatic"
           "pop" "instanceof" "ldc_w" "sipush" "bipush" "aaload" "bastore" "baload" "arraylength"
           "castore" "saload" "lastore" "daload" "dastore" "ifle" "istore" "lookupswitch" "iinc"
           "if_icmpge" "isub" "if_icmpgt" "if_acmpne" "iflt" "if_icmplt" "if_icmple" "dcmpg"
           "dcmpl" "ldc2_w" "lcmp" "fcmpg" "fcmpl" "ifge" "jsr" "ifgt" "ret" "aconst_null" "swap"
           "if_acmpeq" "dup_x2"))
       . font-lock-keyword-face)

      (".add" . font-lock-keyword-face)

      (,(regexp-opt
         '("public" "static" "final" "volatile" ";" "transient" "class" "extends" "implements"
           "synchronized" "protected" "private" "abstract" "interface" "Code:" "throws"))
       . font-lock-comment-face)
      ;;      ("\\(\\w+\\)" . font-lock-keyword-face)
      ))
  "Default expressions to highlight in ad-javap mode.")

(defvar ad-javap-mode-syntax-table′ (make-syntax-table)
  "Syntax table for use in ad-javap-mode.")

 ;;;###autoload
(define-derived-mode ad-javap-mode fundamental-mode "ad-javap"
  "A major mode for viewing javap files."
  :syntax-table ad-javap-mode-syntax-table′
  (modify-syntax-entry ?_ "w" ad-javap-mode-syntax-table′)
  (modify-syntax-entry ?# "<" ad-javap-mode-syntax-table′)
  (modify-syntax-entry ?\n ">" ad-javap-mode-syntax-table′)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "#")
  (set (make-local-variable 'font-lock-defaults) '(ad-javap-font-lock-keywords)))

(provide 'ad-javap-mode)

;;; ad-javap-mode.el ends here
