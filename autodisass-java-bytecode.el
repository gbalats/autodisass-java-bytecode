;;; autodisass-java-bytecode --- Automatically disassemble Java bytecode

;;; Commentary:

;; This package enables automatic disassembly of Java bytecode.
;;
;; Disassembly can happen in two cases:
;; (a) when opening a Java .class file
;; (b) when disassembling a .class file inside a jar
;;
;; When `javap-mode' is available, it is automatically selected for the
;; current Java bytecode-containing buffer.
;;
;; In any case, `javap' must be installed in the system for this
;; extension to have any effect, since that is the tool that actually
;; performs the disassembly.

;;; Code:


;; Add handlers for automatically disassembling .class files
(add-to-list 'file-name-handler-alist
             '("\\.class$" . autodisass-java-handler))

;; Add hook for automatically disassembling .class files inside jars
(add-hook 'archive-extract-hooks
          (lambda () (cond ((and (executable-find "javap")
                                 (string-match "\\.class$" (buffer-file-name)))
                            (autodisass-inside-jar)))))


;;---------------------------------------------------------------
;; Auto-extracting and disassembly of Java bytecode inside jars
;;---------------------------------------------------------------

(defun autodisass-inside-jar ()
  "Disassembles a Java class-file inside a jar archive."
  (let*
      ((components (split-string (buffer-file-name) ":"))
       (jar-file   (car components))
       (class-file (cadr components)))

    ;; Erase previous contents
    (erase-buffer)

    ;; Now disassemble bytecode inside empty buffer
    (autodisass-bytecode-buffer class-file jar-file)

    ;; Display some info on what just happened
    (message "Disassembled %s" class-file)))


;;------------------------------
;; Java Bytecode Disassembly
;;------------------------------

(defun autodisass-bytecode-buffer (class-file &optional jar-file)
  "Disassembles a Java CLASS-FILE inside the current buffer, using `javap'.

The JAR-FILE argument is non-nil if the disassembly is happening
inside a jar archive, during auto-extraction."

  (let* ((inside-jar-p (not (eq jar-file nil)))
         (dirname      (file-name-directory class-file))
         (filename     (file-name-nondirectory class-file))
         (classname    (file-name-sans-extension filename))
         (classpath    dirname))

    ;; fully qualify class name if inside jar; adjust classpath as
    ;; well
    (when inside-jar-p
      (setq classpath jar-file)
      (setq classname (replace-regexp-in-string
                       "/" "." (file-name-sans-extension class-file)))
      (rename-buffer (concat classname
                           " (" (file-name-nondirectory jar-file) ")")))

    ;; Disassemble .class file
    (call-process "javap" nil t nil "-private" "-verbose"
                  "-classpath" classpath classname)

    ;; Set buffer's filename
    (setq buffer-file-name
          (if inside-jar-p (concat jar-file ":" class-file) class-file))

    ;; Set read-only mode for this buffer
    (setq buffer-read-only t)

    ;; Mark the buffer as unmodified
    (set-buffer-modified-p nil)

    ;; Jump to the beginning of the buffer
    (goto-char (point-min))

    ;; Switch to `javap-mode'
    (when (fboundp 'javap-mode)
      (javap-mode))))


;;------------------------------
;; Java bytecode handlers
;;------------------------------

(defun autodisass-java-handler (op &rest args)
  "Handle .class files by putting the output of `javap' in the buffer.

OP is the name of the I/O primitive to be handled; ARGS are the
arguments that were passed to that primitive.  This function only
applies to `get-file-buffer' operations."
  (cond
   ((and (eq op 'get-file-buffer) (executable-find "javap"))
     (let* ((class-file  (car args))
            (buffer-name (file-name-nondirectory class-file)))

       ;; Create new buffer to hold the output of `javap'
       (with-current-buffer (generate-new-buffer buffer-name)

         ;; Disassemble bytecode inside buffer
         (autodisass-bytecode-buffer class-file)

         ;; Display some info on what just happened
         (message "Disassembled %s" class-file)

         ;; Return current buffer
         (current-buffer))))

   ((autodisass-java--default-handler op args))))


(defun autodisass-java--default-handler (operation args)
  "Run the real handler to avoid automatic disassembly.

OPERATION is the name of the I/O primitive to be handled; ARGS
are the arguments that were passed to that primitive."
  (let ((inhibit-file-name-handlers
         (cons 'autodisass-java-handler
               (and (eq inhibit-file-name-operation operation)
                          inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))


(provide 'autodisass-java-bytecode)

;;; autodisass-java-bytecode ends here
