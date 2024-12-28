;;; binfile.el --- Disassemble binary files     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; An extendable package for examining binary files.  It can disassemble many
;; types of ELF files and postprocess the results to be more easily readable
;; (e.g. it can parse relocations reported by objdump and output them
;; intermixed with code).
;;
;; The main command is `binfile-disassemble', which prompts for a function name
;; (by default, the function at point) and, if it can't be guessed, a binary
;; file.  The binary file is by default provided by `compiled-file.el' library.
;;
;; Using `compdb-output-filename' as `compiled-file-function' allows
;; automatically finding binary (.o) files for current buffer from
;; "compile_commands.json" file.
;;
;; The other commands are:
;;
;; - `binfile-insert-data'
;;
;;   Insert data (a symbol, a section, or an address range) from a binary file
;;   into the current disassembly buffer.  It can be used to examine .data,
;;   .rodata sections etc.
;;
;; - `binfile-diff'
;;
;;   Display a diff buffer for examining a difference between two disassembled
;;   binary files.
;;
;; - `binfile-symbol-info'
;;
;;   Prompt for a symbol and a binary filename, and display information about
;;   that symbol.

;;; Code:

(require 'cl-lib)
(require 'compiled-file)
(require 'map)
(require 'objdump)
(require 'project)
(require 'pulse)
(require 'rx)
(require 'seq)
(require 'subr-x)
(require 'which-func)

(defgroup binfile nil "Disassemble binary files."
  :group 'languages
  :group 'tools)

(defvar binfile-postprocessing-functions
  '(
    binfile-postprocess-relocations
    binfile-postprocess-local-jumps
    binfile-postprocess-strip-addresses
    binfile-postprocess-boring-comments
    binfile-postprocess-numeric-to-symbolic-references
    binfile-postprocess-unused-symbolic-local-references
    )
  "List of functions that postprocess objdump's disassembly regions.
Each function is called with three arguments (BEG END NAME),
where BEG and END are region bounds and NAME is the name of the
symbol disassembled in that region (function name).")

(defvar binfile-arch-postprocessing-function-alist nil
  "Alist of functions to call per file format.
Keys are regexps which are matched against the file format of
binary file.  Values are functions which are called with three
arguments (BEG END NAME).  BEG and END are buffer regions to
process, NAME is the function name in that region.")

(defvar binfile-disassembly-prologue nil
  "Value to insert at buffer beginning.")

(defvar binfile-relocation-handler-alist nil
  "Alist of relocation handler functions.
Keys are regexps which are matched against the relocation's type.
The handler functions are called with five arguments:
\(RELOC-TYPE RELOC RELOC-ADDRESS RELOC-ADDEND FUNCTION-NAME)
where
RELOC-TYPE is the relocation type string,
RELOC is the relocated symbol,
RELOC-ADDRESS is the relocation's address,
RELOC-ADDEND is either nil or the relocation's offset,
FUNCTION-NAME is the function in which this relocation was found.

When the handler function is called, the point is at the
beginning of the line before the relocation line, and the buffer
is narrowed to just this line.

The handler function should return non-nil value if it processed
the relocation, i.e. performed a replace, and nil otherwise.
When it returns non-nil, the relocation line is removed from the buffer.

Values in this alist can also be lists of functions.  In this
case, the functions are called in order, until one of them returns non-nil.")

(defvar binfile-data-relocation-handler-alist nil
  "Alist of relocation handler functions for raw data dumps.
Keys are regexps which are matched against the relocation’s type.
The handler functions are called with three arguments:
  (RELOC-TYPE RELOC RELOC-ADDEND)

where
  RELOC-TYPE is the relocation type string,
  RELOC is the relocated symbol,
  RELOC-ADDEND is either nil or the relocation’s offset,

The handler function must return a cons (NBYTES . DIRECTIVES),
where NBYTES says how many bytes that relocation occupies and
DIRECTIVES is a list of conses (DIRECTIVE . STRING) which are
inserted, in order, in a place where the relocation ocurred.

E.g. for R_X86_64_64 relocation the handler function should
return (8 . ((\".8byte\" . \"symbol\"))).")

(defcustom binfile-replace-insert-nondestructively 25000
  "Replace buffer contents nondestructively if it's size is less than this.

When disassembling, the output of objdump is inserted into a
temporary buffer; if the size of this temporary buffer is less
than this, and if the size of the current *disassembly* buffer is
less than this value, the contents of *disassembly* are replaced
with the contents of the temporary buffer with
`replace-buffer-contents'.  This is slow for large buffers, but
has the advantage of properly preserving point.

If the size of any of the two buffers is larger than this, the
contents are replaced destructively and point is not preserved."
  :type 'integer)

(defvar binfile-scan-for-symbol-at-point-limit 40000
  "Search for symbol at point only at most this many times.
If an object file contains more than that many symbols, a scan
for demangled symbols in it is not performed when searching for
the symbol at point.")

(defvar binfile-symbol-transform-function #'identity
  "Function which transforms a symbol before putting it to minibuffer.
Normally this is `identity', e.g. no transformation is performed;
but this can be set e.g. to `regexp-quote' to work with custom
completion providers.")

(defvar binfile-disassembly-hook nil "Hook called after disassembly.")

(defconst binfile-disassembly-buffer "*disassembly*"
  "Buffer with disassembled code.")

(defconst binfile-disassembly-diff-buffer-a "*disassembly A*"
  "Name of the first buffer for diffing disassemblies.")

(defconst binfile-disassembly-diff-buffer-b "*disassembly B*"
  "Name of the second buffer for diffing disassemblies.")

(defvar binfile-symbol-history nil
  "History variable for `binfile-disassemble'.")

(defvar binfile-raw-symbol-history nil
  "History variable for `binfile-insert-data'.")

(defvar binfile--file)
(defvar binfile-mode)


;; Symbol at point; reading symbols

(defun binfile--demangled-symbols (filename &optional include-empty)
  "Read demangled symbols from file named FILENAME.
If INCLUDE-EMPTY is non-nil, don't filter-out empty (no size
prop) symbols."
  (map-apply
   (lambda (_k v) (plist-get (car v) :demangled))
   (map-filter
    (lambda (_name defs)
      ;; Only symbols with non-zero size
      (cl-notany (if include-empty #'ignore #'zerop)
                 (mapcar (lambda (x) (plist-get x :size)) defs)))
    (objdump-read-symtab filename))))

(defun binfile--symbol-and-offset-at-point ()
  "Get symbol and offset at point, if available.

The return value is either nil or a cons (SYMBOL . OFFSET).
OFFSET is always an integer.  SYMBOL is always demangled."
  (let ((pt (point)) found bounds)
    (when-let* ((symtab (and binfile--file
                             (objdump-read-symtab binfile--file))))
      (when (< (hash-table-count symtab)
               binfile-scan-for-symbol-at-point-limit)
        (catch 'done
          (save-excursion
            (goto-char (line-beginning-position))
            (map-do
             (lambda (symbol data)
               (save-excursion
                 (let ((demangled (plist-get (car data) :demangled)))
                   (when (string-match "^[.]hidden " demangled)
                     (setq demangled (replace-match "" nil nil demangled)))
                   (when (and (search-forward demangled (line-end-position) t)
                              (<= (match-beginning 0) pt (match-end 0)))
                     (setq found symbol
                           bounds (cons (match-beginning 0) (match-end 0)))
                     (throw 'done t)))))
             symtab)))))

    (save-match-data
      (when-let* ((bounds (or bounds (bounds-of-thing-at-point 'symbol)))
                  (symbol (or found (thing-at-point 'symbol t)))
                  (offset 0))
        (when (string-match "\\([+-]\\)0x\\([[:xdigit:]]+\\)$" symbol)
          (setq offset (* (string-to-number (match-string 2 symbol) 16)
                          (if (string= "+" (match-string 1 symbol))
                              1 -1))
                symbol (replace-match "" nil nil symbol)))
        (when (zerop offset)
          (save-excursion
            (goto-char (cdr bounds))
            (when (looking-at "\\([+-]\\)0x\\([[:xdigit:]]+\\)")
              (setq offset (* (string-to-number (match-string 2) 16)
                              (if (string= "+" (match-string 1))
                                  1 -1))))))
        (cons (objdump-demangle symbol) (or offset 0))))))

(defvar binfile--symbol-alist-cache nil
  "Cache of pretty symbol names to mangled names.
Keys are conses (FILENAME . CALLER), where CALLER is the argument
to `binfile--get-symbol-and-filename', a Lisp symbol.

Values are conses (MTIME . ALIST), where MTIME is the FILENAME's
modification time and ALIST is an alist of
  (TRANSFORMED-SYMBOL . MANGLED-SYMBOL); TRANSFORMED-SYMBOL is
the result of calling symbol-transformer function with
MANGLED-SYMBOL as an argument.")

(defun binfile--get-symbol-and-filename (prompt &optional interactive
                                                symbol-transformer
                                                initial-input
                                                filename
                                                caller)
  "Get a symbol and filename using `completing-read' with PROMPT.

The return value is a list (MANGLED-SYMBOL FILENAME).

If INTERACTIVE is a number, the user is asked to select the file,
otherwise an attempt is made to guess it first.

If SYMBOL-TRANSFORMER is provided, it should be a function that
accepts two arguments (MANGLED-SYMBOL FILENAME) and returns a
string that is presented to the user in the minibuffer for each
symbol.

If the minor mode `binfile-mode' is enabled, this function tries
to get the symbol at point.  If it's found, it is used as initial
input for `completing-read'.  Otherwise, INITIAL-INPUT is used.

If FILENAME is provided, it is returned as is, and possible
candidates are read from that file.

CALLER should be a symbol for the calling function.  It is used
for caching purposes."
  (or symbol-transformer
      (setq symbol-transformer (lambda (sym _filename)
                                 (objdump-demangle sym))))

  (let* ((filename (or filename
                       (and (numberp interactive)
                            (read-file-name "Select compiled file: "))
                       (and binfile-mode binfile--file)
                       (compiled-file)
                       (read-file-name  "Select compiled file: ")))

         (existing-symbol-at-pt
          (and binfile-mode
               (car (binfile--symbol-and-offset-at-point))))

         (initial-input (or existing-symbol-at-pt initial-input))

         (all-symbols
          (or
           (when-let* ((cache (alist-get (cons filename caller)
                                         binfile--symbol-alist-cache
                                         nil nil #'equal))
                       (mtime (binfile--file-mtime filename)))
             (and (equal mtime (car cache)) (cdr cache)))
           (mapcar (lambda (sym)
                     (cons (funcall symbol-transformer sym filename)
                           sym))
                   (map-keys (objdump-read-symtab filename)))))

         selected)
    (setf (map-elt binfile--symbol-alist-cache
                   (cons filename caller))
          (cons (binfile--file-mtime filename) all-symbols))
    (prog1
        (list (objdump-mangle
               (progn (setq selected
                            (completing-read
                             prompt all-symbols
                             nil t
                             (and initial-input
                                  (funcall
                                   binfile-symbol-transform-function
                                   initial-input))
                             'binfile-symbol-history))
                      (alist-get selected all-symbols nil nil #'string=)))
              filename)
      (when (equal selected (car binfile-symbol-history))
        (pop binfile-symbol-history))
      (add-to-history
       'binfile-symbol-history
       (funcall binfile-symbol-transform-function selected)))))


;; Relocation processing

(defvar binfile--reloc-regexp
  (rx bol (1+ ?	)
      ;; Relocation address, in hex
      (group (1+ hex-digit)) ?: (1+ space)

      ;; Relocation type, like "R_X86_64_PC32"
      (group (1+ (not space))) (1+ space)

      ;; Symbolic name and addend
      (or
       (and
        (group-n 3 (+ (not (in ?- ?+ "\n"))))
        (group-n 4 (? (in ?- ?+) "0x" (1+ hex-digit))))
       (and
        (group-n 3 (minimal-match (+ (not (in "\n")))))
        (group-n 4 (in ?- ?+) "0x" (1+ hex-digit))))
      eol)
  "Regular expression matching relocations in objdump's disassembly output.
The groups are:
1 - address
2 - type
3 - symbol
4 - addend")

(defun binfile-postprocess-relocations (_beg _end function-name)
  "Process relocations in function named FUNCTION-NAME in narrowed buffer."
  (while (re-search-forward binfile--reloc-regexp nil t)
    (let* ((reloc (match-string 3))
           (reloc-type (match-string 2))
           (reloc-address (string-to-number (match-string 1) 16))
           (reloc-addend (match-string 4))
           (reloc-addend-nr
            (and reloc-addend
                 (string-to-number
                  (replace-regexp-in-string "0x" "" reloc-addend)
                  16)))
           (did-replace nil)
           (handler (cdr (cl-find-if (lambda (ent)
                                       (string-match-p (car ent) reloc-type))
                                     binfile-relocation-handler-alist))))
      (when handler
        (unless (listp handler)
          (setq handler (list handler)))
        (save-excursion
          (save-restriction

            (forward-line -1)
            (goto-char (line-beginning-position))

            (narrow-to-region (line-beginning-position) (line-end-position))

            (while (and handler (null did-replace))
              (let ((func (pop handler)))
                (setq did-replace
                      (funcall
                       func reloc-type reloc reloc-address reloc-addend-nr
                       function-name))

                ;; Add information about this relocation on this line, for
                ;; debugging
                (when did-replace
                  (put-text-property
                   (line-beginning-position) (line-end-position) 'binfile-reloc
                   `(:type ,reloc-type
                           :name ,reloc
                           :address ,reloc-address
                           :addend ,reloc-addend
                           :handler ,func))))))))
      (when did-replace
        (delete-region (line-beginning-position) (1+ (line-end-position)))))))


;; Other postprocessing

(defun binfile--local-jump-regexp (function-name)
  "Get the regexp for a local jump in FUNCTION-NAME."
  ;; "123ac <foo()+0x30>"
  (eval `(rx
          space
          ;; target
          (group (+ hex-digit)) space

          ;; offset to current function
          ?<
          ,function-name
          (optional "+0x" (+ hex-digit))
          ?>
          )))

(defun binfile-postprocess-local-jumps (_beg _end function-name)
  "Labelize FUNCTION-NAME and rewrite jumps to symbolic jumps to those labels."
  (let* ((regexp (binfile--local-jump-regexp function-name))
         (in-comment-regexp (concat "#\\s-*" regexp))
         (label-count 0))
    (while (re-search-forward regexp nil t)
      (unless (save-match-data (looking-back in-comment-regexp
                                             (line-beginning-position)))
        (when-let* ((target-addr (match-string 1))
                    (target (save-match-data
                              (save-excursion
                                (goto-char (point-min))
                                (re-search-forward
                                 (format "^ +0*%s+:" target-addr) nil t)))))
          (setq target (move-marker (make-marker) target))
          (let ((label (format ".L%d" (cl-incf label-count))))
            (replace-match (concat " " label) nil t)
            (save-excursion
              (goto-char target)
              (goto-char (line-beginning-position))
              (insert label ":\n"))))))))

(defun binfile-postprocess-strip-addresses (_beg _end _name)
  "Strip address from all instructions."
  (while (re-search-forward "^ +[0-9a-f]+:" nil t) (replace-match "")))

(defun binfile-postprocess-boring-comments (_beg _end _name)
  "Remove boring comments."
  (while (re-search-forward "# +[0-9a-f]+ *$" nil t) (replace-match "")))

(defun binfile-postprocess-unused-symbolic-local-references
    (_beg _end function-name)
  "Remove useless symbolic references with offsets to FUNCTION-NAME.
E.g. this removes all instances of '<foo()+0x123af>'."
  (while (re-search-forward
          (format "<%s[+-]0x[0-9a-f]+>" (regexp-quote function-name))
          nil t)
    (replace-match "")))

(defvar binfile--symbolic-reference-regexp
  (rx
   space
   word-boundary
   (1+ hex-digit) space
   ?<
   (group (1+ any))
   ?>
   eol)
  "Regexp matching a symbolic reference, with hex address before.
Group 1 is the symbol.")

(defun binfile-postprocess-numeric-to-symbolic-references (_beg _end _name)
  "Convert references with address and symbol to just symbol."
  (while (re-search-forward binfile--symbolic-reference-regexp nil t)
    (replace-match " \\1")))

(defvar binfile-file-format)

(defun binfile--postprocess-region (beg end name)
  "Process disassembled region named NAME, spanning BEG END."
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region beg end)
      (goto-char beg)

      ;; Add text properties to each line, for debugging
      (save-excursion
        (cl-loop until (eobp)
                 do (put-text-property
                     (line-beginning-position) (line-end-position)
                     'binfile-original
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))
                 do (forward-line 1)))

      (dolist (func binfile-postprocessing-functions)
        (save-excursion
          (save-restriction
            (funcall func (point-min) (point-max) name))))

      (when-let* ((fmt binfile-file-format)
                  (func (cdr (cl-find-if
                              (lambda (re) (string-match-p re fmt))
                              binfile-arch-postprocessing-function-alist
                              :key #'car))))
        (save-excursion (funcall func beg end name)))

      (delete-trailing-whitespace (point-min) (point-max)))))

(defvar binfile--function-start-regexp
  ;; 00000000000fa <foo(int)>:
  (rx
   bol
   ;; Function address
   (group (+ hex-digit)) (* space)

   ;; Function name
   ?< (group (* any)) ?> ?:

   eol)
  "A regexp matching function beginning in disassembled output.
Group 1 is hex address, group 2 is function name.")

(defun binfile--replace-buffer-contents (target source)
  "Replace contents of buffer TARGET with SOURCE.
Returns TARGET."
  (let ((limit binfile-replace-insert-nondestructively)
        (pr (make-progress-reporter "Inserting output")))
    (unwind-protect
        (with-current-buffer target
          (let ((buffer-read-only nil))
            (prog1 target
              (if (and (< (buffer-size target) limit)
                       (< (buffer-size source) limit))
                  (replace-buffer-contents source)
                (erase-buffer)
                (insert-buffer-substring source)))))
      (progress-reporter-done pr))))


;; Disassembly

(defvar binfile--file nil "Current object file.")

(defvar binfile--symbol)

(defvar binfile-file-format nil
  "Disassembled file format string, e.g. \"elf64-x86-64\".")

(defun binfile--disassemble (symbol section filename start-address end-address
                                    &optional demangle)
  "Disassemble SYMBOL in SECTION of FILENAME between START-ADDRESS:END-ADDRESS.
If DEMANGLE is non-nil, don't demangle symbols."
  (setq binfile--file filename)
  (setq binfile--symbol symbol)
  (if demangle
      (setq symbol (objdump-demangle symbol))
    (setq symbol (objdump-mangle symbol)))
  (objdump-disassemble filename section start-address end-address
                       :demangle demangle
                       :reloc t)
  (goto-char (point-min))
  (when binfile-disassembly-prologue
    (insert binfile-disassembly-prologue "\n"))
  (insert "; file \"" filename "\"\n")
  (setq binfile-file-format objdump-file-format)

  (save-excursion
    ;; Postprocess all regions.
    (let ((pr (make-progress-reporter "Postprocessing disassembly"
                                      (point-min) (point-max))))
      (unwind-protect
          (while (re-search-forward binfile--function-start-regexp nil t)
            (progress-reporter-update pr (point))
            (let* ((name (match-string 2))
                   (beg (move-marker (make-marker) (1+ (match-end 0))))
                   (end (move-marker
                         (make-marker)
                         (or (save-match-data
                               (and (or
                                     (re-search-forward
                                      "^Disassembly of section" nil t)
                                     (re-search-forward
                                      binfile--function-start-regexp nil t))
                                    (match-beginning 0)))
                             (point-max)))))

              ;; Postprocess the region.
              (replace-match "\\2:")
              (binfile--postprocess-region beg end name)))
        (progress-reporter-done pr))))

  (save-excursion
    ;; Rewrite to have standard ASM .section directives.
    (while (re-search-forward "^Disassembly of section \\(.*\\):" nil t)
      (replace-match ".section \\1")))

  (save-excursion
    ;; Remove empty sections
    (while (re-search-forward "^[.]section .*$" nil t)
      (let* ((start (match-beginning 0))
             (contents-start (1+ (match-end 0)))
             (next-section-start
              (or (save-excursion
                    (and (re-search-forward "^[.]section .*$" nil t)
                         (match-beginning 0)))
                  (point-max)))
             (contents (buffer-substring contents-start next-section-start)))
        (when (< (length contents) 5)
          (delete-region start next-section-start))))))

(defvar binfile-mode)

(defvar binfile--symbol nil "Currently disassembled symbol.")

(defun binfile--buffer-visible-p (buffer)
  "Check if any window is showing BUFFER."
  (cl-some (lambda (w) (eq (window-buffer w) buffer))
           (cl-reduce #'nconc
                      (mapcar #'window-list (frame-list)))))

(defun binfile--file-mtime (filename)
  "Return modification time of file FILENAME."
  (file-attribute-modification-time (file-attributes filename)))

;;;###autoload
(defun binfile-disassemble (symbol filename &optional mangled)
  "Disassemble SYMBOL in object FILENAME.
Interactively, this selects the function at point and the proper
object file.  If there are multiple candidates for disassembly,
asks (with completion) to select the symbol to disassemble.

If MANGLED is non-nil, don't demangle symbols."
  (interactive (binfile--get-symbol-and-filename
                "Disassemble symbol: " current-prefix-arg
                nil
                (and (not binfile-mode) (or (which-function)
                                            (thing-at-point 'symbol t)))
                nil
                #'binfile-disassemble))
  (setq filename (expand-file-name filename))
  (pcase-let* ((`(,address ,size ,section)
                (if-let* ((mangled-name (objdump-mangle symbol))
                          (entry
                           (or
                            (car (gethash mangled-name
                                          (objdump-read-symtab filename)))
                            (car (gethash
                                  (format ".hidden %s" mangled-name)
                                  (objdump-read-symtab filename))))))
                    (list (plist-get entry :address)
                          (plist-get entry :size)
                          (plist-get entry :section))
                  (error "Symbol %S not found in %S" symbol filename)))
               (end-address (+ address size))
               (source-file (buffer-file-name)))
    (setq symbol (if mangled (objdump-mangle symbol)
                   (objdump-demangle symbol)))
    (when (<= end-address address)
      (setq end-address most-positive-fixnum))
    (with-current-buffer (get-buffer-create binfile-disassembly-buffer)
      (setq default-directory (file-name-directory filename))
      (let ((buffer-read-only nil)
            (target (current-buffer)))
        (with-temp-buffer
          (let ((source (current-buffer)))
            (binfile--disassemble symbol section filename address end-address
                                  (not mangled))
            (with-current-buffer target
              (set-text-properties (point-min) (point-max) nil)
              (delete-all-overlays)
              (unless (binfile--buffer-visible-p target)
                (delete-region (point-min) (point-max)))
              (binfile--replace-buffer-contents target source)))))
      (asm-mode)
      (binfile-mode +1)
      (setq buffer-read-only t)
      (setq truncate-lines t)
      (setq buffer-undo-list t)
      (display-buffer (current-buffer))

      (run-hooks 'binfile-disassembly-hook)
      (hack-local-variables)

      (when (and source-file
                 (time-less-p (binfile--file-mtime filename)
                              (binfile--file-mtime source-file)))
        (message "Note: object file is older than the source file")))))

(defun binfile-toggle-name-mangling ()
  "Disassemble the current buffer again, toggling mangling of symbol names."
  (interactive)
  (binfile-disassemble binfile--symbol binfile--file
                       (not (objdump-symbol-mangled-p binfile--symbol))))


;; Raw data inspection

(defun binfile--get-raw-data-as-directives
    (filename section start-address stop-address)
  "Get raw data from SECTION in FILENAME between START-ADDRESS:STOP-ADDRESS.
The return value is a list of conses (DIRECTIVE . VALUE), where
both DIRECTIVE and VALUE are strings to insert in the disassembly buffer."
  (pcase-let ((`(,bytes ,relocs ,real-start-address)
               (objdump-raw filename section start-address stop-address))
              (relocs-by-offset))
    (setq start-address real-start-address)

    (setq relocs-by-offset
          (mapcar (lambda (reloc) (cons (- (nth 0 reloc) start-address) reloc))
                  relocs))

    (cl-loop
     with offset = 0
     with length = (length bytes)

     with region-contains-relocs-p =
     (lambda (nbytes)
       (cl-some (lambda (off) (map-elt relocs-by-offset (+ offset off)))
                (number-sequence 0 (1- nbytes))))

     while (< offset length)

     for reloc = (map-elt relocs-by-offset offset)
     if reloc
     append
     (pcase-let ((`(_ ,type ,symbol ,addend) reloc)
                 (func)
                 (directives))

       (setq symbol (propertize symbol 'help-echo (objdump-demangle symbol)))

       (setq func (cdr (cl-find-if (lambda (re) (string-match-p re type))
                                   binfile-data-relocation-handler-alist
                                   :key #'car)))

       (if func
           (pcase-let ((`(,nbytes . ,d)
                        (funcall func type symbol addend)))
             (setq directives d)
             (cl-incf offset (max nbytes 1)))
         (setq directives
               (list
                (cons (format "# Relocation to %s%s of type %s"
                              symbol
                              (if (and addend (not (zerop addend)))
                                  (format "%s0x%x"
                                          (if (cl-plusp addend) "+" "-")
                                          (abs addend))
                                "")
                              type)
                      "")
                (cons ".byte" (number-to-string (seq-elt bytes offset)))))
         (cl-incf offset 1))

       directives)

     else if (zerop (seq-elt bytes offset))
     collect (let ((start offset))
               (while (and (< offset length)
                           (zerop (seq-elt bytes offset))
                           (null (map-elt relocs-by-offset offset)))
                 (cl-incf offset))
               (cons ".zero" (number-to-string (- offset start))))

     else if (and (< offset (- length 8))
                  (not (funcall region-contains-relocs-p 8)))
     collect (cons ".byte"
                   (format "0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x, 0x%x"
                           (seq-elt bytes offset)
                           (seq-elt bytes (+ offset 1))
                           (seq-elt bytes (+ offset 2))
                           (seq-elt bytes (+ offset 3))
                           (seq-elt bytes (+ offset 4))
                           (seq-elt bytes (+ offset 5))
                           (seq-elt bytes (+ offset 6))
                           (seq-elt bytes (+ offset 7))))
     and do (cl-incf offset 8)

     else if (and (< offset (- length 4))
                  (not (funcall region-contains-relocs-p 4)))
     collect (cons ".byte" (format "0x%x, 0x%x, 0x%x, 0x%x"
                                   (seq-elt bytes offset)
                                   (seq-elt bytes (+ offset 1))
                                   (seq-elt bytes (+ offset 2))
                                   (seq-elt bytes (+ offset 3))))
     and do (cl-incf offset 4)

     else
     collect (cons ".byte" (format "0x%x" (seq-elt bytes offset)))
     and do (cl-incf offset))))

(defun binfile-insert-data
    (section address &optional stop-address offset symbol)
  "Dump contents of SECTION starting at ADDRESS in the ASM buffer.
If STOP-ADDRESS is given, the dump stops at this address.

If OFFSET is given, it is added to ADDRESS.  This is used for
display purposes.

SYMBOL can be an arbitrary name, that says what is being dumped.
This is only for display purposes."
  (interactive
   (pcase-let*
       ((full current-prefix-arg)
        (at-pt (binfile--symbol-and-offset-at-point))
        (`(,symbol ,filename)
         (binfile--get-symbol-and-filename
          "Dump symbol: " nil
          (lambda (symbol filename)
            (let ((data (car (gethash symbol (objdump-read-symtab filename)))))
              (cl-destructuring-bind
                  (&key section demangled flags &allow-other-keys) data
                (if (string= demangled section)
                    (format "Section %s" section)
                  (format "%s in section %s%s" demangled section
                          (if-let* ((interesting-flags
                                     (delete nil
                                             (list (when (memq 'function flags)
                                                     "function")
                                                   (when (memq 'object flags)
                                                     "object")))))
                              (format " [%s]"
                                      (string-join interesting-flags
                                                   ", "))
                            ""))))))
          nil nil #'binfile-insert-data))
        (offset (if full
                    (let ((s (read-string "Offset: "
                                          (or (and (cdr at-pt)
                                                   (format "0x%x" (cdr at-pt)))
                                              "0x0")
                                          nil "0")))
                      (if (string-match "^\\([+-]\\)?0x\\([[:xdigit:]]+\\)" s)
                          (* (string-to-number (match-string 2 s) 16)
                             (if (equal "-" (match-string 1 s)) -1 1))
                        (string-to-number s)))
                  0))
        (ent (gethash symbol (objdump-read-symtab filename)))
        (data (car ent))
        (size (plist-get data :size))
        (size
         (if full
             (let ((s (read-string "Size: "
                                   (and size (number-to-string size)))))
               (if (string-match "0x\\([[:xdigit:]]+\\)" s)
                   (string-to-number (match-string 1 s) 16)
                 (string-to-number s)))
           size)))
     (list
      (plist-get data :section)
      (plist-get data :address)
      (unless (or (null size) (zerop size))
        (+ (plist-get data :address) size))
      (if (zerop offset) nil offset)
      symbol)))
  (let ((data (and (not (string= ".bss" section))
                   (binfile--get-raw-data-as-directives
                    binfile--file section (+ address (or offset 0))
                    (and stop-address
                         (+ stop-address (or offset 0)))))))
    (with-current-buffer binfile-disassembly-buffer
      (goto-char (point-max))
      (let ((buffer-read-only nil)
            startpt)
        (unless (looking-back "\n\n" (- (point) 2))
          (insert "\n\n"))

        (setq startpt (point))

        (insert ".section " section "\n")

        (when (or offset (> address 0))
          (insert ".org " section " + " (format "0x%x" address))
          (when offset
            (let ((sign (if (cl-plusp offset) "+" "-")))
              (insert " " sign " " (format "0x%x" (abs offset)))))
          (insert "\n"))

        (when (and symbol (not (string= symbol section)))
          (unless offset
            (when stop-address
              (insert ".size "
                      symbol ", "
                      (number-to-string (- stop-address address))
                      "\n"))
            (insert (propertize symbol 'help-echo
                                (objdump-demangle symbol))
                    ":\n")))

        (if data
            (cl-loop for (directive . value) in data
                     do (insert "\t" directive " " value "\n"))
          (insert "\t" ".zero" " "
                  (number-to-string
                   (-
                    (or (and stop-address (+ stop-address (or offset 0))) 0)
                    (or (and address (+ address (or offset 0))) 0)))
                  "\n"))

        (goto-char startpt)
        (let ((pulse-iterations 10)
              (pulse-delay 0.15)
              (recenter-positions '(middle)))
          (unless (binfile--buffer-visible-p (current-buffer))
            (pop-to-buffer (current-buffer)))
          (pulse-momentary-highlight-region startpt (point-max))
          (when (eq (current-buffer) (window-buffer))
            ;; This condition is required for tests in -batch mode.
            (recenter-top-bottom)))))))


;; Diffing

(defvar-local binfile--diff-filename nil "Filename for this diff buffer.")

;;;###autoload
(defun binfile-diff (symbol filename-a filename-b)
  "Compare SYMBOL between FILENAME-A and FILENAME-B."
  (interactive
   (let* ((filename-a
           (read-file-name "Binary file A to compare: "
                           nil nil nil
                           (or (ignore-errors
                                 (with-current-buffer
                                     binfile-disassembly-diff-buffer-a
                                   binfile--diff-filename))
                               binfile--file)))
          (filename-b
           (read-file-name "Binary file B to compare: "
                           (file-name-directory filename-a) nil nil
                           (ignore-errors
                             (with-current-buffer
                                 binfile-disassembly-diff-buffer-b
                               binfile--diff-filename)))))
     (list
      (car
       (binfile--get-symbol-and-filename
        "Symbol to compare: " nil nil
        (and binfile--symbol
             (funcall binfile-symbol-transform-function
                      binfile--symbol))
        filename-a #'binfile-diff))
      filename-a filename-b)))
  (binfile-disassemble symbol filename-a)

  (ignore-errors (kill-buffer binfile-disassembly-diff-buffer-a))
  (ignore-errors (kill-buffer binfile-disassembly-diff-buffer-b))

  (with-current-buffer binfile-disassembly-buffer
    (let ((buffer-a (clone-buffer))
          (buffer-b (current-buffer)))
      (binfile-disassemble symbol filename-b)

      (with-current-buffer buffer-a
        (rename-buffer binfile-disassembly-diff-buffer-a)
        (setq-local binfile--diff-filename filename-a)
        (let ((buffer-read-only nil))
          (save-excursion
            (goto-char (point-min))
            (insert "# File " filename-a "\n"))))
      (with-current-buffer buffer-b
        (rename-buffer binfile-disassembly-diff-buffer-b)
        (setq-local binfile--diff-filename filename-b)
        (let ((buffer-read-only nil))
          (save-excursion
            (goto-char (point-min))
            (insert "# File " filename-b "\n"))))

      (ediff-buffers buffer-a buffer-b))))


;; Symbol info

(defconst binfile-symbol-info-buffer "*binfile symbol info*"
  "Buffer for symbol info.")

;;;###autoload
(defun binfile-symbol-info (symbol filename)
  "Display a buffer showing information about SYMBOL in binary file FILENAME."
  (interactive
   (binfile--get-symbol-and-filename "Info for symbol: "
                                     nil nil nil nil
                                     #'binfile-symbol-info))
  (let* ((demangled (objdump-demangle symbol))
         (symtab (objdump-read-symtab filename))
         (data (gethash symbol symtab))
         (insert-fn (lambda (label string)
                      (insert (propertize (concat label ":") 'face 'bold)
                              " " string "\n"))))
    (unless data
      (error "Symbol %S not found in file %s" symbol filename))
    (let ((buffer (get-buffer-create binfile-symbol-info-buffer)))

      (with-current-buffer buffer
        (let ((buffer-read-only nil))
          (erase-buffer)

          (dolist (elt data)
            (cl-destructuring-bind
                (&key address section size flags &allow-other-keys) elt
              (funcall insert-fn "File" filename)
              (funcall insert-fn "Symbol" symbol)
              (funcall insert-fn "Demangled name" demangled)
              (funcall insert-fn "Section" section)
              (funcall insert-fn "Start address"  (format "0x%x" address))
              (funcall insert-fn "Stop address"
                       (format "0x%x, size %s" (+ address size) size))
              (funcall insert-fn "Flags" (format "%s" flags))

              (insert "\n\n")))
          (special-mode)
          (binfile-mode +1))
        (setq buffer-read-only t)
        (setq truncate-lines t))

      (display-buffer buffer))))


;; Minor mode

(defvar binfile-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "g") #'binfile-disassemble)
    map)
  "Map for `binfile-mode'.")

(define-minor-mode binfile-mode
  "Minor mode for ASM buffers."
  :keymap binfile-mode-map
  :lighter " binfile")

(provide 'binfile)
;;; binfile.el ends here
