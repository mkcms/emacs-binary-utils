;;; binfile.el --- Prettify disassembly     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Michał Krzywkowski

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
;; This package provides the function `binfile-postprocess-buffer' which
;; prettifies objdump disassembly in the current buffer and makes it easier to
;; read and follow by stripping addresses, adding labels for jump targets,
;; removing useless comments, and some other things.

;; By default it works on objdump output (and is optimized for x86
;; architecture) but you can set `binfile-region-postprocessing-functions',
;; `binfile-region-postprocessing-functions-alist' and
;; `binfile-file-format-function' for your own needs to work with different
;; disassemblers/architectures.

;;; Code:

(require 'cl-lib)
(require 'rx)

;; `require'-ing `map' does not guarantee it is loaded as it is preloaded
;; in Emacs.
;;
;; This hack was stolen from the built-in eglot.el.
(eval-and-compile
  (if (< emacs-major-version 28)
      (progn
        (load "map" nil 'nomessage))
    (require 'map)))

(defgroup binfile nil "Prettify disassembly."
  :group 'languages
  :group 'tools)

(defvar binfile-region-postprocessing-functions
  '(
    binfile-postprocess-relocations
    binfile-postprocess-local-jumps
    binfile-postprocess-strip-addresses
    binfile-postprocess-boring-comments
    binfile-postprocess-numeric-to-symbolic-references
    binfile-postprocess-unused-symbolic-local-references
    binfile-postprocess-add-relocation-buttons
    )
  "List of functions that postprocess ASM regions.
Each function is called with three arguments (BEG END NAME),
where BEG and END are region bounds and NAME is the name of the
symbol disassembled in that region (function name).")

(defvar binfile-buffer-postprocessing-functions nil
  "List of functions to call when postprocessing the buffer.
This is called at the end of `binfile-postprocess-buffer'.")

(defvar binfile-region-postprocessing-functions-alist nil
  "Alist of functions to call per file format.
Keys are regexps which are matched against the file format of binary
file, e.g. \"elf64-x86-64\".  Values are functions which are called with
three arguments (BEG END NAME).  BEG and END are buffer regions to
process, NAME is the function name in that region.")

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

(defun binfile-postprocess-unused-symbolic-local-references
    (_beg _end function-name)
  "Remove useless symbolic references with offsets to FUNCTION-NAME.
E.g. this removes all instances of '<foo()+0x123af>'."
  (while (re-search-forward
          (format "<%s[+-]0x[0-9a-f]+>" (regexp-quote function-name))
          nil t)
    (replace-match "")))

(define-button-type 'binfile-reloc
  'action #'binfile-reloc-button-action
  'keymap (let ((map (make-sparse-keymap)))
            (define-key map [mouse-1] #'push-button)
            (define-key map (kbd "RET") #'push-button)
            map)
  'mouse-face 'highlight
  'help-echo "mouse-1, RET: disassemble relocation")

(defvar binfile-reloc-button-action #'ignore
  "Function to call when clicking on a relocation button.
It is called with one argument, the relocation's name.")

(defun binfile-reloc-button-action (button)
  "Act on relocation BUTTON after clicking on it."
  (funcall binfile-reloc-button-action
           (overlay-get button 'binfile-reloc-name)))

(defun binfile-postprocess-add-relocation-buttons (_beg _end _name)
  "Replace relocations with buttons which will disassemble the relocated sym."
  (let (pos)
    (while (setq pos (next-single-property-change (point) 'binfile-reloc))
      (goto-char pos)
      (unless (get-text-property (point) 'binfile-reloc)
        (forward-char -1))
      (pcase-let (((map :name) (get-text-property (point) 'binfile-reloc)))
        (save-restriction
          (narrow-to-region (line-beginning-position) (line-end-position))
          (goto-char (line-beginning-position))
          (when-let* ((end (and name (re-search-forward (regexp-quote name)))))
            (make-button (match-beginning 0) (match-end 0)
                         'type 'binfile-reloc
                         'binfile-reloc-name name)))
        name)
      (forward-line 1))))


;; Disassembly

(defvar binfile-file-format nil
  "File format of the region being processed, e.g. \"elf64-x86-64\".
This is let-bound to the return value of
`binfile-file-format-function' for each region and is used to
select the proper postprocessing and relocation handling functions.")

(defvar binfile-file-format-function #'binfile-file-format-function-objdump
  "Function used to find the `binfile-file-format' for the region at point.
When this function is called, the buffer is narrowed to the region bounds.")

(defvar binfile-next-region-function #'binfile-next-region-function-objdump
  "Function used to find the next region to postprocess.
It must either return nil to stop processing the buffer, or a
list of 3 elements (BEG END NAME), where BEG and END are the
buffer positions where the region starts/ends, and NAME is the
name of the function/section/object in that region.")

(defun binfile-file-format-function-objdump ()
  "Find file format from objdump output.
This finds file format by searching for this line:

/path/to/file.o:     file format elf64-x86-64"
  (save-match-data
    (save-restriction
      (widen)
      (when (re-search-backward "^.*:.*file format \\(.*\\)" nil t)
        (match-string-no-properties 1)))))

(defvar binfile--objdump-region-start-regexp
  ;; 00000000000fa <foo(int)>:
  (rx
   bol
   ;; Function address
   (group (+ hex-digit)) (* space)

   ;; Function name
   ?< (group (* any)) ?> ?:

   eol)
  "A regexp matching function beginning in objdump disassembly output.
Group 1 is hex address, group 2 is function name.")

(defun binfile-next-region-function-objdump ()
  "Find the next region in buffer containing objdump -D output."
  (when (re-search-forward binfile--objdump-region-start-regexp nil t)
    (let* ((name (match-string 2))
           (beg (move-marker (make-marker) (1+ (match-end 0))))
           (end (move-marker
                 (make-marker)
                 (or (save-match-data
                       (and (or
                             (re-search-forward
                              "^Disassembly of section" nil t)
                             (re-search-forward
                              binfile--objdump-region-start-regexp nil t))
                            (match-beginning 0)))
                     (point-max)))))
      (replace-match "\\2:")
      (list beg end name))))

(defun binfile--postprocess-region (beg end name)
  "Process disassembled region named NAME, spanning BEG END."
  (save-excursion
    (save-restriction
      (widen)
      (narrow-to-region beg end)
      (goto-char beg)

      (let* ((fmt (save-excursion (funcall binfile-file-format-function)))
             (binfile-file-format fmt))

        (dolist (func binfile-region-postprocessing-functions)
          (save-excursion
            (save-restriction
              (funcall func (point-min) (point-max) name))))

        (when-let* ((func
                     (and fmt
                          (cdr (cl-find-if
                                (lambda (re) (string-match-p re fmt))
                                binfile-region-postprocessing-functions-alist
                                :key #'car)))))
          (save-excursion (funcall func beg end name)))

        (delete-trailing-whitespace (point-min) (point-max))))))

(defun binfile-postprocess-buffer ()
  "Postprocess the entire current buffer."
  (goto-char (point-min))
  (let ((pr (make-progress-reporter "Post-processing disassembly"
                                    (point-min) (point-max))))
    (cl-loop
     do (progress-reporter-update pr (point))
     while (not (eobp))
     for region = (funcall binfile-next-region-function)
     while region
     for (beg end name) = region
     do (goto-char beg)
     do (binfile--postprocess-region beg end name)
     do (goto-char end)
     finally do (progress-reporter-done pr)))
  (run-hooks 'binfile-buffer-postprocessing-functions))

(provide 'binfile)
;;; binfile.el ends here
