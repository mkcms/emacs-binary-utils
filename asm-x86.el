;;; asm-x86.el --- X86 utilities     -*- lexical-binding: t; -*-

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

;;; Code:

(require 'cl-lib)
(require 'map)

(defun asm-x86--format-reloc-and-addend (reloc addend)
  "Return a string for relocation RELOC with ADDEND.
This returns a string like \"reloc+0x1a\".  ADDEND can be nil."
  (format "%s%s"
          reloc
          (if (and addend (not (zerop addend)))
              (if (cl-plusp addend)
                  (format "+0x%x" addend)
                (format "-0x%x" (abs addend)))
            "")))

(defun asm-x86--reloc-replace-0x0 (_type reloc _address addend _function-name)
  "Simply replace the string \"0x0\" with the relocation name, RELOC+ADDEND."
  (when (re-search-forward "\\b0x0\\b" nil t)
    ;; Check if we have another '0x0' and don't replace in that case.
    (unless (save-match-data (re-search-forward "\\b0x0\\b" nil t))
      (replace-match (asm-x86--format-reloc-and-addend reloc addend) nil t)
      t)))

(defun asm-x86--reloc-replace-address-minus-addend
    (_type reloc address addend _function-name)
  "Subtract ADDEND from the ADDRESS and replace that with RELOC."
  (setq address (- address (or addend 0)))
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward (format "\\b%x\\b" address) nil t)
      (goto-char (match-beginning 0))
      (unless (save-match-data
                (looking-back "^[[:space:]]+" (line-beginning-position)))
        ;; Handle the case when we matched address of the instruction itself -
        ;; we don't want that.  We want to match an address _inside_ the
        ;; instruction.
        (replace-match reloc nil t)
        t))))

(defun asm-x86--reloc-replace-0x0-plus-addend-plus-4
    (_type reloc _address addend _function-name)
  "Replace the string \"0x0\" with the RELOC+ADDEND, plus 4."
  (when (re-search-forward "\\b0x0\\b" nil t)
    ;; Only replace if we have exactly one '0x0'
    (unless (save-match-data (re-search-forward "\\b0x0\\b" nil t))
      (replace-match
       (asm-x86--format-reloc-and-addend reloc (+ addend 4)) nil t)
      t)))

(defun asm-x86--reloc-64
    (reloc-type reloc reloc-address reloc-addend function-name)
  "Handle x64 RELOC-TYPE RELOC at RELOC-ADDRESS RELOC-ADDEND in FUNCTION-NAME."
  (pcase reloc-type
    ('"R_X86_64_PC32"
     (or (asm-x86--reloc-replace-address-minus-addend
          reloc-type reloc reloc-address reloc-addend function-name)
         (asm-x86--reloc-replace-0x0-plus-addend-plus-4
          reloc-type reloc reloc-address reloc-addend function-name)))
    ('"R_X86_64_GOTPCREL"
     (asm-x86--reloc-replace-address-minus-addend
      reloc-type reloc reloc-address reloc-addend function-name))
    ('"R_X86_64_REX_GOTPCRELX"
     (asm-x86--reloc-replace-address-minus-addend
      reloc-type reloc reloc-address reloc-addend function-name))
    ('"R_X86_64_PLT32"
     (asm-x86--reloc-replace-address-minus-addend
      reloc-type reloc reloc-address reloc-addend function-name))
    ('"R_X86_64_32"
     (asm-x86--reloc-replace-0x0
      reloc-type reloc reloc-address reloc-addend function-name))))

(defun asm-x86--postprocess (_beg _end _name)
  "Do final cleanups on x86 binaries."
  (save-excursion
    ;; Intel syntax
    (while (re-search-forward
            "\\(\\[rip\\+0x0]\\)\\([^\n]*\\)#\\s-*\\([^\n]*?\\)[[:space:]]*$"
            nil t)
      (replace-match "[rip+\\3]\\2")))
  (save-excursion
    ;; AT&T syntax
    (while (re-search-forward
            (rx
             (group word-boundary "0x0")
             "(%rip)"
             (group (one-or-more (not (in ?#))))
             ?#
             (zero-or-more space)
             (group (one-or-more (not space)))
             (group (zero-or-more any) eol))
             nil t)
      (let ((rest (match-string 4)))
        (replace-match "\\3(%rip)\\2")
        (unless (string-match-p "\\s-*" rest)
          (insert rest))))))

(cl-eval-when (compile)
  (require 'binfile))

(with-eval-after-load 'binfile
  (setf (map-elt binfile-relocation-handler-alist "^R_X86_64_.*")
        #'asm-x86--reloc-64)

  (setf (map-elt binfile-region-postprocessing-functions-alist "elf64-x86-64")
        #'asm-x86--postprocess))

(provide 'asm-x86)
;;; asm-x86.el ends here
