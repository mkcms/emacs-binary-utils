;;; objdump.el --- Disassemble binary files     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: c, tools
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
;; This library provides functions for working with the objdump program.  It
;; can read the symbol table, disassemble code, extract contents of sections,
;; mangle/demangle names.
;;
;; The API functions are:
;;
;; - `objdump-file-dynamic-p'
;;   Check if file is dynamic.
;;
;; - `objdump-read-symtab'
;;   Read the symbol table from a file.
;;
;; - `objdump-mangle', `objdump-demangle', `objdump-symbol-mangled-p',
;;   `objdump-symbol-demangled-p'
;;   Mangle and demangle symbols; check if they are mangled.
;;
;; - `objdump-disassemble'
;;   Disassemble a portion of a code section.
;;
;; - `objdump-raw'
;;   Get the raw contents of a section.
;;

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'rx)
(require 'subr-x)

(defgroup objdump nil "Disassemble binary files."
  :group 'tools
  :group 'c)

(defcustom objdump-program (or (executable-find "objdump") "objdump")
  "Objdump executable.
This can either be a file path to the objdump executable, or an
alist of (TESTER . PATH) entries, where TESTER is either a regexp
for file format or a function accepting a filename, and PATH is a
path to objdump executable.  The output is checked against parsed
output of running \\='PATH -f\\=' on a binary file."
  :type '(choice file
                 (alist :key-type (choice string function) :value-type file)))

(defvar objdump-file-format nil
  "File format of the last file processed.
This is a string like \"elf64-x86-64\" and is printed by
objdump on every run.")

(defun objdump--choose-executable (binary)
  "Choose objdump program for examining BINARY."
  (let (objdump)
    (cond
     ((stringp objdump-program) (setq objdump objdump-program))
     ((listp objdump-program)
      (setq objdump
            (cl-loop
             for (tester . program) in objdump-program

             if (and (functionp tester) (funcall tester binary))
             return program

             else if (when-let* ((output
                                  (and (stringp tester)
                                       (shell-command-to-string
                                        (format "%s -f %s" program binary)))))
                       (and (string-match
                             ".*[^:\n]+:[[:space:]]+file format \\([^\n]*\\).*"
                             output)
                            (string-match-p tester (match-string 1 output))))
             return program)))
     (t (error "`objdump-program' has unrecognized value")))
    (unless objdump
      (error "No objdump program found for %s" binary))
    (or (executable-find objdump) objdump)))

(defun objdump--run-command (args)
  "Run objdump with ARGS.
The output of the process is appended to the end of current
buffer.  The variable `objdump-file-format' is updated."
  (let* ((pr nil)
         (pt (point))
         (program (objdump--choose-executable
                   (cl-find-if (lambda (a)
                                 (and (not (string-prefix-p "-" a))
                                      (file-exists-p a)))
                               args)))
         (process
          (make-process
           :name "objdump"
           :buffer (current-buffer)
           :command `(,program ,@args)
           :connection-type 'pipe
           :noquery t)))
    (unwind-protect
        (progn
          (setq pr (make-progress-reporter
                    (format "Running %s %s" program
                            (combine-and-quote-strings args))))
          (accept-process-output nil 0.05)
          (while (process-live-p process)
            (progress-reporter-force-update pr)
            (accept-process-output nil 0.1))
          (unless (= 0 (process-exit-status process))
            (let (msg)
              ;; Find error message
              (cl-loop until msg
                       for re in (list (format "^%s: \\(.*\\)$"
                                               (regexp-quote program))
                                       "objdump: \\(.*\\)$")
                       do (goto-char (point-min))
                       if (re-search-forward re nil t)
                       do (setq msg (match-string 1)))
              (error "Objdump failed with code %s%s"
                     (process-exit-status process)
                     (or (and msg (concat ": " msg)) ""))))
          (goto-char pt)
          (setq objdump-file-format nil)
          (save-excursion
            (cl-loop
             until (eobp)
             if (looking-at "^[^:\n]+:[[:space:]]+file format \\(.*\\)$")
             do (progn (setq objdump-file-format (match-string 1))
                       (replace-match ""))
             else if (looking-at-p "^$")
             do (delete-region (line-beginning-position)
                               (min (1+ (line-end-position)) (point-max)))
             else do (cl-return))))
      (progress-reporter-done pr)
      (delete-process process)
      (goto-char pt)
      (flush-lines "^Process .* finished$"))))

(defun objdump--file-mtime (filename)
  "Return modification time of file FILENAME."
  (file-attribute-modification-time (file-attributes filename)))

(defvar objdump--flags-cache (make-hash-table :test #'equal)
  "Cache for objdump's file headers output.
Keys are filenames.  Values are conses (MTIME . FLAGS), where
MTIME is the file modification time, and FLAGS is the output of
`objdump--read-file-flags' for that file.")

(defun objdump--read-file-flags (filename)
  "Read the flags from header of FILENAME."
  (let ((mtime (objdump--file-mtime filename))
        cached)
    (cond
     ((and (setq cached (gethash filename objdump--flags-cache))
           (equal mtime (car cached)))
      (cdr cached))
     (t
      (cdr
       (puthash filename
                (cons mtime
                      (with-temp-buffer
                        ;; Parse output which looks like:
                        ;;
                        ;;   foo.o:     file format elf64-x86-64
                        ;;   architecture: i386:x86-64, flags 0x00000011:
                        ;;   HAS_RELOC, HAS_SYMS
                        ;;   start address 0x0000000000000000
                        (objdump--run-command `("-f" ,filename))
                        (re-search-forward "flags 0x[[:xdigit:]]+:")
                        (forward-line 1)
                        (split-string
                         (buffer-substring (line-beginning-position)
                                           (line-end-position))
                         ", *")))
                objdump--flags-cache))))))

(defun objdump-file-dynamic-p (filename)
  "Return non-nil if FILENAME is the name of a dynamic file."
  (and (member "DYNAMIC" (objdump--read-file-flags filename)) t))

(defvar objdump--symtab-cache (make-hash-table :test #'equal)
  "Cache for objdump's symtab output.
Keys are filenames.  Values are conses (MTIME . HTAB), where
MTIME is the file modification time, and HTAB is the output of
`objdump-read-symtab' for that file.")

(defvar objdump--mangled-to-demangled-names (make-hash-table :test #'equal)
  "Hash table mapping mangled to demangled names.")

(defvar objdump--demangled-to-mangled-names (make-hash-table :test #'equal)
  "Hash table mapping demangled to mangled names.")

(defvar objdump--symtab-entry-regexp
  (rx
   bol
   (group (one-or-more hex-digit)) (1+ space) ;; Address
   (group (repeat 7 any)) (1+ space)          ;; Flag characters
   (group (1+ (not space))) (1+ space)        ;; Section
   (group (one-or-more hex-digit)) (1+ space) ;; Size/alignment
   (group (not (in space ?\n)) (1+ any))      ;; Symbol name
   eol)
  "Regular expression for parsing objdump's symbol table.
The groups are:
1 - address
2 - flags
3 - section
4 - size/alignment
5 - symbol name")

(defvar objdump--symtab-start-regexp
  (rx bol "SYMBOL TABLE:" eol)
  "Regular expression matching the start of the symbol table.")

(defvar objdump--dynamic-symtab-entry-regexp
  (rx
   bol
   (group (one-or-more hex-digit)) (1+ space) ;; Address
   (group (repeat 7 any)) (1+ space) ;; Flag characters
   (group (1+ (not space))) (1+ space) ;; Section
   (group (one-or-more hex-digit)) (1+ space) ;; Size/alignment
   (or
    ;; The version information is optional
    (and
     (and (1+ space) (1+ (not space)) (1+ space)) ;; Version information
     (group-n 5 (not space) (1+ any)))     ;; Symbol name
    (group-n 5 (1+ (not (in space ?\n))))) ;; Symbol name
   eol)
  "Regular expression for parsing objdump's dynamic symbol table.")

(defvar objdump--dynamic-symtab-start-regexp
  (rx bol "DYNAMIC SYMBOL TABLE:" eol)
  "Regular expression matching the start of the dynamic symbol table.")

(defcustom objdump-c++filt-program (or (executable-find "c++filt") "c++filt")
  "Program for name demangling."
  :type 'file)

(defun objdump--parse-symbol-flags (flags)
  "Parse symbol FLAGS.
See documentation of `objdump-read-symtab' for a description of
the return value."
  (let ((ret (list)))
    (pcase (aref flags 0)
      ('?l (push 'local ret))
      ('?g (push 'global ret))
      ('?u (push 'unique-global ret))
      ('?! (push 'global ret) (push 'local ret)))
    (pcase (aref flags 1)
      ('?w (push 'weak ret))
      ('?\  (push 'strong ret)))
    (pcase (aref flags 2)
      ('?C (push 'constructor ret))
      ;; ('?\  (push 'normal ret))
      )
    (pcase (aref flags 3)
      ('?W (push 'warning ret))
      ;; ('?\  (push 'normal ret))
      )
    (pcase (aref flags 4)
      ('?I (push 'indirect-reference ret))
      ('?i (push 'ifunc ret))
      ;; ('?\  (push 'normal ret))
      )
    (pcase (aref flags 5)
      ('?d (push 'debug ret))
      ('?D (push 'dynamic ret))
      ;; ('?\  (push 'normal ret))
      )
    (pcase (aref flags 6)
      ('?F (push 'function ret))
      ('?f (push 'file ret))
      ('?O (push 'object ret))
      ;; ('?\  (push 'normal ret))
      )
    (nreverse ret)))

(defun objdump-read-symtab (filename)
  "Read symbol table from FILENAME.
The return value is a hash table mapping mangled symbol names to
list of plists.  Each plist is of the form

  (:address NUM :size NUM :section STR :demangled STR :flags FLAGS)

where

  :address    NUM    is the symbol's address
  :size       NUM    is it's size
  :section    STR    is the name of it's section
  :demangled  STR    is the demangled name
  :flags      FLAGS  is a list of the symbol's flags.
                     It is a list of zero or more Lisp symbols:
    \\='local
    \\='global
    \\='unique-global
    \\='weak
    \\='strong
    \\='constructor
    \\='warning
    \\='indirect-reference
    \\='ifunc
    \\='debug
    \\='dynamic
    \\='function
    \\='file
    \\='object"
  (let ((mtime (objdump--file-mtime filename))
        cached)
    (cond
     ((and (setq cached (gethash filename objdump--symtab-cache))
           (equal mtime (car cached)))
      (cdr cached))
     (t
      (let* (demangler
             (symbols (make-hash-table :test #'equal))
             mangled-symbols
             (nsymbols 0)
             (args `("--syms" ,filename))
             (args (if (objdump-file-dynamic-p filename)
                       `("--dynamic-syms" ,@args)
                     args))
             syms-start syms-end
             dynsyms-start dynsyms-end)
        (unwind-protect
            (with-temp-buffer
              (setq demangler
                    (make-process
                     :name "objdump-cxxfilt"
                     :noquery t
                     :command (list objdump-c++filt-program)
                     :connection-type 'pipe
                     :buffer (generate-new-buffer " *objdump-c++filt*")))

              (objdump--run-command args)

              ;; Check where normal symbols start
              (save-excursion
                (if (re-search-forward objdump--symtab-start-regexp nil t)
                    (setq syms-start (1+ (line-end-position))
                          syms-end (or (re-search-forward "^$" nil t)
                                       (point-max)))
                  (setq syms-start (point-max) syms-end (point-max))))
              (when (< (- syms-end syms-start) 32)
                (setq syms-start (point-max) syms-end (point-max)))

              ;; ... and range of dynamic symbols
              (save-excursion
                (if (re-search-forward
                     objdump--dynamic-symtab-start-regexp nil t)
                    (setq dynsyms-start (1+ (line-end-position))
                          dynsyms-end (or (re-search-forward "^$" nil t)
                                          (point-max)))
                  (setq dynsyms-start (point-max) dynsyms-end (point-max))))

              (let ((pr (make-progress-reporter "Reading symbol table"
                                                0 (point-max))))
                (unwind-protect
                    (pcase-dolist
                        (`(,start ,end ,re)
                         `((,syms-start
                            ,syms-end
                            ,objdump--symtab-entry-regexp)
                           (,dynsyms-start
                            ,dynsyms-end
                            ,objdump--dynamic-symtab-entry-regexp)))
                      (goto-char start)
                      (while (< (point) end)
                        (progress-reporter-update pr (point))
                        (goto-char (line-beginning-position))
                        (when (re-search-forward
                               re (min (line-end-position) end) t)
                          (let ((address
                                 (string-to-number (match-string 1) 16))
                                (flags (objdump--parse-symbol-flags
                                        (match-string 2)))
                                (section (match-string 3))
                                (size (string-to-number (match-string 4) 16))
                                (name (match-string 5)))
                            (push `(:address ,address
                                             :flags ,flags
                                             :size ,size
                                             :section ,section)
                                  (gethash name symbols))
                            (push name mangled-symbols)
                            (cl-incf nsymbols)
                            (process-send-string demangler
                                                 (format "%s\n" name))))
                        (forward-line 1)))
                  (progress-reporter-done pr)))

              (process-send-eof demangler)
              (accept-process-output nil 0.05)
              (while (process-live-p demangler)
                (accept-process-output nil 0.1))

              (with-current-buffer (process-buffer demangler)
                (let ((pr (make-progress-reporter
                           "Demangling names" 0 (point-max))))

                  (goto-char (point-min))
                  (forward-line nsymbols)

                  ;; Really read demangled names
                  (while mangled-symbols
                    (when (bobp)
                      (error "c++filt returned too few results"))

                    (forward-line -1)
                    (let ((mangled-name (pop mangled-symbols))
                          (demangled-name
                           (buffer-substring
                            (line-beginning-position) (line-end-position))))
                      (puthash mangled-name demangled-name
                               objdump--mangled-to-demangled-names)
                      (puthash demangled-name
                               mangled-name
                               objdump--demangled-to-mangled-names)

                      (dolist (plist (gethash mangled-name symbols))
                        (plist-put plist :demangled demangled-name))

                      (progress-reporter-update pr (- (point-max) (point)))))
                  (progress-reporter-done pr)
                  (unless (bobp)
                    (error "c++filt returned too many results"))))

              (puthash filename (cons mtime symbols) objdump--symtab-cache)
              symbols)
          (kill-buffer (process-buffer demangler))
          (delete-process demangler)))))))

(defun objdump-mangle (symbol)
  "Mangle a demangled SYMBOL, or return SYMBOL."
  (propertize
   (gethash symbol objdump--demangled-to-mangled-names symbol)
   'objdump-mangled t))

(defun objdump-demangle (symbol)
  "Demangle a mangled SYMBOL, or return SYMBOL."
  (save-match-data
    (propertize
     (or
      (gethash symbol objdump--mangled-to-demangled-names)
      (and (string-match "^[^@]+\\(@.*$\\)" symbol)
           (gethash
            (replace-match "" nil nil symbol 1)
            objdump--mangled-to-demangled-names))
      symbol)
     'objdump-mangled nil)))

(defun objdump-symbol-mangled-p (symbol)
  "Return non-nil if SYMBOL is mangled."
  (if (memq 'objdump-mangled (text-properties-at 0 symbol))
      (get-text-property 0 'objdump-mangled symbol)
    (string= symbol (objdump-mangle symbol))))

(defun objdump-symbol-demangled-p (symbol)
  "Return non-nil if SYMBOL is demangled."
  (not (objdump-symbol-mangled-p symbol)))

(defvar objdump-disassembly-extra-args nil
  "List of strings to append to objdump's disassembly command.")

(defvar objdump-disassembly-extra-args-alist nil
  "Alist mapping objdump executable to a list of extra arguments.
The extra arguments are passed to objdump's disassembly command.")

(cl-defun objdump-disassemble
    (filename &optional section start-address stop-address
              &key demangle
              reloc
              (hide-raw-insn t))
  "Disassemble SECTION in FILENAME between START-ADDRESS and STOP-ADDRESS.
The disassembled output is inserted in the current buffer after
point.

If DEMANGLE is non-nil, the -C (--demangle) flag is passed to objdump.

If RELOC is non-nil, relocations are interspersed with the
disassembly; this adds the --reloc flag.  If the file is a
dynamic object the --dynamic-reloc flag is also added.

If HIDE-RAW-INSN is nil, raw instructions are shown, otherwise
the --no-show-raw-insn option is added.

Arguments passed to objdump will also include the contents of
`objdump-disassembly-extra-args' and `objdump-disassembly-extra-args-alist'."
  (objdump--run-command
   (remq nil
         (cl-list*
          "-d" filename
          (and section (format "--section=%s" section))
          (and demangle "-C")
          (and reloc "--reloc")
          (and reloc (objdump-file-dynamic-p filename) "--dynamic-reloc")
          (and hide-raw-insn "--no-show-raw-insn")
          (and start-address (format "--start-address=0x%x" start-address))
          (and stop-address (format "--stop-address=0x%x" stop-address))
          (append objdump-disassembly-extra-args
                  (cdr (assoc (objdump--choose-executable filename)
                              objdump-disassembly-extra-args-alist)))))))

(defvar objdump--dump-line-regexp
  ;;  00f0 12ff3200 00000000 00000000 00000000  ..2.............
  (rx
   bol
   ;; Address
   (* space) (group (one-or-more hex-digit)) (1+ space)

   ;; Raw bytes
   (group (** 1 16 hex-digit hex-digit (? space))) (1+ space)

   ;; Hexdump
   (repeat 16 any)
   eol
   )
  "Regular expression matching objdump's raw dump line.")

(defvar objdump--relocation-record-regexp
  ;; 0000000000000004 R_X86_64_PC32     .bss+0x000000000000000c
  (rx
   bol
   ;; Address
   (* space) (group (one-or-more hex-digit)) (1+ space)

   ;; Relocation type
   (group (1+ (not space))) (1+ space)

   ;; Relocated symbol
   (group (minimal-match (1+ any)))

   ;; Optional offset
   (group (optional (in ?- ?+) "0x" (1+ hex-digit)))

   eol
   )
  "Regular expression matching objdump's relocation record line.")

(defun objdump-raw (filename section &optional start-address stop-address)
  "Get contents of SECTION in FILENAME between START-ADDRESS and STOP-ADDRESS.

If STOP-ADDRESS is nil, return data range from START-ADDRESS to
the end.

The return value is a list (BYTES-VEC RELOCS REAL-START-ADDRESS),
where
  BYTES-VEC is a vector of bytes,
  RELOCS is a list of relocations found in the dumped region.
         Each element of RELOCS is a list (ADDRESS TYPE SYMBOL OFFSET).
  REAL-START-ADDRESS is usually START-ADDRESS, or the actual start address
         which was dumped by objdump if START-ADDRESS was not provided
         as argument."
  (with-temp-buffer
    (let* ((args
            (remq nil
                  `("-s" "-j" ,section "--reloc" ,filename
                    ,(and start-address
                          (format "--start-address=0x%x" start-address))
                    ,(and stop-address
                          (or (null start-address)
                              (/= stop-address start-address))
                          (format "--stop-address=0x%x" stop-address)))))
           (args (if (objdump-file-dynamic-p filename)
                     `("--dynamic-reloc" ,@args)
                   args))
           pr
           relocs
           dumped-address
           real-start-address)
      (objdump--run-command args)
      (goto-char (point-min))

      ;; Check if there are relocation records in that data range
      (dolist (regexp '("^RELOCATION RECORDS" "^DYNAMIC RELOCATION RECORDS"))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward regexp nil t)
            (forward-line 2)            ;skip header line
            (goto-char (line-beginning-position))
            (cl-loop
             while (looking-at objdump--relocation-record-regexp)
             do (forward-line 1)
             for addr = (string-to-number (match-string 1) 16)
             for type = (match-string 2)
             for symbol = (match-string 3)
             for offset-str = (match-string 4)
             for offset = (and offset-str
                               (string-to-number
                                (replace-regexp-in-string "0x" "" offset-str)
                                16))
             do (push (list addr type symbol offset) relocs)))))

      (unless (re-search-forward "^Contents of section.*" nil t)
        (error (concat "objdump did not produce raw bytes for"
                       " section %s in file %s (range %s - %s)")
               section filename
               (or start-address 0)
               (or stop-address "INF")))
      (when (re-search-forward "^Contents of section.*" nil t)
        (error "Objdump dumped multiple sections"))
      (setq pr (make-progress-reporter "Parsing output"
                                       (point-min) (point-max)))
      (unwind-protect
          (list
           (cl-loop
            while (re-search-forward objdump--dump-line-regexp nil t)
            do (progress-reporter-update pr (point))
            for addr = (string-to-number (match-string 1) 16)

            ;; Ensure that the address range is continuous and begins at
            ;; START-ADDRESS
            do (if (null dumped-address)
                   (progn
                     (setq real-start-address addr)
                     (when (and start-address (/= addr start-address))
                       (error
                        "Objdump address range started at 0x%x (wanted 0x%x)"
                        addr start-address)))
                 (when (/= addr (+ dumped-address 16))
                   (error "Objdump missed address 0x%x"
                          (+ dumped-address 16))))
            (setq dumped-address addr)

            for byte-string = (replace-regexp-in-string " " ""
                                                        (match-string 2))

            vconcat (cl-loop
                     with size = (/ (length byte-string) 2)
                     for i below size
                     with vec = (make-vector size 0)
                     for chars = (substring byte-string (* i 2) (* (1+ i) 2))
                     do (setf (aref vec i) (string-to-number chars 16))
                     finally return vec))
           (sort relocs #'car-less-than-car)
           (or real-start-address start-address))
        (progress-reporter-done pr)))))

(provide 'objdump)
;;; objdump.el ends here
