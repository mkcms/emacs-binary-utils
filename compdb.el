;;; compdb.el --- Work with compilation databases    -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: c, tools

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

;; This package provides utilities for working with compilation databases.
;;
;; The main API functions are:
;;
;; `compdb-path'
;;
;;   Locate database for FILENAME by scanning directory tree upwards.
;;
;; `compdb'
;;
;;   Get parsed compilation database for a project.  The return value is either
;; nil if the database does not exist, or a hash table.
;;
;; `compdb-compile'
;;
;;   Compile the file the current buffer is visiting.
;;
;; `compdb-switch'
;;
;;   Switch the current compilation database.  This works by replacing the
;;   current compilation database with a symbolic link to the new database.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;; `require'-ing these does not guarantee they are loaded as they are preloaded
;; in Emacs.
;;
;; This hack was stolen from the built-in eglot.el.
(eval-and-compile
  (if (< emacs-major-version 28)
      (load "seq" nil 'nomessage)
    (require 'seq)))

(defvar compdb--cache (make-hash-table :test #'equal)
  "Cache of compilation databases.
Keys are paths to compilation databases, values are the
json-decoded databases.")

(defun compdb-path (&optional filename)
  "Locate database for FILENAME by scanning directory tree upwards.
The return value is path to the database file, or nil if not found."
  (or filename (setq filename (or (buffer-file-name) default-directory)))
  (when-let ((dir (locate-dominating-file filename "compile_commands.json")))
    (expand-file-name "compile_commands.json" dir)))

(defun compdb (&optional filename)
  "Get the compilation database for FILENAME.
The return value is either nil if the database does not exist, or
a hash table.

The hash table keys are filenames, and the values are database
entries for that file.  The values are in the form of plists."
  (when-let* ((path (compdb-path filename))
              (mtime (file-attribute-modification-time
                      (file-attributes path))))
    (let (cached)
      (cond
       ((and (setq cached (gethash path compdb--cache))
             (equal mtime (car cached)))
        (cdr cached))
       (t
        (cdr
         (puthash path
                  (cons mtime
                        (with-temp-buffer
                          (let ((json-object-type 'plist)
                                (json-array-type 'vector)
                                json-value
                                (htab (make-hash-table :test #'equal))
                                pr)
                            (unwind-protect
                                (progn
                                  (insert-file-contents-literally path)
                                  (setq pr (make-progress-reporter
                                             "Reading compilation database"
                                             nil (point-max)))
                                  (progress-reporter-update pr)
                                  (setq json-value (json-read))
                                  (cl-loop
                                   for entry across json-value
                                   for file = (expand-file-name
                                               (plist-get entry :file)
                                               (plist-get entry :directory))
                                   do (puthash file entry htab)
                                   (progress-reporter-update pr (point))))
                              (progress-reporter-done pr))
                            htab)))
                  compdb--cache)))))))

(defvar compdb-fallback-extensions
  '(".c" ".cc" ".cpp" ".C" ".CC" ".CPP")
  "Other extensions to try if an entry is missing.")

(defun compdb-entry (&optional filename)
  "Get compilation database entry for FILENAME."
  (or filename (setq filename (buffer-file-name))
      (error "Don't have a filename for current buffer"))
  (setq filename (expand-file-name filename))
  (when-let ((db (compdb))
             (file-sans-ext (file-name-sans-extension filename)))
    (or (gethash filename db)
        (cl-loop for ext in compdb-fallback-extensions
                 for ent = (gethash (concat file-sans-ext ext) db)
                 if ent
                 return ent))))

(defun compdb-compile (&optional filename modify-command)
  "Compile FILENAME (or the file the current buffer is visiting).
If MODIFY-COMMAND is non-nil (interactively, with a prefix
argument), prompt the user to edit the command before running it."
  (interactive (list nil current-prefix-arg))
  (or filename (setq filename (buffer-file-name))
      (error "Don't have a filename for current buffer"))
  (setq filename (expand-file-name filename))
  (let* ((ent (compdb-entry))
         (command (plist-get ent :command))
         (args (plist-get ent :arguments)))
    (unless ent
      (error "Compilation database has no entry for %s" filename))
    (when (and (null command) args)
      (setq command (mapconcat #'shell-quote-argument args " ")))
    (unless command
      (error "Entry for %S has no command or arguments" filename))
    (when modify-command
      (setq command (read-string "Modify command: " command)))
    (unless (yes-or-no-p (format "Run %S? " command))
      (user-error "Not compiling"))
    (let ((default-directory (plist-get ent :directory)))
      (compile command))))

(defun compdb-output-filename (&optional filename)
  "Get the output filename (the object, .o file) for FILENAME."
  (when-let ((ent (compdb-entry filename)))
    (cond
     ((plist-get ent :output)
      (expand-file-name (plist-get ent :output)
                        (plist-get ent :directory)))
     ((and (plist-get ent :command)
           (string-match-p " -o [^[:space:]]+" (plist-get ent :command)))
      (let ((args (split-string-and-unquote (plist-get ent :command))))
        (expand-file-name
         (nth (1+ (cl-position "-o" args :test #'string=)) args)
         (plist-get ent :directory))))
     ((and (plist-get ent :arguments)
           (seq-contains-p (plist-get ent :arguments) "-o"))
      (let* ((pos (1+ (seq-position (plist-get ent :arguments) "-o")))
             (out-arg (seq-elt (plist-get ent :arguments) pos)))
        (expand-file-name out-arg (plist-get ent :directory)))))))

(defun compdb-switch (new-path)
  "Switch the current compilation database to NEW-PATH.
This works by replacing the current compilation database with a
symbolic link to NEW-PATH."
  (interactive
   (if-let ((current-path (compdb-path)))
       (list (read-file-name
               "Switch to compilation database: "
               nil nil t nil
               (lambda (name)
                 (or (file-directory-p name)
                     (string= (file-name-nondirectory name)
                              "compile_commands.json")))))
     (user-error "Compile database not found")))
  (setq new-path (expand-file-name new-path))
  (when (file-directory-p new-path)
    (setq new-path (expand-file-name "compile_commands.json" new-path)))
  (when (or (not (file-exists-p new-path))
            (not (string= (file-name-nondirectory new-path)
                          "compile_commands.json")))
    (error "Not a valid compilation database: %s" new-path))
  (let ((old-path (compdb-path)))
    (unless old-path
      (error "Compilation database not found for dir %s" default-directory))
    (make-symbolic-link new-path old-path t)))

(provide 'compdb)
;;; compdb.el ends here
