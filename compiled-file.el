;;; compiled-file.el --- Get compiled file for current buffer     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: languages, tools
;; Package-Requires: ((emacs "26"))

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
;; This helper package defines a variable `compiled-file' which can be set to
;; the path to the object file for current source file.
;;
;; It can also automatically find an object file if the variable
;; `compiled-file-directory' is set to the build directory.
;;
;; Having the path to a binary file of the current source is used by some other
;; packages to automatically switch from source to disassembly and vice-versa.

;;; Code:

(require 'project)

(defvar compiled-file-directory nil
  "Current build directory.
You can set this per buffer/directory to a directory containing
compiled files.  If this is relative, it is treated as relative to
`project-root', or `default-directory' if not in a project.")

(put 'compiled-file-directory 'safe-local-variable 'stringp)

(defun compiled-file-directory ()
  "Get current build directory, or project root."
  (let ((root (ignore-errors (project-root (project-current)))))
    (or (and compiled-file-directory
             (if (file-name-absolute-p compiled-file-directory)
                 compiled-file-directory
               (expand-file-name compiled-file-directory root)))
        root default-directory)))

(defvar compiled-file nil "Compiled file to use.")

(put 'compiled-file 'safe-local-variable 'stringp)

(defvar compiled-file-function #'compiled-file-find
  "Function which returns the compiled file.
It is called with a single argument FILENAME.")

(defun compiled-file ()
  "Find compiled file for current buffer."
  (or (and compiled-file
           (if (file-name-absolute-p compiled-file)
               compiled-file
             (expand-file-name compiled-file (compiled-file-directory))))
      (and (buffer-file-name) (funcall compiled-file-function
                                       (buffer-file-name)))))

(defun compiled-file-find (filename)
  (let* ((name
          (file-name-nondirectory
           (file-name-sans-extension filename)))
         (pattern
          (format "%s\\([.][^.]+\\)?[.]o$" (regexp-quote name)))
         (candidates
          (if (>= emacs-major-version 27)
              (directory-files-recursively
               (compiled-file-directory) pattern nil t)
            (ignore-errors
              (directory-files-recursively
               (compiled-file-directory) pattern)))))
    (when (= 1 (length candidates))
      (car candidates))))

(provide 'compiled-file)
;;; compiled-file.el ends here
