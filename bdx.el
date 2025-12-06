;;; bdx.el --- Frontend for bdx -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: tools, c
;; Version: 0.1.0
;; Homepage: https://github.com/mkcms/emacs-binary-utils
;; Package-Requires: ((emacs "29.1"))

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
;; [bdx] is an indexer for ELF files.
;;
;; This package provides an Emacs frontend for the command line tool, and
;; allows quickly finding and disassembling symbols even in huge repositories.
;;
;; Searching interactively displays a list of matched symbols as the user
;; types.
;;
;; The commands defined:
;;
;; - `bdx-disassemble'
;;
;;   Read a bdx query from the user with `ivy', and disassemble the selected
;;   symbol.
;;
;; - `bdx-find-definition'
;;
;;   Read a bdx query from the user and go to the selected symbol's definition.
;;
;; - `bdx-show-graph-xdg-open'
;;
;;   Read two queries from user, START and GOAL, and use `bdx' to generate an
;;   image of a graph that connects symbols matching START and GOAL, then
;;   display that image.
;;
;; It also provides these API functions:
;;
;; - `bdx-query'
;;
;;   Read a bdx query from the user, interactively displaying results.
;;
;; - `bdx-generate-graph'
;;
;;   Generate graph from two queries, and write it (in DOT format) to the
;;   current buffer.
;;
;; - `bdx-generate-graph-image'
;;
;;   Generate a graph image from two queries, and return it's path.
;;

;;; Code:

(require 'cl-lib)
(require 'ivy)
(require 'json)

;; `require'-ing these does not guarantee they loaded as they are preloaded in
;; Emacs.
;;
;; This hack was stolen from the built-in eglot.el.
(eval-and-compile
  (if (< emacs-major-version 28)
      (progn
        (load "map" nil 'nomessage)
        (load "project" nil 'nomessage))
    (require 'map)
    (require 'project)))

(defgroup bdx nil "Frontend for bdx tool."
  :group 'tools)

(defcustom bdx-program "bdx"
  "Path to bdx executable."
  :type 'file)

(defun bdx-safe-directory-p (val)
  "Return non-nil if VAL is a string that satisfies `file-directory-p'."
  (and (stringp val) (file-directory-p val)))

(defvar bdx-index-path nil
  "The argument to use as \\='--index-path\\=' option.")

(put 'bdx-index-path 'safe-local-variable #'bdx-safe-directory-p)

(defvar bdx-binary-directory nil
  "Path to use as \\='-d\\=' option, which specifies the binary dir.")

(put 'bdx-binary-directory 'safe-local-variable #'bdx-safe-directory-p)

(defvar bdx--query-buffer nil
  "The buffer used for the current query.")

(defvar bdx-verbosity 0
  "The verbosity level.
Higher values mean more \\='-v\\=' flags added to each invocation of bdx.")

(defconst bdx-stderr-buffer "*bdx-stderr*")

(defun bdx--command (subcommand &rest args)
  "Generate a list of arguments to bdx SUBCOMMAND with ARGS.
The return value is a list where the first element is
`bdx-program', and the remaining elements are the arguments,
SUBCOMMAND followed by ARGS.

--index-path and -d arguments are appended as necessary.
The ARGS list is filtered out to keep only non-nil values."
  (with-current-buffer (or (ignore-errors (get-buffer bdx--query-buffer))
                           (current-buffer))
    (let ((cmd
           `(,bdx-program
             ,subcommand
             ,@(and (cl-plusp bdx-verbosity)
                    (list (concat "-" (make-string bdx-verbosity ?v))))
             ,@(and bdx-binary-directory (list "-d" bdx-binary-directory))
             ,@(and bdx-index-path (list "--index-path" bdx-index-path))
             ,@(delq nil args))))
      (setq cmd (mapcar #'substring-no-properties cmd))
      (with-current-buffer (get-buffer-create bdx-stderr-buffer)
        (let ((inhibit-read-only t))
          (princ (format "Command: %S\n" cmd) (current-buffer))))
      cmd)))

(defun bdx--run-sync-process (args)
  "Run bdx process synchronously, passing ARGS to it.
The output is inserted into the current buffer."
  (let* ((name (format "bdx-%s" (car args)))
         (process
          (make-process
           :name name
           :command (apply #'bdx--command args)
           :buffer (current-buffer)
           :stderr (with-current-buffer
                       (get-buffer-create bdx-stderr-buffer)
                     (goto-char (point-max))
                     (let ((inhibit-read-only t))
                       (insert "\n\n"))
                     (current-buffer))
           :sentinel
           (lambda (proc _status)
             (let ((code (process-exit-status proc)))
               (unless (eq 0 code)
                 (with-current-buffer (process-buffer proc)
                   (insert
                    (format
                     "error: %s process failed with code %s"
                     name code)))))))))
    (while (accept-process-output process))))


;; Searching

(defvar bdx--callback nil)
(defvar bdx--done-callback nil)
(defvar bdx--error-callback nil)

(defun bdx--read-sexp ()
  "Read the current buffer or return nil, leaving point unmodified."
  (let ((p (point)))
    (condition-case nil
        (read (current-buffer))
      (error (goto-char p) nil))))

(defun bdx--process-filter (process output)
  "Process filter for OUTPUT of bdx search PROCESS."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert output))
    (let (sexp sexps)
      (while (setq sexp (bdx--read-sexp))
        (push sexp sexps))
      (save-excursion
        (save-current-buffer
          (funcall bdx--callback (nreverse sexps)))))))

(defun bdx--process-sentinel (process _status)
  "Process sentinel for bdx search PROCESS."
  (unless (process-live-p process)
    (kill-buffer (process-buffer process))
    (pcase (process-exit-status process)
      ('0)
      ((pred integerp)
       (funcall
        bdx--error-callback
        (with-current-buffer (get-buffer-create bdx-stderr-buffer)
          (goto-char (point-max))
          (when (looking-back "^Process.*\n" nil)
            (forward-line -1)
            (back-to-indentation))
          (let ((string (buffer-substring (point-min) (point))))
            (if (string-match "\\([Ee]rror: .*\\)" string)
                (match-string 1 string)
              string))))))
    (funcall bdx--done-callback)))

(cl-defun bdx--search-async (query &key
                                   (callback #'ignore)
                                   (done-callback #'ignore)
                                   (error-callback #'ignore)
                                   (limit nil))
  "Start and return a search process, searching for QUERY.
CALLBACK will be called for a batch of symbols found.  It must
accept a single argument, which will be the batch, a list of
symbols serialized as plists.

DONE-CALLBACK will be called once the process exits.
ERROR-CALLBACK will be called with an error string on errors.
LIMIT says how many results to return.

The return value is a process object for the search."
  (setq bdx--callback callback)
  (setq bdx--done-callback done-callback)
  (setq bdx--error-callback error-callback)
  (with-current-buffer (get-buffer-create bdx-stderr-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (make-process
   :name "bdx-search"
   :buffer (generate-new-buffer "bdx-search")
   :stderr bdx-stderr-buffer
   :command (apply #'bdx--command
                   (flatten-list
                    `("search" "-f" "sexp"
                      ,(and limit (list "-n" (number-to-string limit)))
                      "--" ,query)))
   :filter #'bdx--process-filter
   :sentinel #'bdx--process-sentinel))

(cl-defun bdx--search (query &key
                             (limit nil))
  "Search for QUERY, returning results as a list.
LIMIT says how how many results to return."
  (let (process results)
    (unwind-protect
        (progn
          (setq process (bdx--search-async
                         query
                         :limit limit
                         :callback (lambda (syms)
                                     (setq results (append results syms)))))
          (while (process-live-p process)
            (accept-process-output process 0.1)))
      (when (process-live-p process)
        (interrupt-process process)))
    results))

(defun bdx-data (string)
  "Get the symbol plist from STRING.
The symbol data, like it's path and section, address are stored
as a property of the string."
  (get-text-property 0 'bdx-data string))

(defvar bdx--last-process nil)
(defvar bdx--prev-candidates nil)
(defvar bdx--all-candidates nil)
(defvar bdx--outdated-files nil)
(defvar bdx--sources-needing-recompilation nil
  "List of source files that are newer than their compiled counterparts.")

(defvar bdx--last-error nil)
(defvar bdx--last-warning nil)

(defun bdx--ivy-collection-function (string &rest _args)
  "Collect candidates for query STRING.
This should be used as COLLECTION for `ivy-read'."
  (when bdx--all-candidates
    (setq bdx--prev-candidates bdx--all-candidates))
  (setq bdx--all-candidates nil)
  (setq bdx--outdated-files nil)
  (setq bdx--sources-needing-recompilation nil)
  (setq bdx--last-error nil)
  (setq bdx--last-warning nil)
  (setq bdx--callback #'ignore)
  (setq bdx--done-callback #'ignore)
  (setq bdx--error-callback #'ignore)
  (when (processp bdx--last-process)
    (delete-process bdx--last-process)
    (accept-process-output bdx--last-process 0.1))

  (let ((depth (minibuffer-depth))
        (start (current-time)))
    (or (ivy-more-chars)
        (prog1 bdx--prev-candidates
          (setq
           bdx--last-process
           (bdx--search-async
            string
            :callback
            (lambda (results)
              (setq bdx--all-candidates
                    (append bdx--all-candidates
                            (mapcar
                             (lambda (result)
                               (propertize (plist-get result :name)
                                           'bdx-data result))
                             results)))
              (pcase-dolist ((map (:outdated
                                   (map (:binary binary-outdated)
                                        (:symbol symbol-outdated)))
                                  :path :source)
                             results)
                (when symbol-outdated
                  (add-to-list 'bdx--outdated-files path))
                (when binary-outdated
                  (add-to-list 'bdx--sources-needing-recompilation source)))
              (cond
               (bdx--outdated-files
                (setq bdx--last-warning
                      (format "Warning: %s file(s) outdated, re-index needed"
                              (length bdx--outdated-files))))
               (bdx--sources-needing-recompilation
                (setq bdx--last-warning
                      (format
                       (concat
                        "Warning: %s binary file(s) are older than their"
                        " source files, re-compilation and re-index needed")
                       (length bdx--sources-needing-recompilation)))))
              (when (and (eq (minibuffer-depth) depth)
                         (> (time-to-seconds (time-since start)) 1.0))
                ;; To minimize the flicker as the user types (when the old
                ;; candidate list gets removed, minibuffer is cleared and we
                ;; haven't yet received any output from the process to
                ;; repopulate the minibuffer), wait some time before updating
                ;; the minibuffer with this batch.  (time-since start) is
                ;; basically time since last character was typed.
                (ivy-update-candidates bdx--all-candidates)))
            :done-callback
            (lambda ()
              (when (eq (minibuffer-depth) depth)
                (ivy-update-candidates bdx--all-candidates)
                (setq bdx--prev-candidates bdx--all-candidates)))
            :error-callback
            (lambda (err-string) (setq bdx--last-error err-string))))))))

(defun bdx-complete ()
  "Complete the string before point using bdx complete-query command.
E.g. when the string before point is \"section:.b\" it will allow
selecting all possible completions for section that start with
\".b\"."
  (interactive)
  (let* ((pt (point))
         (line
          (buffer-substring-no-properties (line-beginning-position) pt))
         common-prefix
         (start-of-sexp (save-excursion
                          (condition-case nil
                              (progn (backward-sexp) (point))
                            (error (line-beginning-position)))))
         (chars-to-remove (- start-of-sexp (line-beginning-position)))
         command completion candidates)
    (save-match-data
      (setq command (bdx--command "complete-query" line))
      (setq candidates
            (mapcar (lambda (cand)
                      (if (>= (length cand) chars-to-remove)
                          (substring cand chars-to-remove)
                        cand))
                    (with-temp-buffer
                      (apply #'call-process (car command) nil
                             (list (current-buffer) nil) nil
                             (cdr command))
                      (split-string (buffer-string) "[\n\r]"))))
      (cond
       ((or (null candidates)
            (and (null (cdr candidates))
                 (string-empty-p (car candidates))))
        (error "No completions for %S" line))
       ((null (cdr candidates)) (setq completion (car candidates)))
       ((and (setq common-prefix (try-completion line candidates))
             (not (eq t common-prefix))
             (not (string= common-prefix line)))
        (setq completion common-prefix))
       (t
        (setq completion
              (completing-read "Completion: " candidates)))))

    (let ((inhibit-read-only t))
      (delete-region start-of-sexp pt)
      (insert completion))))

(defvar bdx-search-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'bdx-toggle-name-demangling)
    (define-key map (kbd "C-c C-f") #'bdx-toggle-filename)
    (define-key map (kbd "C-c C-t") #'bdx-toggle-templates)
    (define-key map (kbd "C-c C-s") #'bdx-toggle-sections)
    (define-key map [remap ivy-partial-or-done] #'bdx-complete)
    map)
  "Keymap used in minibuffer in `bdx-query'.")

(defvar bdx--demangle-names)
(defvar bdx--show-filenames)
(defvar bdx--show-templates)
(defvar bdx--show-sections)

(cl-defun bdx-query (prompt &key initial-input history require-match
                            action)
  "Search for single symbol with PROMPT.
INITIAL-INPUT if non-nil is inserted into the minibuffer as the
initial input string.

HISTORY can be a history variable.

REQUIRE-MATCH if non-nil will disallow exiting without selecting
a symbol.

ACTION can be a function taking one argument that will be called
with SYMBOL-PLIST for the selected symbol."
  ;; Exit early if index does not exist
  (with-temp-buffer
    (let* ((command (bdx--command "search" "--check-index-exists"))
           (res (apply #'call-process (car command) nil (current-buffer) nil
                       (cdr command))))
      (unless (= 0 res)
        (error (buffer-string)))))

  (setq bdx--demangle-names nil)
  (setq bdx--show-filenames t)
  (setq bdx--show-templates t)
  (setq bdx--show-sections t)
  (setq bdx--query-buffer (current-buffer))
  (setq bdx--last-error nil)
  (ivy-read prompt #'bdx--ivy-collection-function
            :require-match require-match
            :dynamic-collection t
            :keymap bdx-search-keymap
            :caller 'bdx
            :history history
            :initial-input initial-input
            :action (lambda (selection)
                      (when action
                        (funcall action (bdx-data selection))))
            :unwind (lambda ()
                      (setq bdx--query-buffer nil)
                      (setq bdx--callback #'ignore)
                      (setq bdx--done-callback #'ignore)
                      (setq bdx--error-callback #'ignore)
                      (setq bdx--all-candidates nil)
                      (when (processp bdx--last-process)
                        (delete-process bdx--last-process)
                        (accept-process-output bdx--last-process 0.1)))
            :extra-props `(:bdx-action ,action)))

(cl-defun bdx-get-query (prompt &key history)
  "Get a search query from the user, with results preview, using PROMPT.
HISTORY can be a history variable."
  (bdx-query prompt :history history)
  ivy-text)

(defvar bdx--demangle-names nil
  "If non-nil, the symbols in minibuffer are demangled.")

(defun bdx-toggle-name-demangling ()
  "Toggle name demangling in current search session."
  (interactive)
  (setq bdx--demangle-names (not bdx--demangle-names)))

(defvar bdx--show-filenames t
  "If non-nil, the filenames are shown in the minibuffer.")

(defun bdx-toggle-filename ()
  "Toggle showing filenames in current search session."
  (interactive)
  (setq bdx--show-filenames (not bdx--show-filenames)))

(defvar bdx--show-templates t
  "If nil, the templates are hidden in the minibuffer.")

(defun bdx-toggle-templates ()
  "Toggle showing templates in current search session."
  (interactive)
  (setq bdx--show-templates (not bdx--show-templates)))

(defvar bdx--show-sections t
  "If nil, the sections are hidden in the minibuffer.")

(defun bdx-toggle-sections ()
  "Toggle showing section names in current search session."
  (interactive)
  (setq bdx--show-sections (not bdx--show-sections)))

(defun bdx--untemplatize-string (str)
  "Remove C++ templates from STR.
This turns a string of the form \\='function<type<T>>\\=' into
\\='function<...>\\='."
  (cl-loop with depth = 0
           for char across str
           if (eq char ?<) do (when (eq 1 (cl-incf depth))
                                (setq chars (nconc chars (list ?< ?. ?. ?.))))
           else if (eq char ?>) do (when (zerop (cl-decf depth))
                                     (setq chars (nconc chars (list ?>))))
           else if (zerop depth) collect char into chars
           finally return (concat chars)))

(defun bdx--ivy-display-transformer (string)
  "Return a string for displaying STRING in the minibuffer."
  (if-let* ((data (bdx-data string)))
      (cl-destructuring-bind
          (&key name section path demangled &allow-other-keys) data
        (concat
         (let ((line
                (if (and demangled bdx--demangle-names)
                    (if bdx--show-templates
                        demangled
                      (bdx--untemplatize-string demangled))
                  name)))
           (dolist (word (split-string ivy-text))
             (when (ignore-errors
                     (let ((case-fold-search t))
                       (string-match word line)))
               (add-face-text-property (match-beginning 0) (match-end 0)
                                       'ivy-minibuffer-match-face-2 nil line)))
           line)
         (and bdx--show-sections
              (propertize (concat " [" section "]")
                          'face 'ivy-grep-info))
         (and bdx--show-filenames
              (concat " "
                      (propertize
                       (file-name-nondirectory (or path ""))
                       'face 'shadow)))))
    ""))

(ivy-configure 'bdx
  :display-transformer-fn #'bdx--ivy-display-transformer
  :more-chars 2)

(defun bdx--ivy-prompt ()
  "Return a prompt for `bdx-query' search."
  (let ((message-log-max nil))
    (cond
     (bdx--last-error
      (message "%s" (propertize bdx--last-error 'face 'error)))
     (bdx--last-warning
      (message "%s" (propertize bdx--last-warning 'face 'warning)))))
  (let* ((cur (ivy-state-current ivy-last))
         (data (and cur (bdx-data cur)))
         (index (1+ (or (and data (plist-get data :index)) 0)))
         (total (or (and data (plist-get data :total)) 0)))
    (format "%-12s %s"
            (if bdx--last-error "ERR"
              (format "%s%s/%s"
                      index
                      (if (process-live-p bdx--last-process) "++" "")
                      total))
            (ivy-state-prompt ivy-last))))

(ivy-set-prompt 'bdx #'bdx--ivy-prompt)


;; Occur

(defun bdx--occur (cands)
  "Create ivy occur buffer with CANDS."

  ;; TODO: Figure out why we can't use just CANDS and have to do this.
  (setq cands ivy--all-candidates)

  (let ((inhibit-read-only t)
        (total-found
         (or (and cands (plist-get (bdx-data (car cands)) :total)) 0))
        (action (plist-get (ivy-state-extra-props ivy-last)
                           :bdx-action)))
    (erase-buffer)
    (setq buffer-read-only t)

    (insert "Search query: " (propertize (format "%S" ivy-text)
                                         'face 'font-lock-string-face)
            "\n")
    (insert "Command: "
            (with-current-buffer bdx-stderr-buffer
              (if (string-match "Command: \\(.*\\)" (buffer-string))
                  (match-string 1 (buffer-string))
                "n/a"))
            "\n")
    (insert "\n")

    (insert (format "Found %s candidates in total, retrieved %s:\n\n"
                    total-found (length cands)))

    (cl-flet ((insert-row
                (name string &optional face)
                (insert "    "
                        (propertize name 'face 'font-lock-keyword-face)
                        ": "
                        (if (stringp string)
                            (propertize string 'face face)
                          (propertize
                           "(ERROR: Invalid value: nil)" 'face 'error))
                        "\n")))
      (dolist (cand cands)
        (pcase-let (((map (:outdated (map (:binary binary-outdated)
                                          (:symbol symbol-outdated)))
                          :demangled :name :path
                          :size :section :address :type)
                     (bdx-data cand)))
          (insert (propertize (or demangled name "(ERROR: No name)")
                              'face 'font-lock-constant-face))
          (make-button (line-beginning-position) (line-end-position)
                       'action (lambda (&rest _args)
                                 (funcall action (bdx-data cand))))
          (insert ":\n")
          (cond
           (binary-outdated
            (insert "    " (propertize "Warning:" 'face 'warning)
                    " binary is outdated, need to re-compile\n"))
           (symbol-outdated
            (insert "    " (propertize "Warning:" 'face 'warning)
                    " provided data is outdated, re-index needed\n")))
          (insert-row "name" name 'font-lock-constant-face)
          (insert-row "path" path 'font-lock-comment-face)
          (insert-row "section" section 'font-lock-constant-face)
          (insert-row "address" (and address (format "0x%x" address))
                      'font-lock-number-face)
          (insert-row "size" (and size (format "0x%x (%s)" size size))
                      'font-lock-number-face)
          (insert-row "type" type 'font-lock-type-face)
          (insert "\n")))))

  (when (fboundp 'button-mode)
    (button-mode +1))
  (goto-char (point-min)))

(ivy-configure 'bdx :occur #'bdx--occur)


;; Disassembly

(defvar bdx-disassembly-buffer "*bdx disassembly %s*"
  "String template for the disassembly buffer name.")

(defvar bdx-disassembly-results-limit 10
  "If non-nil, then only disassemble at most that many symbols in one buffer.")

(defvar bdx-disassembly-hook nil
  "Hook called at the end of `bdx-disassemble'.")

(defvar-local bdx-disassembly-current-symbol
    nil "Currently disassembled symbol.")
(defvar-local bdx-disassembly-stack nil
  "Stack of previously disassembled symbols.
Each item is a list (SYMBOL-PLIST POINT WINDOW-START), where POINT is
the point in the disassembly buffer and WINDOW-START is the value of
`window-start', and it's used to restore the window position.")
(defvar-local bdx-disassembly-forward-stack nil
  "Stack of disassembled symbols for going forward.
Each element is as in `bdx-disassembly-stack'.")

(defvar bdx-disassembly-mode)

(defun bdx--disassembly-buffer ()
  "Get the disassembly buffer."
  (if bdx-disassembly-mode
      (current-buffer)
    (let* ((name
            (or (and bdx-index-path
                     (format "index:%s" (abbreviate-file-name bdx-index-path)))
                (and bdx-binary-directory
                     (format "dir:%s"
                             (abbreviate-file-name bdx-binary-directory)))
                (and (project-current)
                     (format "project:%s"
                             (project-name (project-current))))
                (abbreviate-file-name default-directory)
                "<global>"))
           (buffer-name (format bdx-disassembly-buffer name))
           (buffer (get-buffer buffer-name)))
      (unless buffer
        (setq buffer (generate-new-buffer buffer-name))
        (with-current-buffer buffer
          (asm-mode)
          (bdx-disassembly-mode +1)
          (setq-local revert-buffer-function #'bdx-revert-disassembly-buffer)))
      buffer)))

(defun bdx-revert-disassembly-buffer (&optional _ignore-auto _noconfirm)
  "Revert the current disassembly buffer.
IGNORE-AUTO and NOCONFIRM are unused."
  (interactive)
  (unless bdx-disassembly-current-symbol
    (error "Not in a disassembly buffer"))
  (let ((bdx-disassembly-stack nil)
        (bdx-disassembly-forward-stack nil))
    (bdx-disassemble bdx-disassembly-current-symbol)))

(defun bdx-disassemble (symbol-plist)
  "Disassemble the symbol encoded in SYMBOL-PLIST.
Interactively, prompts for a query and allows selecting a single
symbol."
  (interactive (list 'interactive))
  (if (eq symbol-plist 'interactive)
      (bdx-query "Disassemble symbol: " :require-match t
                 :action #'bdx-disassemble)
    (with-current-buffer (bdx--disassembly-buffer)
      (pcase-let (((map :name :demangled :path :section) symbol-plist))
        (let ((args
               (append '("disass")
                       (and bdx-disassembly-results-limit
                            (list "-n" (number-to-string
                                        bdx-disassembly-results-limit)))
                       (list
                        (and name (format "fullname:\"%s\"" name))
                        (and demangled
                             (format "demangled:\"%s\"" demangled))
                        (and path (format "path:\"%s\"" path))
                        (and section
                             (format "section:\"%s\"" section)))))
              (current-state
               (and bdx-disassembly-current-symbol
                    (list bdx-disassembly-current-symbol
                          (point) (window-start))))
              (inhibit-read-only t))
          (erase-buffer)

          (pop-to-buffer (current-buffer))
          (bdx--run-sync-process args)

          (run-hooks 'bdx-disassembly-hook)

          (goto-char (point-min))

          (setq buffer-read-only t)
          (setq buffer-undo-list t)

          (when current-state (push current-state bdx-disassembly-stack))
          (setq bdx-disassembly-current-symbol symbol-plist)

          (when (equal symbol-plist (caar bdx-disassembly-forward-stack))
            (pop bdx-disassembly-forward-stack)))))))

(defun bdx-disassemble-name (name)
  "Disassemble the symbol named NAME.
NAME can be either mangled or demangled.  This is just a wrapper for
`bdx-disassemble'."
  (bdx-disassemble `(:name ,name)))

(defun bdx--disassemble-from-history-var (forward)
  "Disassemble the previously disassembled sym, or the next one if FORWARD."
  (let ((stack (if forward 'bdx-disassembly-forward-stack
                 'bdx-disassembly-stack))
        (reverse-stack (if forward 'bdx-disassembly-stack
                         'bdx-disassembly-forward-stack)))
    (when bdx-disassembly-current-symbol
      (push (list bdx-disassembly-current-symbol (point) (window-start))
            (symbol-value reverse-stack)))
    (pcase-let
        ((`(,item ,point ,window-start) (pop (symbol-value stack)))
         (total-count (+ 1 (length bdx-disassembly-stack)
                         (length bdx-disassembly-forward-stack)))
         (pos (1+ (length bdx-disassembly-stack)))
         (bdx-disassembly-stack nil))
      (bdx-disassemble item)
      (ignore-errors (goto-char point))
      (ignore-errors (set-window-start (selected-window) window-start))
      (message "History item %s/%s" pos total-count))))

(defun bdx-disassemble-previous ()
  "Go back to the previous symbol's disassembly."
  (interactive)
  (unless bdx-disassembly-stack
    (error "The previous item stack is empty"))
  (bdx--disassemble-from-history-var nil))

(defun bdx-disassemble-next ()
  "Go forward to the next symbol's disassembly."
  (interactive)
  (unless bdx-disassembly-forward-stack
    (error "The forward item stack is empty"))
  (bdx--disassemble-from-history-var t))

(defun bdx-disassembly-goto-item (item)
  "Disassemble an ITEM from history.
ITEM should be an index of the item.  0 means the the oldest item
disassembled in this buffer.  The current item will have the index equal
to the length of `bdx-disassembly-stack'."
  (interactive
   (let ((items
          (mapcar (pcase-lambda (`(,item _ _))
                    (or (plist-get item :demangled)
                        (plist-get item :name)))
                  (append (reverse bdx-disassembly-stack)
                          (when bdx-disassembly-current-symbol
                            (list
                             (list bdx-disassembly-current-symbol
                                   (point) (window-start))))
                          bdx-disassembly-forward-stack))))
     (list
      (cl-position (completing-read "Go to history item: " items nil t)
                   items :test #'string=))))
  (let ((elems
         (append (reverse bdx-disassembly-stack)
                 (when bdx-disassembly-current-symbol
                   (list
                    (list bdx-disassembly-current-symbol
                          (point) (window-start))))
                 bdx-disassembly-forward-stack)))
    (when (or (cl-minusp item) (>= item (length elems)))
      (error "Item out of bounds: %s %s" item (length elems)))
    (setq bdx-disassembly-current-symbol nil)
    (setq bdx-disassembly-stack (reverse (seq-subseq elems 0 item)))
    (pcase-let ((bdx-disassembly-stack nil)
                (bdx-disassembly-forward-stack nil)
                (`(,item ,point ,window-start) (seq-elt elems item)))
      (bdx-disassemble item)
      (ignore-errors (goto-char point))
      (ignore-errors (set-window-start (selected-window) window-start)))
    (setq bdx-disassembly-forward-stack (seq-subseq elems (1+ item)))))

(defvar bdx-disassembly-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M-p") #'bdx-disassemble-previous)
    (define-key map (kbd "C-c M-n") #'bdx-disassemble-next)
    (define-key map (kbd "C-c M-g") #'bdx-disassembly-goto-item)
    (define-key map (kbd "C-c M-r") #'bdx-revert-disassembly-buffer)
    map)
  "Keymap used in `bdx-disassembly-mode'.")

(define-minor-mode bdx-disassembly-mode
  "Minor mode used in disassembly buffers."
  :lighter " bdx"
  :keymap bdx-disassembly-mode-map)


;; Find definition

(defun bdx-find-definition (symbol-plist)
  "Find file containing definition of SYMBOL-PLIST and go to definition line.
The return value is a cons (FILE . LINE).
If SYMBOL-PLIST is the symbol \\='interactive, then prompt for the symbol."
  (interactive (list 'interactive))
  (if (eq symbol-plist 'interactive)
      (list (bdx-query "Find definition: " :require-match t
                       :action #'bdx-find-definition))
    (pcase-let (((map :name :path :section) symbol-plist))
      (let ((args
             (append '("find-definition" "-n" "1")
                     (list
                      (and name (format "fullname:\"%s\"" name))
                      (and path (format "path:\"%s\"" path))
                      (and section
                           (format "section:\"%s\"" section)))))
            file line sym)
        (with-temp-buffer
          (bdx--run-sync-process args)

          (goto-char (point-min))
          (if (looking-at "^\\(.*\\):\\([0-9]+\\): \\(.*\\)")
              (setq file (match-string 1)
                    line (string-to-number (match-string 2))
                    sym (match-string 3))
            (error "No definition found")))
        (when (equal sym name)
          (with-current-buffer (or (find-buffer-visiting file)
                                   (find-file-noselect file))
            (pop-to-buffer (current-buffer))
            (goto-char (point-min))
            (forward-line (1- line))
            (pulse-momentary-highlight-one-line (point))
            (recenter-top-bottom)
            (cons file line)))))))


;; Graphs

(defun bdx-generate-graph (start-query goal-query &optional output-buffer)
  "Generate graph from START-QUERY to GOAL-QUERY.
The graph is output to OUTPUT-BUFFER, or the current buffer if that's nil."
  (setq output-buffer (or output-buffer (current-buffer)))
  (with-current-buffer (get-buffer-create bdx-stderr-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (let ((proc (make-process
               :name "bdx-graph-generation"
               :buffer output-buffer
               :stderr (get-buffer-create bdx-stderr-buffer)
               :command (bdx--command
                         "graph" "--json-progress" start-query goal-query)
               :sentinel #'ignore
               :noquery t)))
    (unwind-protect
        (let ((inhibit-quit t))
          (unless
              (with-local-quit
                (cl-loop
                 do (accept-process-output proc 0.2)

                 with num-done = 0
                 with num-total = 0
                 with symbols-visited = 0
                 with routes-found = 0

                 do
                 (let ((message-log-max nil))
                   (message
                    (concat
                     "Progress: %.2f%% [%d/%d] [%d symbols visited, "
                     "%d routes found]")
                    (if (process-live-p proc)
                        (min 100.0 (* 100.0 (/ (float num-done)
                                               (float (max 1 num-total)))))
                      100)
                    num-done num-total symbols-visited routes-found))

                 for progress-msg =
                 (and (get-buffer bdx-stderr-buffer)
                      (with-current-buffer (get-buffer bdx-stderr-buffer)
                        (goto-char (point-min))
                        (ignore-errors
                          (re-search-forward "^[{].*[}]$")
                          (goto-char (line-beginning-position))
                          (let ((json-object-type 'plist))
                            (prog1 (json-read)
                              (delete-region
                               (line-beginning-position)
                               (min (1+ (point)) (point-max))))))))

                 do (pcase progress-msg
                      ((map (:done (and done (pred integerp)))
                            (:total (and total (pred integerp))))
                       (setq num-done done)
                       (setq num-total total))
                      ((map (:visited (and visited (pred integerp))))
                       (setq symbols-visited visited))
                      ((map (:found (and found (pred integerp))))
                       (setq routes-found found)))

                 while (or (process-live-p proc) progress-msg)
                 finally return t))
            (interrupt-process proc)
            (setq quit-flag nil))
          (setq inhibit-quit nil)
          (while (process-live-p proc)
            (accept-process-output proc 0.1)))
      (delete-process proc))))

(defvar bdx-graphviz-program "dot"
  "Program used to generate images from graphviz graphs received on stdin.")

(cl-defun bdx-generate-graph-image
    (start-query goal-query &key (image-type "svg"))
  "Generate a graph image from START-QUERY to GOAL-QUERY and return it's path.
IMAGE-TYPE can be a string for the image type, e.g. png or svg.
It is used as an argument to dot's -T option."
  (let* ((res-file (make-temp-file "bdx-graph" nil (concat "." image-type)))
         image-buf)
    (with-temp-file res-file
      (setq image-buf (current-buffer))
      (with-temp-buffer
        (bdx-generate-graph start-query goal-query)
        (call-process-region
         (point-min) (point-max)
         (executable-find bdx-graphviz-program)
         nil image-buf nil "-T" image-type)))
    res-file))

(defvar bdx-show-graph-query-history nil
  "History variable for `bdx-show-graph-xdg-open'.")

(defun bdx-show-graph-xdg-open (start-query goal-query)
  "Show a graph from START-QUERY to GOAL-QUERY using xdg-open."
  (interactive
   (let (start goal)
     (let ((bdx-show-graph-query-history
            (purecopy bdx-show-graph-query-history)))
       (setq start (bdx-get-query "Generate graph start query: "
                                  :history 'bdx-show-graph-query-history)))
     (add-to-history 'bdx-show-graph-query-history start)
     (let ((bdx-show-graph-query-history
            (purecopy bdx-show-graph-query-history)))
       (setq goal (bdx-get-query "Generate graph goal query: "
                                 :history 'bdx-show-graph-query-history)))
     (add-to-history 'bdx-show-graph-query-history goal)
     (list start goal)))
  (browse-url-xdg-open
   (bdx-generate-graph-image start-query goal-query)))

(provide 'bdx)
;;; bdx.el ends here
