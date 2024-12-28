;;; bdx.el --- Frontend for bdx -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: tools, c

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
;;   Read a bdx query from the user with `ivy', and use `binfile-disassemble'
;;   (or a custom function) to disassemble the selected symbol.
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
;;   Read a bdx query from the user, interactively displaying results, and
;;   return symbol data.
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

;; `require'-ing `map' does not guarantee it loaded as it is preloaded in
;; Emacs.
;;
;; This hack was stolen from the built-in eglot.el.
(eval-and-compile
  (if (< emacs-major-version 28)
      (load "map" nil 'nomessage)
    (require 'map)))

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
      (with-current-buffer (get-buffer-create bdx-stderr-buffer)
        (let ((inhibit-read-only t))
          (princ (format "Command: %S\n" cmd) (current-buffer))))
      cmd)))


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

(defcustom bdx-demangle-names t
  "If non-nil, request to demangle C++ names."
  :type 'boolean)

(cl-defun bdx--search-async (query &key
                                   (callback #'ignore)
                                   (done-callback #'ignore)
                                   (error-callback #'ignore))
  "Start and return a search process, searching for QUERY.
CALLBACK will be called for a batch of symbols found.  It must
accept a single argument, which will be the batch, a list of
symbols serialized as plists.

DONE-CALLBACK will be called once the process exits.
ERROR-CALLBACK will be called with an error string on errors.

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
   :command (bdx--command
             "search" "-f" "sexp"
             (and bdx-demangle-names "--demangle-names")
             "--" query)
   :filter #'bdx--process-filter
   :sentinel #'bdx--process-sentinel))

(defun bdx-data (string)
  "Get the symbol plist from STRING.
The symbol data, like it's path and section, address are stored
as a property of the string."
  (get-text-property 0 'bdx-data string))

(defvar bdx--last-process nil)
(defvar bdx--all-candidates nil)
(defvar bdx--outdated-files nil)

(defvar bdx--last-error nil)
(defvar bdx--last-warning nil)

(defun bdx--ivy-collection-function (string &rest _args)
  "Collect candidates for query STRING.
This should be used as COLLECTION for `ivy-read'."
  (setq bdx--all-candidates nil)
  (setq bdx--outdated-files nil)
  (setq bdx--last-error nil)
  (setq bdx--last-warning nil)
  (setq bdx--callback #'ignore)
  (setq bdx--done-callback #'ignore)
  (setq bdx--error-callback #'ignore)
  (when (processp bdx--last-process)
    (delete-process bdx--last-process)
    (accept-process-output bdx--last-process 0.1))

  (let ((depth (minibuffer-depth)))
    (or (ivy-more-chars)
        (prog1 '("" "working...")
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
              (dolist (res results)
                (when (plist-get res :outdated)
                  (add-to-list 'bdx--outdated-files (plist-get res :path))))
              (when bdx--outdated-files
                (setq bdx--last-warning
                      (format "Warning: %s file%s outdated, re-index needed"
                              (length bdx--outdated-files)
                              (if (cdr bdx--outdated-files) "s are" " is"))))
              (when (eq (minibuffer-depth) depth)
                (ivy-update-candidates bdx--all-candidates)))
            :done-callback
            (lambda ()
              (when (eq (minibuffer-depth) depth)
                (ivy-update-candidates bdx--all-candidates)))
            :error-callback
            (lambda (err-string) (setq bdx--last-error err-string))))))))

(defun bdx-complete ()
  "Complete the string before point using bdx complete-prefix command.
E.g. when the string before point is \"section:.b\" it will allow
selecting all possible completions for section that start with
\".b\"."
  (interactive)
  (let (field prefix command completion
              (known-fields
               '("path" "source" "name" "fullname"
                 "section" "address" "size"
                 "type" "relocations" "mtime"))
              candidates common-prefix)
    (cond
     ((looking-back "\\(\\b[a-z]+\\):\\([^ \n]*\\)")
      (setq field (match-string-no-properties 1)
            prefix (match-string-no-properties 2)))
     ((looking-back "\\([^ \n]+\\)" (line-beginning-position) t)
      (setq prefix (match-string-no-properties 1)))
     (t (setq prefix "")))

    (save-match-data
      (setq command (bdx--command "complete-prefix" (or field "name") prefix))
      (setq candidates
            (with-temp-buffer
              (apply #'call-process (car command) nil
                     (list (current-buffer) nil) nil
                     (cdr command))

              ;; Also complete the field name, if it's not
              ;; provided
              (unless field
                (dolist (known-field known-fields)
                  (when (string-prefix-p prefix known-field)
                    (insert known-field ":\n"))))

              (split-string (buffer-string))))
      (cond
       ((null candidates) (error "No completions for %S" prefix))
       ((null (cdr candidates)) (setq completion (car candidates)))
       ((and (setq common-prefix (try-completion prefix candidates))
             (not (string= common-prefix prefix)))
        (setq completion common-prefix))
       (t
        (setq completion
              (completing-read (format "Completion for \"%s\": "
                                       (if field
                                           (format "%s:%s" field prefix)
                                         prefix))
                               candidates nil nil prefix)))))
    (let ((inhibit-read-only t))
      (replace-match
       (if field
           (format "%s:%s" field completion)
         completion)))))

(defvar bdx-search-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") #'bdx-toggle-name-demangling)
    (define-key map (kbd "<tab>") #'bdx-complete)
    map)
  "Keymap used in minibuffer in `bdx-query'.")

(defvar bdx--demangle-names)

(cl-defun bdx-query (prompt &key initial-input history require-match)
  "Search for single symbol with PROMPT.
INITIAL-INPUT if non-nil is inserted into the minibuffer as the
initial input string.

HISTORY can be a history variable.

REQUIRE-MATCH if non-nil will disallow exiting without selecting
a symbol."
  (setq bdx--demangle-names nil)
  (setq bdx--query-buffer (current-buffer))
  (setq bdx--last-error nil)
  (bdx-data
   (ivy-read prompt #'bdx--ivy-collection-function
             :require-match require-match
             :dynamic-collection t
             :keymap bdx-search-keymap
             :caller 'bdx
             :history history
             :initial-input initial-input
             :unwind (lambda ()
                       (setq bdx--query-buffer nil)
                       (setq bdx--callback #'ignore)
                       (setq bdx--done-callback #'ignore)
                       (setq bdx--error-callback #'ignore)
                       (when (processp bdx--last-process)
                         (delete-process bdx--last-process)
                         (accept-process-output bdx--last-process 0.1))))))

(cl-defun bdx-get-query (prompt &key history)
  "Get a search query from the user, with results preview, using PROMPT.
HISTORY can be a history variable."
  (bdx-query prompt :history history)
  ivy-text)

(defvar bdx--demangle-names nil
  "If non-nil, the symbols in minibuffer are demangled.")

(defun bdx-toggle-name-demangling ()
  "Toggle name demangling in current search session.
This will error if `bdx-demangle-names' is nil."
  (interactive)
  (unless bdx-demangle-names
    (error "Demangling is disabled by user"))
  (setq bdx--demangle-names (not bdx--demangle-names)))

(defun bdx--ivy-display-transformer (string)
  "Return a string for displaying STRING in the minibuffer."
  (if-let* ((data (bdx-data string)))
      (cl-destructuring-bind
          (&key name section path demangled &allow-other-keys) data
        (concat
         (if (and demangled bdx--demangle-names) demangled name)
         " "
         (propertize (concat "[" section "]")
                     'face 'ivy-grep-info)
         " "
         (propertize (file-relative-name (or path ""))
                     'face 'shadow)))
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
         (or (and cands (plist-get (bdx-data (car cands)) :total)) 0)))
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
        (pcase-let (((map :outdated :demangled :name :path
                          :size :section :address :type
                          )
                     (bdx-data cand)))
          (insert (propertize (or demangled name "(ERROR: No name)")
                              'face 'font-lock-constant-face)
                  ":\n")
          (when outdated
            (insert "    " (propertize "Warning:" 'face 'warning)
                    " provided data is outdated, re-index needed\n"))
          (insert-row "name" name 'font-lock-constant-face)
          (insert-row "path" path 'font-lock-comment-face)
          (insert-row "section" section 'font-lock-constant-face)
          (insert-row "address" (and address (format "0x%x" address))
                      'font-lock-number-face)
          (insert-row "size" (and size (format "0x%x (%s)" size size))
                      'font-lock-number-face)
          (insert-row "type" type 'font-lock-type-face)
          (insert "\n")))))
  (goto-char (point-min)))

(ivy-configure 'bdx :occur #'bdx--occur)


;; Disassembly

(defun bdx-disassemble-binfile (symbol-plist)
  "Disassemble SYMBOL-PLIST with `binfile-disassemble'."
  (eval-and-compile (require 'binfile))
  (binfile-disassemble (plist-get symbol-plist :name)
                       (plist-get symbol-plist :path)))

(defun bdx-disassemble-symbol-at-point-binfile ()
  "Try to get the symbol at point using `binfile' function."
  (eval-and-compile (require 'binfile))
  (eval-and-compile (require 'objdump))
  (when-let* ((symbol (car-safe (binfile--symbol-and-offset-at-point))))
    (setq symbol (objdump-mangle symbol))
    (if (string-match ".hidden \\(.*\\)" symbol)
        (match-string 1 symbol)
      symbol)))

(defvar bdx-disassemble-function #'bdx-disassemble-binfile
  "Function to disasssemble selected symbols.
It must accept a single argument, the symbol plist.")

(defvar bdx-disassemble-symbol-at-point-function
  #'bdx-disassemble-symbol-at-point-binfile
  "Function to get a symbol at point.
It must be callable with zero arguments and return a string or nil.")

(defun bdx-disassemble (symbol-plist)
  "Disassemble the symbol encoded in SYMBOL-PLIST.
Interactively, prompts for a query and allows selecting a single
symbol.

The function used for disassembly is set in
`bdx-disassemble-function'."
  (interactive
   (list (bdx-query "Disassemble symbol: "
                     :require-match t
                     :initial-input
                     (funcall bdx-disassemble-symbol-at-point-function))))
  (funcall bdx-disassemble-function symbol-plist))


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
