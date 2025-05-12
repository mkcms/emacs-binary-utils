;;; bdx-test.el --- Tests for bdx.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: tests
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

;;; Code:

(require 'ert)
(require 'bdx)

(cl-defun bdx-test--compile-file
    (filename source &key args)
  (let* ((dir (file-name-directory filename))
         (out-filename (concat filename ".o"))
         (opts (cl-list* filename "-o" out-filename args))
         (prog "gcc")
         gcc-rv)
    (when dir
      (make-directory dir t))
    (with-temp-buffer
      (with-temp-file filename (insert source))
      (with-temp-buffer
        (message
         "Running %s with args %S in dir %s" prog opts default-directory)
        (setq gcc-rv (apply #'call-process prog nil (current-buffer) nil opts))
        (unless (zerop gcc-rv)
          (error
           "GCC failed with code %s, src %s, dst %s, dir %s, args %s:\n\n%s"
           gcc-rv filename out-filename default-directory opts
           (buffer-string)))))))

(defmacro bdx-test--with-files (files &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((dir (gensym)))
    `(let* ((,dir (make-temp-file "emacs-bdx-test--with-files" t))
            (bdx-index-path (expand-file-name "index" ,dir))
            (default-directory ,dir))
       (mapc (lambda (args)
               (apply #'bdx-test--compile-file args))
             ,files)
       ,@body
       (delete-directory ,dir t))))

(defun bdx-test--index (&optional dir)
  (with-temp-buffer
    (let* ((bdx-binary-directory (or dir default-directory))
           (cmd (combine-and-quote-strings
                 (bdx--command "index" "--opt" "index_relocations=True")))
           (rv (call-process-shell-command cmd nil (current-buffer))))
      (unless (zerop rv)
        (error "'%s' failed for %s: %s"
               cmd (or dir default-directory)
               (string-trim (buffer-string)))))))

(ert-deftest bdx-search-async ()
  (bdx-test--with-files
      `(("foo.c" "
int foo() { return 0; }
int bar() { return 0; }
" :args ("-c"))
        ("subdir/file.c" "
int subdir_file() { return 0; }
" :args ("-c")))
    (bdx-test--index)

    (let (results proc)
      (setq proc (bdx--search-async
                  "path:foo*" :callback
                  (lambda (batch)
                    (setq results (nconc results batch)))))
      (should-not results)
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (should results)

      (setq results (seq-sort-by (lambda (x) (plist-get x :name))
                                 #'string-lessp results))

      (should (equal (expand-file-name "foo.c.o")
                     (plist-get (nth 0 results) :path)))
      (should (integerp (plist-get (nth 0 results) :index)))
      (should (integerp (plist-get (nth 0 results) :total)))
      (should (plist-member (nth 0 results) :outdated))
      (should (equal "bar" (plist-get (nth 0 results) :name)))

      (should (equal (expand-file-name "foo.c.o")
                     (plist-get (nth 1 results) :path)))
      (should (equal "foo" (plist-get (nth 1 results) :name)))

      (should (equal 2 (length results))))

    (let (results proc)
      (setq proc (bdx--search-async
                  "path:subdir/*" :callback
                  (lambda (batch)
                    (setq results (nconc results batch)))))
      (should-not results)
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (should results)

      (should (equal (expand-file-name "subdir/file.c.o")
                     (plist-get (nth 0 results) :path)))
      (should (equal "subdir_file" (plist-get (nth 0 results) :name)))

      (should (equal 1 (length results))))))

(ert-deftest bdx-search-async-small-buffer ()
  "Check that the process filter works when we receive small stdout chunks."
  (bdx-test--with-files
      `(("foo.c" "
int foo() { return 0; }
int bar() { return 0; }
" :args ("-c")))
    (bdx-test--index)

    (let (results proc filter)
      (setq proc (bdx--search-async
                  "path:foo*" :callback
                  (lambda (batch)
                    (setq results (nconc results batch)))))
      (setq filter (process-filter proc))
      (set-process-filter
       proc
       (lambda (proc output)
         (while (not (string-empty-p output))
           (let ((chunk (substring output 0
                                   (min (length output) 8))))
             (funcall filter proc chunk)
             (setq output (substring output (length chunk)))))))
      (should-not results)
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (should results)

      (setq results (seq-sort-by (lambda (x) (plist-get x :name))
                                 #'string-lessp results))

      (should (equal (expand-file-name "foo.c.o")
                     (plist-get (nth 0 results) :path)))
      (should (equal "bar" (plist-get (nth 0 results) :name)))

      (should (equal (expand-file-name "foo.c.o")
                     (plist-get (nth 1 results) :path)))
      (should (equal "foo" (plist-get (nth 1 results) :name)))

      (should (equal 2 (length results))))))

(ert-deftest bdx-search-async-done-callback ()
  "Check that :done-callback is called."
  (bdx-test--with-files
      `(("foo.c" " int foo() { return 0; }" :args ("-c")))
    (bdx-test--index)

    (let (results proc (call-count 0))
      (setq proc (bdx--search-async
                  "path:foo*"
                  :callback (lambda (batch)
                                          (should (equal 0 call-count))
                                          (setq results batch))
                  :done-callback (lambda () (cl-incf call-count))))
      (should-not results)
      (should (equal 0 call-count))

      (while (process-live-p proc) (accept-process-output proc 0.1))

      (should results)
      (should (equal 1 call-count)))))

(ert-deftest bdx-search-async-error-callback ()
  "Check that :done-callback is called."
  (bdx-test--with-files `(("foo.c" " int foo() { return 0; }" :args ("-c")))
    (bdx-test--index)

    (let (proc error-str)
      (setq proc (bdx--search-async
                  "UNKNOWN_FIELD:1"
                  :error-callback (lambda (err) (setq error-str err))))
      (should-not error-str)
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (should (string-match-p "error.*Unknown field \"UNKNOWN_FIELD\""
                              error-str)))))

(ert-deftest bdx-search-async-query-string-contains-option ()
  "Check that we don't fail when the query string contains an --option."
  (bdx-test--with-files `(("foo.c" "int foo() { return 0; }" :args ("-c")))
    (bdx-test--index)

    (let (results proc (error-calls 0))
      (setq proc (bdx--search-async
                  "--unknown-option OR path:foo*"
                  :callback (lambda (batch)
                              (setq results (nconc results batch)))
                  :error-callback (lambda (_err) (cl-incf error-calls))))
      (should (= 0 error-calls))
      (should-not results)
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (should (= 0 error-calls))
      (should results)

      (should (equal (expand-file-name "foo.c.o")
                     (plist-get (nth 0 results) :path)))
      (should (equal "foo" (plist-get (nth 0 results) :name)))

      (should (equal 1 (length results))))

    (let (results proc (error-calls 0))
      (setq proc (bdx--search-async
                  "path:foo* OR *:* --index-path ./index -d ./test"
                  :callback (lambda (batch)
                              (setq results (nconc results batch)))
                  :error-callback (lambda (_err) (cl-incf error-calls))))
      (should (= 0 error-calls))
      (should-not results)
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (should (= 0 error-calls))
      (should results))))

(ert-deftest bdx-search-async-many-files ()
  (bdx-test--with-files
      (mapcar (lambda (n)
                `(,(format "file_%s.c" n)
                  ,(format "int symbol_%s() { return 0; }\n" n)
                  :args ("-c")))
              (number-sequence 0 100))
    (bdx-test--index)

    (let (results proc)
      (setq proc (bdx--search-async
                  "path:file_100*" :callback
                  (lambda (batch)
                    (setq results (nconc results batch)))))
      (should-not results)
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (should results)

      (setq results (seq-sort-by (lambda (x) (plist-get x :name))
                                 #'string-lessp results))

      (should (equal (expand-file-name "file_100.c.o")
                     (plist-get (nth 0 results) :path)))
      (should (equal "symbol_100" (plist-get (nth 0 results) :name)))

      (should (equal 1 (length results))))

    (let (results proc)
      (setq proc (bdx--search-async
                  "path:file_30* OR name:symbol_90" :callback
                  (lambda (batch)
                    (setq results (nconc results batch)))))
      (should-not results)
      (while (process-live-p proc) (accept-process-output proc 0.1))
      (should results)

      (setq results (seq-sort-by (lambda (x) (plist-get x :name))
                                 #'string-lessp results))

      (should (equal (expand-file-name "file_30.c.o")
                     (plist-get (nth 0 results) :path)))
      (should (equal "symbol_30" (plist-get (nth 0 results) :name)))

      (should (equal (expand-file-name "file_90.c.o")
                     (plist-get (nth 1 results) :path)))
      (should (equal "symbol_90" (plist-get (nth 1 results) :name)))

      (should (equal 2 (length results))))))

(ert-deftest bdx-search-occur ()
  (bdx-test--with-files
      `(("foo.c" "
int foo() { return 0; }
const char bar[10];
" :args ("-c")))
    (bdx-test--index)

    (with-temp-buffer
      (let (all-cands proc ivy--all-candidates)
        (setq proc
              (bdx--search-async "*:*" :callback
                                 (lambda (cands)
                                   (setq all-cands
                                         (append all-cands cands)))))
        (while (process-live-p proc)
          (accept-process-output nil 0.1))

        (setq ivy--all-candidates
              (mapcar (lambda (data)
                        (propertize (plist-get data :name) 'bdx-data data))
                      all-cands))

        (bdx--occur nil)

        (should (search-forward "Found 2 candidates in total"))

        (should (re-search-forward "^foo:$" nil t))
        (forward-line 1)
        (goto-char (line-beginning-position))
        (should (looking-at-p ".*name: foo"))
        (forward-line 1)
        (should (looking-at-p ".*path:.*foo.c.o"))

        (should (re-search-forward "^bar:$" nil t))
        (forward-line 1)
        (goto-char (line-beginning-position))
        (should (looking-at-p ".*name: bar"))
        (forward-line 1)
        (should (looking-at-p ".*path:.*foo.c.o"))
        (forward-line 1)
        (should (looking-at-p ".*section: .rodata"))
        (forward-line 1)
        (should (looking-at-p ".*address:"))
        (forward-line 1)
        (should (looking-at-p ".*size: 0xa (10)"))
        (forward-line 1)
        (should (looking-at-p ".*type: OBJECT"))))))

(ert-deftest bdx-disassembly ()
  (bdx-test--with-files
      `(("foo.c" "
int foo() { return 128; }
int bar(int x) { return x+1; }
" :args ("-c")))
    (bdx-test--index)
    (let ((bdx-disassembler-options "-M intel"))
      (bdx-disassemble (car (bdx--search "name:foo" :limit 1))))
    (with-current-buffer (bdx--disassembly-buffer)
      (goto-char (point-min))
      (should (re-search-forward "<foo>:"))
      (should (re-search-forward "mov.*eax,0x80"))
      (goto-char (point-min))
      (should-not (re-search-forward "bar" nil t)))
    (let ((bdx-disassembler-options "-M intel"))
      (bdx-disassemble (car (bdx--search "name:bar" :limit 1))))
    (with-current-buffer (bdx--disassembly-buffer)
      (goto-char (point-min))
      (should-not (re-search-forward "<foo>:" nil t))
      (should (re-search-forward "add.*eax,0x1")))))

(ert-deftest bdx-disassembly-history ()
  (bdx-test--with-files
      `(("foo.c" "
int foo() { return 128; }
int bar(int x) { return x+1; }
int baz(int x) { return x+1; }
int quux(int x) { return x+1; }
" :args ("-c")))
    (bdx-test--index)
    (bdx-disassemble (car (bdx--search "name:foo" :limit 1)))
    (bdx-disassemble (car (bdx--search "name:bar" :limit 1)))
    (bdx-disassemble (car (bdx--search "name:baz" :limit 1)))
    (bdx-disassemble (car (bdx--search "name:quux" :limit 1)))
    (with-current-buffer (bdx--disassembly-buffer)
      (goto-char (point-min))
      (should (re-search-forward "<quux>:"))

      (bdx-disassemble-previous)
      (goto-char (point-min))
      (should (re-search-forward "<baz>:"))

      (bdx-disassemble-previous)
      (goto-char (point-min))
      (should (re-search-forward "<bar>:"))

      (bdx-disassemble-previous)
      (goto-char (point-min))
      (should (re-search-forward "<foo>:"))

      (should-error (bdx-disassemble-previous)))))

(ert-deftest bdx-disassembly-forward-history ()
  (bdx-test--with-files
      `(("foo.c" "
int foo() { return 128; }
int bar(int x) { return x+1; }
int baz(int x) { return x+1; }
int quux(int x) { return x+1; }
" :args ("-c")))
    (bdx-test--index)
    (bdx-disassemble (car (bdx--search "name:foo" :limit 1)))
    (bdx-disassemble (car (bdx--search "name:bar" :limit 1)))
    (bdx-disassemble (car (bdx--search "name:baz" :limit 1)))
    (bdx-disassemble (car (bdx--search "name:quux" :limit 1)))
    (with-current-buffer (bdx--disassembly-buffer)
      (should-error (bdx-disassemble-next))

      (goto-char (point-min))
      (should (re-search-forward "<quux>:"))

      (bdx-disassemble-previous)
      (goto-char (point-min))
      (should (re-search-forward "<baz>:"))

      (bdx-disassemble-next)
      (goto-char (point-min))
      (should (re-search-forward "<quux>:"))
      (should-error (bdx-disassemble-next))

      (bdx-disassemble-previous)
      (bdx-disassemble-previous)
      (goto-char (point-min))
      (should (re-search-forward "<bar>:"))

      (bdx-disassemble-previous)
      (goto-char (point-min))
      (should (re-search-forward "<foo>:"))

      (bdx-disassemble-next)
      (bdx-disassemble-next)
      (goto-char (point-min))
      (should (re-search-forward "<baz>:"))

      (bdx-disassemble-next)
      (goto-char (point-min))
      (should (re-search-forward "<quux>:"))
      (should-error (bdx-disassemble-next)))))

(ert-deftest bdx-generate-graph ()
  (bdx-test--with-files
      '(("foo.c" "
extern int middle();
int start() { return middle(); }
" :args ("-c"))
        ("bar.c" "
extern int goal();
int middle() { return goal(); }
" :args ("-c"))
        ("baz.c" "
int goal() { return 0; }
" :args ("-c")))
    (bdx-test--index)

    (with-temp-buffer
      (bdx-generate-graph "path:foo*" "path:baz*" (current-buffer))

      (goto-char (point-min))

      (should (re-search-forward "strict graph" nil t))
      (should (re-search-forward "start -- middle" nil t))
      (should (re-search-forward "middle -- goal" nil t)))))

(ert-deftest bdx-generate-graph-image ()
  (bdx-test--with-files
      '(("foo.cpp" "
extern int middle();
int start() { return middle(); }
" :args ("-c"))
        ("bar.cpp" "
extern int goal();
int middle() { return goal(); }
" :args ("-c"))
        ("baz.cpp" "
int goal() { return 0; }
" :args ("-c")))
    (bdx-test--index)

    (let ((path (bdx-generate-graph-image "path:foo*" "path:baz*"
                                          :image-type "svg")))
      (should (file-exists-p path))
      (when (fboundp 'imagep)
        (should (imagep (ignore-errors (create-image path)))))
      (with-temp-buffer
        (insert-file-contents path)
        (should (re-search-forward "_Z6middlev" nil t))))))

(provide 'bdx-test)
;;; bdx-test.el ends here
