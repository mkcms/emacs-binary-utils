;;; binfile-test.el --- Tests for binfile.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: tests
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

;;; Code:

(require 'ert)
(require 'binfile)

(defmacro binfile-test (source to-file program options &rest body)
  (declare (indent 4) (debug (form form form form body)))
  (let ((out-filename (cl-gensym))
        (src-filename (cl-gensym))
        (opts (cl-gensym))
        (gcc-rv (cl-gensym))
        (prog (cl-gensym))
        (src (cl-gensym))
        (dir (cl-gensym)))
    `(with-temp-buffer
       (let* ((,src ,source)
              (,out-filename ,to-file)
              (,src-filename (concat (file-name-base ,out-filename) ".c"))
              (,prog ,program)
              (,opts ,options)
              (,gcc-rv nil)
              (,dir (make-temp-file "binfile-test" t))
              (default-directory ,dir)
              (objdump--symtab-cache (make-hash-table :test #'equal))
              (objdump--mangled-to-demangled-names
               (make-hash-table :test #'equal))
              (objdump--demangled-to-mangled-names
               (make-hash-table :test #'equal))
              (objdump--flags-cache (make-hash-table :test #'equal))
              (objdump-disassembly-extra-args nil)
              (objdump-disassembly-extra-args-alist nil)

              binfile--file
              binfile--symbol
              binfile--symbol-alist-cache

              binfile-disassembly-hook
              binfile-disassembly-prologue
              binfile-file-format
              binfile-mode-hook

              (binfile-relocation-handler-alist '(("^R_X86_64_.*" . asm-x86--reloc-64)))
              (binfile-data-relocation-handler-alist '(("^R_X86_64_.*" . asm-x86--data-reloc-64)))
              (binfile-arch-postprocessing-function-alist '(("elf64-x86-64" . asm-x86--postprocess))))
         (with-temp-file ,src-filename
           (insert ,src))
         (with-temp-buffer
           (setq ,opts (cl-list* ,src-filename "-o" ,out-filename ,opts))
           (message "Running %s with args %S in dir %s" ,prog ,opts default-directory)
           (setq ,gcc-rv (apply #'call-process ,prog nil (current-buffer) nil ,opts))
           (unless (zerop ,gcc-rv)
             (error "GCC failed with code %s, src %s, dst %s, dir %s, args %s:\n\n%s"
                    ,gcc-rv ,src-filename ,out-filename default-directory ,opts
                    (buffer-string))))
         ,@body
         (delete-directory ,dir t)))))

(ert-deftest binfile-disassembly ()
  (binfile-test
      "
int foo(int x) { return x + 1; }
"
      "file.o" "gcc" '("-c")
    (binfile-disassemble "foo" "file.o")
    (with-current-buffer binfile-disassembly-buffer
      (goto-char (point-min))
      (should (search-forward ".section .text"))
      (should (search-forward "foo:"))
      (should (search-forward "ret")))))

(ert-deftest binfile-disassembly-c++ ()
  (binfile-test
      "
int foo(int x) { return x + 1; }
"
      "file.o" "g++" '("-c")
    (binfile-disassemble "_Z3fooi" "file.o")
    (with-current-buffer binfile-disassembly-buffer
      (goto-char (point-min))
      (should (search-forward ".section .text"))
      (should (search-forward "foo(int):"))
      (should (search-forward "ret")))))

(ert-deftest binfile-disassembly-with-relocation ()
  (binfile-test
      "
extern int bar();
int foo() { return bar(); }
"
      "file.o" "g++" '("-c")
    (let ((objdump-disassembly-extra-args '("-M" "intel")))
      (binfile-disassemble "_Z3foov" "file.o"))
    (with-current-buffer binfile-disassembly-buffer
      (goto-char (point-min))
      (should (search-forward ".section .text"))
      (should (search-forward "foo():"))
      (should (re-search-forward "call[[:space:]]+bar()")))))

(ert-deftest binfile-insert-raw-data ()
  (binfile-test
      "
#include <stdint.h>

int foo(int x) { return 123; }

uint8_t bar[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
const uint16_t quux[] = {1, 2, 4, 8, 16, 32, 64, 128};

int main() { return foo(1); }

"
      "file.o" "gcc" '()
    (binfile-disassemble "foo" "file.o")
    (with-current-buffer binfile-disassembly-buffer
      (let* ((symtab (objdump-read-symtab "file.o"))
             (sym (car (map-elt symtab "bar")))
             (section (plist-get sym :section))
             (address (plist-get sym :address))
             (size (plist-get sym :size)))
        (binfile-insert-data section address (+ address size) nil "bar")
        (goto-char (point-min))
        (should (search-forward "bar:"))
        (should (search-forward ".byte 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8"))
        (should (search-forward ".byte 0x9"))

        (setq sym (car (map-elt symtab "quux"))
              section (plist-get sym :section)
              address (plist-get sym :address)
              size (plist-get sym :size))
        (binfile-insert-data section address (+ address size) nil "quux")
        (goto-char (point-min))
        (should (search-forward "quux:"))
        ;; assumes little-endian
        (should (search-forward ".byte 0x1, 0x0, 0x2, 0x0, 0x4, 0x0, 0x8, 0x0"))
        (should (search-forward ".byte 0x10, 0x0, 0x20, 0x0"))
        (should (search-forward ".byte 0x40"))
        (should (search-forward ".zero 1"))
        (should (search-forward ".byte 0x80"))
        (should (search-forward ".zero 1"))))))

(ert-deftest binfile-insert-raw-data-with-relocations ()
  (binfile-test
      "
#include <stdint.h>

int foo(int x) { return 123; }

uint8_t bar[] = {1, 2, 3, 4, 5, 6, 7, 8, 9};
const uint8_t * const quux = &bar[7];

int main() { return foo(1); }

"
      "file.o" "gcc" '("-c")
    (binfile-disassemble "foo" "file.o")
    (with-current-buffer binfile-disassembly-buffer
      (let* ((symtab (objdump-read-symtab "file.o"))
             (sym (car (map-elt symtab "bar")))
             (section (plist-get sym :section))
             (address (plist-get sym :address))
             (size (plist-get sym :size)))

        (setq sym (car (map-elt symtab "quux"))
              section (plist-get sym :section)
              address (plist-get sym :address)
              size (plist-get sym :size))
        (binfile-insert-data section address (+ address size) nil "quux")
        (goto-char (point-min))
        (should (search-forward "quux:"))
        (should (search-forward "bar+0x7"))))))

(provide 'binfile-test)
;;; binfile-test.el ends here
