;;; objdump-test.el --- Tests for objdump.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Michał Krzywkowski

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
(require 'objdump)

(defmacro objdump-test (source to-file program options &rest body)
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
              (,dir (make-temp-file "objdump-test" t))
              (default-directory ,dir)
              (objdump--symtab-cache (make-hash-table :test #'equal))
              (objdump--mangled-to-demangled-names
               (make-hash-table :test #'equal))
              (objdump--demangled-to-mangled-names
               (make-hash-table :test #'equal))
              (objdump--flags-cache (make-hash-table :test #'equal))
              (objdump-disassembly-extra-args nil))
         (with-temp-file ,src-filename
           (insert ,src))
         (with-temp-buffer
           (setq ,opts (cl-list* ,src-filename "-o" ,out-filename ,opts))
           (message "Running %s with args %S in dir %s" ,prog ,opts default-directory)
           (setq gcc-rv (apply #'call-process ,prog nil (current-buffer) nil ,opts))
           (unless (zerop gcc-rv)
             (error "GCC failed with code %s, src %s, dst %s, dir %s, args %s:\n\n%s"
                    gcc-rv ,src-filename ,out-filename default-directory ,opts
                    (buffer-string))))
         ,@body
         (delete-directory ,dir t)))))

(ert-deftest objdump-reading-symtab ()
  (objdump-test
      "
int foo(int x) { return x + 1; }

char xxx[129] = {};
"
      "file.o" "gcc" '("-c")
    (let ((symtab (objdump-read-symtab "file.o")))
      (should (gethash "foo" symtab))
      (should (integerp (plist-get (car (gethash "foo" symtab)) :address)))
      (should (integerp (plist-get (car (gethash "foo" symtab)) :size)))
      (should (equal "foo"
                     (plist-get (car (gethash "foo" symtab)) :demangled)))
      (should (equal ".text"
                     (plist-get (car (gethash "foo" symtab)) :section)))
      (should (member 'function
                      (plist-get (car (gethash "foo" symtab)) :flags)))
      (should (member 'global
                      (plist-get (car (gethash "foo" symtab)) :flags)))

      (should (gethash "xxx" symtab))
      (should (integerp (plist-get (car (gethash "xxx" symtab)) :address)))
      (should (equal 129 (plist-get (car (gethash "xxx" symtab)) :size)))
      (should (equal "xxx"
                     (plist-get (car (gethash "xxx" symtab)) :demangled)))
      (should (member 'object
                      (plist-get (car (gethash "xxx" symtab)) :flags))))))

(ert-deftest objdump-reading-c++-symtab ()
  (objdump-test
      "
#include <unordered_map>
int foo(int x) { return x + 1; }

namespace N {
std::unordered_map<int, char> map = {};
}

"
      "file.o"
      "g++"
      '("-c")
    (let ((symtab (objdump-read-symtab "file.o")))
      (should (gethash "_Z3fooi" symtab))
      (should (equal "foo(int)"
                     (plist-get (car (gethash "_Z3fooi" symtab)) :demangled)))

      (should (gethash "_ZN1N3mapE" symtab))
      (should (equal "N::map"
                     (plist-get (car (gethash "_ZN1N3mapE" symtab))
                                :demangled)))
      (should (member 'object
                      (plist-get (car (gethash "_ZN1N3mapE" symtab))
                                 :flags))))))

(ert-deftest objdump-reading-dynamic-symtab ()
  (objdump-test
      "
int foo(int x) { return x + 1; }
"
      "libtest.so" "gcc" '("-shared" "-fPIC" "-s")
    (let ((symtab (objdump-read-symtab "libtest.so")))
      (should (gethash "foo" symtab))

      (should (member 'dynamic
                      (plist-get (car (gethash "foo" symtab)) :flags))))))

(ert-deftest objdump-reading-raw-contents ()
  (objdump-test
      "
char test[8] = {1,2,3,4,5,6,7,8};
char test2[8] = {9,10,11,12,13,14,15,16};

char large[64] = {
    128, 129, 130, 131, 132, 133, 134, 135,
    136, 137, 138, 139, 140, 141, 142, 143,
    144, 145, 146, 147, 148, 149, 150, 151,
    152, 153, 154, 155, 156, 157, 158, 159,
    160, 161, 162, 163, 164, 165, 166, 167,
    168, 169, 170, 171, 172, 173, 174, 175,
    176, 177, 178, 179, 180, 181, 182, 183,
    184, 185, 186, 187, 188, 189, 190, 191,
};

"
      "file.o" "gcc" '("-c")
    (let ((symtab (objdump-read-symtab "file.o"))
          addr section)

      (setq addr (plist-get (car (gethash "test" symtab)) :address))
      (setq section (plist-get (car (gethash "test" symtab)) :section))
      (should (equal [1 2 3 4 5 6 7 8]
                     (car (objdump-raw "file.o" section addr (+ addr 8)))))

      (setq addr (plist-get (car (gethash "test2" symtab)) :address))
      (setq section (plist-get (car (gethash "test2" symtab)) :section))
      (should (equal [9 10 11 12 13 14 15 16]
                     (car (objdump-raw "file.o" section addr (+ addr 8)))))

      (setq addr (plist-get (car (gethash "large" symtab)) :address))
      (setq section (plist-get (car (gethash "large" symtab)) :section))
      (should (equal [128 129 130 131 132 133 134 135
                          136 137 138 139 140 141 142 143
                          144 145 146 147 148 149 150 151
                          152 153 154 155 156 157 158 159
                          160 161 162 163 164 165 166 167
                          168 169 170 171 172 173 174 175
                          176 177 178 179 180 181 182 183
                          184 185 186 187 188 189 190 191]
                     (car (objdump-raw "file.o" section addr (+ addr 64))))))))

(ert-deftest objdump-reading-raw-contents-with-relocations ()
  (objdump-test
      "
extern int external_value;
extern int other_value;

int func() { return external_value + other_value; }
"
      "file.o" "gcc" '("-c")
    (let ((symtab (objdump-read-symtab "file.o"))
          addr size section relocs)

      (setq addr (plist-get (car (gethash "func" symtab)) :address))
      (setq size (plist-get (car (gethash "func" symtab)) :size))
      (setq section (plist-get (car (gethash "func" symtab)) :section))
      (setq relocs (cadr (objdump-raw "file.o" section addr (+ addr size))))

      (should (equal 2 (length relocs)))

      (should (equal "external_value" (nth 2 (car relocs))))
      (should (equal "other_value" (nth 2 (cadr relocs)))))))

(ert-deftest objdump-mangle-demangle ()
  (objdump-test
      "
namespace N {
int test;
}
"
      "file.o" "g++" '("-c")
    (objdump-read-symtab "file.o")

    (should (equal (objdump-demangle "_ZN1N4testE") "N::test"))
    (should (equal (objdump-mangle "N::test") "_ZN1N4testE"))))

(ert-deftest objdump-disassemble ()
  (objdump-test
      "
int foo(int x) { return x + 1; }
"
      "foo.o" "gcc" '("-c")
    (let* ((symtab (objdump-read-symtab "foo.o"))
           (addr (plist-get (car (gethash "foo" symtab)) :address))
           (size (plist-get (car (gethash "foo" symtab)) :size)))
      (with-temp-buffer
        (objdump-disassemble "foo.o" ".text" addr (+ addr size))
        (should (search-forward "Disassembly of section .text:"))
        (should (re-search-forward "^[[:xdigit:]]+ <foo>:$"))))))

(ert-deftest objdump-disassemble-c++ ()
  (objdump-test
      "
int func(int x) { return x - 1; }
"
      "foo.o" "g++" '("-c")
    (let* ((symtab (objdump-read-symtab "foo.o"))
           (addr (plist-get (car (gethash "_Z4funci" symtab)) :address))
           (size (plist-get (car (gethash "_Z4funci" symtab)) :size)))
      (with-temp-buffer
        (objdump-disassemble "foo.o" ".text" addr (+ addr size))
        (should (search-forward "Disassembly of section .text:"))
        (should (re-search-forward "^[[:xdigit:]]+ <_Z4funci>:$"))

        (erase-buffer)
        (objdump-disassemble "foo.o" ".text" addr (+ addr size)
                             :demangle t)
        (should (search-forward "Disassembly of section .text:"))
        (should (re-search-forward "^[[:xdigit:]]+ <func(int)>:$"))))))

(ert-deftest objdump-disassemble-relocations ()
  (objdump-test
      "
extern int external_value;
int foo(int x) { return external_value + 1; }
"
      "foo.o" "gcc" '("-c")
    (let* ((symtab (objdump-read-symtab "foo.o"))
           (addr (plist-get (car (gethash "foo" symtab)) :address))
           (size (plist-get (car (gethash "foo" symtab)) :size)))
      (with-temp-buffer
        (objdump-disassemble "foo.o" ".text" addr (+ addr size)
                             :reloc t)
        (should (re-search-forward "^[[:xdigit:]]+ <foo>:$"))
        (should (search-forward "external_value"))))))

(ert-deftest objdump-disassemble-dynamic-relocations ()
  (objdump-test
      "
extern int external_func(int x);
int foo(int x) { return external_func(x) - 128; }
"
      "libfoo.so" "gcc" '("-m32" "-shared" "-fno-pic" "-s")
    (let* ((symtab (objdump-read-symtab "libfoo.so"))
           (section (plist-get (car (gethash "foo" symtab)) :section))
           (addr (plist-get (car (gethash "foo" symtab)) :address))
           (size (plist-get (car (gethash "foo" symtab)) :size)))
      (with-temp-buffer
        (objdump-disassemble "libfoo.so" section addr (+ addr size)
                             :reloc t)
        (should (re-search-forward "^[[:xdigit:]]+ <foo.*>:$"))
        (should (search-forward "external_func"))))))

(provide 'objdump-test)
;;; objdump-test.el ends here
