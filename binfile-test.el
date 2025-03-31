;;; binfile-test.el --- Tests for binfile.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Michał Krzywkowski

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

              binfile-file-format

              (binfile-relocation-handler-alist
               '(("^R_X86_64_.*" . asm-x86--reloc-64)))
              (binfile-region-postprocessing-functions-alist
               '(("elf64-x86-64" . asm-x86--postprocess))))
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
         (prog1 (progn ,@body)
           (delete-directory ,dir t))))))

(defmacro binfile-test-with-disassembly (program options &rest body)
  (declare (indent 2) (debug (form form body)))
  (let ((opts (cl-gensym))
        (rv (cl-gensym))
        (prog (cl-gensym)))
    `(with-temp-buffer
       (let* ((,prog ,program)
              (,opts ,options)
              (,rv nil))
         (with-temp-buffer
           (message "Running %s with args %S in dir %s" ,prog ,opts default-directory)
           (setq ,rv (apply #'call-process ,prog nil (current-buffer) nil ,opts))
           (unless (zerop ,rv)
             (error "Disassembler failed with code %s, dir %s, args %s:\n\n%s"
                    ,rv default-directory ,opts
                    (buffer-string)))
           ,@body)))))

(ert-deftest binfile-disassembly ()
  (binfile-test
      "
int foo(int x) { return x + 1; }
"
      "file.o" "gcc" '("-c")
    (binfile-test-with-disassembly "objdump"
        '("--disassemble=foo" "-Mintel" "--no-show-raw-insn" "file.o")
      (binfile-postprocess-buffer)
      (goto-char (point-min))
      (should (search-forward "foo:"))
      (should (re-search-forward "^	push   rbp"))
      (should (re-search-forward "^	add    eax,0x1"))
      (should (search-forward "ret")))))

(ert-deftest binfile-disassembly-c++ ()
  (binfile-test
      "
int foo(int x) { return x + 1; }
"
      "file.o" "g++" '("-c")
    (binfile-test-with-disassembly "objdump"
        '("--disassemble=_Z3fooi" "-Mintel" "--no-show-raw-insn" "file.o")
      (binfile-postprocess-buffer)
      (goto-char (point-min))
      (should (search-forward "_Z3fooi:"))
      (should (re-search-forward "^	push   rbp"))
      (should (re-search-forward "^	add    eax,0x1"))
      (should (search-forward "ret")))))

(ert-deftest binfile-disassembly-with-relocation ()
  (binfile-test
      "
extern int bar();
int foo() { return bar(); }
"
      "file.o" "g++" '("-c")
    (binfile-test-with-disassembly "objdump"
        '("--disassemble=_Z3foov" "-r" "-Mintel" "--no-show-raw-insn" "file.o")
      (binfile-postprocess-buffer)
      (goto-char (point-min))
      (should (search-forward "_Z3foov:"))
      (should (re-search-forward "^	call[[:space:]]+_Z3barv")))))

(ert-deftest binfile-postprocess-relocations ()
  (with-temp-buffer
    (insert "0000000000000000 <f()>:
   0:	endbr64
   4:	sub    rsp,0x8
   8:	call   d <f()+0xd>
			9: R_X86_64_PLT32	g()-0x4
   d:	add    eax,DWORD PTR [rip+0x0]        # 13 <f()+0x13>
			f: R_X86_64_PC32	h-0x4
  13:	add    rsp,0x8
  17:	ret
")
    (goto-char (point-min))
    (binfile-postprocess-relocations (point-min) (point-max) "f()")

    (should (string= (buffer-string)
                     "0000000000000000 <f()>:
   0:	endbr64
   4:	sub    rsp,0x8
   8:	call   g() <f()+0xd>
   d:	add    eax,DWORD PTR [rip+0x0]        # h <f()+0x13>
  13:	add    rsp,0x8
  17:	ret
"))))

(ert-deftest binfile-postprocess-local-jumps ()
  (with-temp-buffer
    (insert "
0000000000000000 <func>:
   0:	endbr64
   4:	test   edi,edi
   6:	jle    20 <func+0x20>
   8:	test   esi,esi
   a:	jns    18 <func+0x18>
   c:	mov    eax,0x1
  11:	ret
  12:	nop    WORD PTR [rax+rax*1+0x0]
  18:	mov    eax,0x2
  1d:	ret
  1e:	xchg   ax,ax
  20:	test   esi,esi
  22:	js     30 <func+0x30>
  24:	mov    eax,0x4
  29:	ret
  2a:	nop    WORD PTR [rax+rax*1+0x0]
  30:	mov    eax,0x3
  35:	ret
")
    (goto-char (point-min))
    (binfile-postprocess-local-jumps (point-min) (point-max) "func")

    (should (string= (buffer-string)
                     "
0000000000000000 <func>:
   0:	endbr64
   4:	test   edi,edi
   6:	jle    .L1
   8:	test   esi,esi
   a:	jns    .L2
   c:	mov    eax,0x1
  11:	ret
  12:	nop    WORD PTR [rax+rax*1+0x0]
.L2:
  18:	mov    eax,0x2
  1d:	ret
  1e:	xchg   ax,ax
.L1:
  20:	test   esi,esi
  22:	js     .L3
  24:	mov    eax,0x4
  29:	ret
  2a:	nop    WORD PTR [rax+rax*1+0x0]
.L3:
  30:	mov    eax,0x3
  35:	ret
"
))))

(ert-deftest binfile-postprocess-strip-addresses ()
  (with-temp-buffer
    (insert "
0000000000000000 <func>:
   0:	endbr64
   4:	test   edi,edi
   6:	jle    20 <func+0x20>
   8:	test   esi,esi
   a:	jns    18 <func+0x18>
   c:	mov    eax,0x1
  11:	ret
  12:	nop    WORD PTR [rax+rax*1+0x0]
  18:	mov    eax,0x2
  1d:	ret
  1e:	xchg   ax,ax
  20:	test   esi,esi
  22:	js     30 <func+0x30>
  24:	mov    eax,0x4
  29:	ret
  2a:	nop    WORD PTR [rax+rax*1+0x0]
  30:	mov    eax,0x3
  35:	ret
")
    (goto-char (point-min))
    (binfile-postprocess-strip-addresses (point-min) (point-max) "func")

    (should (string= (buffer-string)
                     "
0000000000000000 <func>:
	endbr64
	test   edi,edi
	jle    20 <func+0x20>
	test   esi,esi
	jns    18 <func+0x18>
	mov    eax,0x1
	ret
	nop    WORD PTR [rax+rax*1+0x0]
	mov    eax,0x2
	ret
	xchg   ax,ax
	test   esi,esi
	js     30 <func+0x30>
	mov    eax,0x4
	ret
	nop    WORD PTR [rax+rax*1+0x0]
	mov    eax,0x3
	ret
"))))

(ert-deftest binfile-postprocess-numeric-to-symbolic-references ()
  (with-temp-buffer
    (insert "
0000000000000000 <func>:
    2550:	cmp    BYTE PTR [rcx+0x2],0x0
    2554:	jns    2560 <func+0x10>
    2556:	ret
    2557:	nop    WORD PTR [rax+rax*1+0x0]
    2560:	jmp    19a0 <func.part.0>
")
    (goto-char (point-min))
    (binfile-postprocess-numeric-to-symbolic-references (point-min) (point-max) "func")

    (should (string= (buffer-string)
                     "
0000000000000000 <func>:
    2550:	cmp    BYTE PTR [rcx+0x2],0x0
    2554:	jns    func+0x10
    2556:	ret
    2557:	nop    WORD PTR [rax+rax*1+0x0]
    2560:	jmp    func.part.0
"))))

(ert-deftest binfile-postprocess-unused-symbolic-local-references ()
  (with-temp-buffer
    (insert "
0000000000000000 <func>:
   0:	endbr64
   4:	test   edi,edi
   6:	jle    20 <func+0x20>
   8:	test   esi,esi
   a:	jns    18 <func+0x18>
   c:	mov    eax,0x1
  11:	ret
  12:	nop    WORD PTR [rax+rax*1+0x0]
  18:	mov    eax,0x2
  1d:	ret
  1e:	xchg   ax,ax
  20:	test   esi,esi
  22:	js     30 <func+0x30>
  24:	mov    eax,0x4
  29:	ret
  2a:	nop    WORD PTR [rax+rax*1+0x0]
  30:	mov    eax,0x3
  35:	ret
")
    (goto-char (point-min))
    (binfile-postprocess-unused-symbolic-local-references (point-min) (point-max) "func")
    (delete-trailing-whitespace)

    (should (string= (buffer-string)
                     "
0000000000000000 <func>:
   0:	endbr64
   4:	test   edi,edi
   6:	jle    20
   8:	test   esi,esi
   a:	jns    18
   c:	mov    eax,0x1
  11:	ret
  12:	nop    WORD PTR [rax+rax*1+0x0]
  18:	mov    eax,0x2
  1d:	ret
  1e:	xchg   ax,ax
  20:	test   esi,esi
  22:	js     30
  24:	mov    eax,0x4
  29:	ret
  2a:	nop    WORD PTR [rax+rax*1+0x0]
  30:	mov    eax,0x3
  35:	ret
"))))

(provide 'binfile-test)
;;; binfile-test.el ends here
