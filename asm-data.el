;;; asm-data.el --- Convert data representations in ASM buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: languages, tools

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
;; This package lets you change the way data is represented in ASM buffers.
;; For example, when the buffer contains:
;;
;; 	.4byte 1819043176
;; 	.byte 111
;; 	.byte 0
;;
;; calling `asm-data-convert' and selecting the ".asciz" directive changes that
;; to:
;;
;; 	.asciz "hello"
;;
;; This representation can again be changed, e.g. to 2byte (here called with a
;; prefix argument for hexadecimal number representation):
;;
;; 	.2byte 0x6568
;; 	.2byte 0x6c6c
;; 	.2byte 0x006f
;;
;; It can convert data to .ascii, .asciz, .byte, .2byte, .4byte, .8byte, .octa
;; (16-byte), .single and .zero GAS directives.
;;
;; Integers can be converted to unsigned/signed decimal/hex/binary
;; representation.
;;
;; Floating-point directive .single assumes 32-bit floats.
;;
;; The variable `asm-data-endianness' controls the type of numbers<->bytes
;; conversion.
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup asm-data nil "Convert data representations in ASM buffers"
  :group 'languages
  :group 'tools)

(defconst asm-data--big-values-support
  (and (>= emacs-major-version 27)
       (fboundp 'bignump)
       (bignump (lsh 1 63)))
  "Non-nil if values larger than 4 bytes are supported.")

(defvar asm-data--directives
  (let ((val'(
              ".ascii"
              ".asciz"
              ".byte"
              ".2byte"
              ".4byte"
              ".8byte"
              ".octa"                   ; 16-byte integer

              ".single"                 ; 32 bit float

              ".zero"

              )))
    (unless asm-data--big-values-support
      (setq val (delete ".8byte" val))
      (setq val (delete ".octa" val)))
    val)
  "List of asm data directives.")

(defcustom asm-data-endianness 'little
  "Data endianness."
  :type '(choice (const little) (const big)))

(defun asm-data--parse-value (directive string)
  "Parse STRING as data encoded with DIRECTIVE and return vector of bytes."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (set-buffer-multibyte nil)
    (cl-loop if (looking-at "[[:space:]]+")
             do (goto-char (match-end 0))
             until (eobp)
             if (looking-at " *, *")
             do (progn
                  (unless values
                    (error "Expected value before comma"))
                  (goto-char (match-end 0))
                  (when (eobp)
                    (error "Missing value after comma")))
             else if values do (error "Values must be separated by comma")
             for v = (asm-data--read-value-from-current-buffer directive)
             if v collect (asm-data--value-to-bytes directive v) into values
             finally do (unless values
                          (error "No value for %S directive" directive))
             finally return (cl-reduce #'vconcat values))))

(defun asm-data--read-value-from-current-buffer (directive)
  "Like `read', but treats numbers specially if DIRECTIVE is a numeric directive.

This parses and returns a Lisp object for the value after point,
like `read', but:

  - reads hexadecimal, binary (0b...) numbers
  - reads signed numbers and makes them unsigned"
  (when (looking-at "[[:space:]]+")
    (goto-char (match-end 0)))
  (cond
   ((looking-at "0x\\([[:xdigit:]]+\\)")
    (prog1 (let* ((str (match-string 1))
                  (val (string-to-number str 16)))
             (cond
              ((integerp val) val)
              (t (error "Invalid hex value %s" str))))
      (goto-char (match-end 0))))
   ((looking-at "0b\\([01]+\\)")
    (prog1 (let* ((str (match-string 1))
                  (val (string-to-number str 2)))
             (cond
              ((integerp val) val)
              (t (error "Invalid binary value %s" str))))
      (goto-char (match-end 0))))
   ((eq 'nan (save-excursion (read (current-buffer))))
    (prog1 0.0e+NaN (read (current-buffer))))
   ((eq '-nan (save-excursion (read (current-buffer))))
    (prog1 -0.0e+NaN (read (current-buffer))))
   ((eq 'inf (save-excursion (read (current-buffer))))
    (prog1 1.0e+INF (read (current-buffer))))
   ((eq '-inf (save-excursion (read (current-buffer))))
    (prog1 -1.0e+INF (read (current-buffer))))
   ((member directive '(".single"))
    (float (read (current-buffer))))
   ((and (member directive '(".byte" ".2byte" ".4byte" ".8byte"))
         (looking-at-p "-"))
    (forward-char 1)
    (pcase-let* ((value-string (save-excursion (read (current-buffer))))
                 (value (asm-data--read-value-from-current-buffer directive))
                 (`(,minimum ,maximum ,mask)
                 (pcase-exhaustive directive
                   ('".byte" (list -128 127 255))
                   ('".2byte" (list -32768 32767 65535))
                   ('".4byte" (list -2147483648 2147483647 4294967295))
                   ('".8byte" (list (- (lsh 1 63))
                                    (1- (lsh 1 63))
                                    (1- (lsh 1 64))))))
                 (minusvalue (- value)))
      (if (<= minimum minusvalue maximum)
          (logand minusvalue mask)
        (error "Value -%s is out of bounds for directive %s" value-string directive))))
   (t (read (current-buffer)))))

(defun asm-data--value-to-bytes (directive value)
  "Convert VALUE (a Lisp object parsed from data DIRECTIVE) to bytes."
  (cond
   ((and (string= directive ".ascii") (stringp value))
    (string-to-vector value))
   ((and (string= directive ".asciz") (stringp value))
    (vconcat (string-to-vector value) "\0"))
   ((and (string= directive ".byte") (integerp value)
         (>= value 0) (<= value 255))
    (vector value))
   ((and (string= directive ".2byte") (integerp value)
         (>= value 0) (<= value 65535))
    (asm-data--integer-to-vector value 2))
   ((and (string= directive ".4byte") (integerp value) (>= value 0)
         (<= value 4294967295))
    (asm-data--integer-to-vector value 4))
   ((and (string= directive ".8byte")
         ;; We need bignum support for 8byte values.
         asm-data--big-values-support
         (integerp value)
         (>= value 0)
         (<= value (1- (lsh 1 64))))
    (asm-data--integer-to-vector value 8))
   ((and (string= directive ".octa")
         asm-data--big-values-support
         (integerp value)
         (>= value 0)
         (<= value (1- (lsh 1 128))))
    (asm-data--integer-to-vector value 16))
   ((and (string= directive ".single") (floatp value))
    (asm-data--float-to-vector value 8 23))
   ((and (string= directive ".zero") (integerp value) (>= value 0))
    (make-vector value 0))
   (t (error "Invalid value for directive %S: %S" directive value))))

(defun asm-data--integer-to-vector (number nbytes)
  "Convert NUMBER to byte vector of length NBYTES."
  (let ((orig number))
    (prog1
        (let ((bytes
               (cl-loop for i from 1 to nbytes
                        vconcat (vector (logand number 255))
                        do (setq number (lsh number -8)))))
          (pcase-exhaustive asm-data-endianness
            ('little bytes)
            ('big (nreverse bytes))))
      (unless (zerop number)
        (error "Number %S is too large to be represented in %d bytes"
               orig nbytes)))))

(defun asm-data--vector-to-integer (vector)
  "Convert VECTOR of bytes to a number."
  (when (and (> (length vector) 4) (not asm-data--big-values-support))
    (error "Vector %S is not representable as a fixnum" vector))
  (setq vector (pcase-exhaustive asm-data-endianness
                 ('little vector)
                 ('big (reverse vector))))
  (cl-loop for byte across (reverse vector)
           for i from 0
           with ret = 0
           do (setq ret (logior (ash ret 8) byte))
           finally return ret))

(defun asm-data--float-to-vector (number exponent-bits significand-bits)
  (let* ((signbit (= (copysign 1.0 number) -1.0))
         (max-exponent (1- (expt 2 (1- exponent-bits))))
         (subnormal-p (<= (cdr (frexp (abs number))) (- (1- max-exponent)))))
    (with-temp-buffer
      (insert "0b")

      (insert (if signbit "1" "0"))

      (cond
       ((or (= +1.0e+INF number) (= -1.0e+INF number))
        (insert (make-string exponent-bits ?1))
        (insert (make-string significand-bits ?0)))
       ((isnan number)
        (insert (make-string exponent-bits ?1))
        (insert "1")
        (insert (make-string (1- significand-bits) ?0)))
       ((= number 0.0)
        (insert (make-string (+ exponent-bits significand-bits) ?0)))
       (subnormal-p
        (pcase-let ((`(,s . ,e) (frexp (* (abs number)
                                          (expt 2 significand-bits))))
                    (min-exponent (- (- max-exponent 2))))
          (insert (make-string exponent-bits ?0))

          (dotimes (_ significand-bits)
            (if (zerop s)
                (if (< e min-exponent)
                    (progn
                      (goto-char (+ 2 (point-min)))
                      (insert "0"))
                  (goto-char (point-max))
                  (insert "0")
                  (setq e (1- e)))
              (insert (if (>= s 0.5) "1" "0"))
              (setq s (mod (* s 2) 1.0))
              (setq e (1- e))))))
       (t
        (pcase-let* ((`(,s . ,e) (frexp number))
                     (`(,s . ,e) (cons (* (abs s) 2) (1- e)))
                     (exponent (+ e max-exponent)))

          (dotimes (_ exponent-bits)
            (insert (char-to-string (aref "01" (% exponent 2))))
            (forward-char -1)
            (setq exponent (lsh exponent -1)))
          (goto-char (point-max))

          (let ((frac (mod s 1.0)))
            (dotimes (_ significand-bits)
              (if (>= frac 0.5) (insert "1") (insert "0"))
              (setq frac (mod (* frac 2) 1.0)))))))

      (goto-char (point-min))

      (pcase-let ((`(,nbytes . ,directive)
                   (pcase-exhaustive (+ 1 exponent-bits significand-bits)
                     ('32 '(4 . ".4byte")))))
        (asm-data--integer-to-vector
         (asm-data--read-value-from-current-buffer directive) nbytes)))))

(defun asm-data--vector-to-float (vector exponent-bits significand-bits)
  (let* ((string (substring (asm-data--vector-to-numeric-string vector nil 2) 2))
         (signbit (expt -1.0 (- (aref string 0) ?0)))
         (exbits (substring string 1 (+ 1 exponent-bits)))
         (max-exponent (1- (expt 2 (1- exponent-bits))))
         (sbits (substring string (+ 1 exponent-bits)
                           (+ 1 exponent-bits significand-bits)))

         (exponent (string-to-number exbits 2))
         (significand (cl-loop for bit across sbits
                               with v = 0.5
                               sum (* v (- bit ?0))
                               do (setq v (/ v 2)))))
    (cond
     ((and (null (cl-position ?0 exbits))
           (or (null (cl-position ?0 sbits))
               (and (eq 0 (cl-position ?1 sbits))
                    (= 1 (cl-count ?1 sbits)))))
      (copysign +1.0e+NaN signbit))
     ((and (null (cl-position ?0 exbits))
           (null (cl-position ?1 sbits)))
      (copysign +1.0e+INF signbit))
     ((null (cl-position ?1 (substring string 1)))
      (copysign 0.0 signbit))
     ((null (cl-position ?1 exbits))
      ;; Subnormal number
      (copysign
       (ldexp significand (- (1- max-exponent)))
       signbit))
     (t
      (copysign
       (ldexp (1+ significand) (- exponent max-exponent))
       signbit)))))

(defun asm-data--byte-to-hex (byte)
  "Make a 2-char hexadecimal string for number BYTE."
  (let ((val (format "%x" byte)))
    (if (cl-evenp (length val)) val
      (concat "0" val))))

(defun asm-data--byte-to-binary (byte)
  "Make an 8-char binary string for number BYTE."
  (cl-loop for i upto 7
           collect (% byte 2) into bits
           do (setq byte (lsh byte -1))
           finally return (mapconcat
                           (lambda (b) (char-to-string (aref "01" b)))
                           (nreverse bits) "")))

(defun asm-data--vector-to-numeric-string (vector &optional signed base)
  "Convert VECTOR of bytes to a string containing a number.
If SIGNED, returns a number that may be nagative.
If BASE is 16, returns a hexadecimal number.
If BASE is 2, returns a binary number."
  (cond
   ((and (eq base 16) (eq 'little asm-data-endianness))
    (concat "0x" (mapconcat #'asm-data--byte-to-hex (reverse vector) "")))
   ((and (eq base 16) (eq 'big asm-data-endianness))
    (concat "0x" (mapconcat #'asm-data--byte-to-hex vector "")))
   ((and (eq base 2) (eq 'little asm-data-endianness))
    (concat "0b" (mapconcat #'asm-data--byte-to-binary (reverse vector) "")))
   ((and (eq base 2) (eq 'big asm-data-endianness))
    (concat "0b" (mapconcat #'asm-data--byte-to-binary vector "")))
   (signed
    (let* ((signbit (lsh 1 (1- (* 8 (length vector)))))
           (exp (expt 2 (* 8 (length vector))))
           (number (asm-data--vector-to-integer vector))
           (is-positive (zerop (logand number signbit))))
      (number-to-string
       (if is-positive number (* (- exp number) -1)))))
   (t (number-to-string (asm-data--vector-to-integer vector)))))

(defcustom asm-data-max-bytes-per-line 49
  "Maximum number of bytes per single line."
  :type 'integer)

(defun asm-data--do-conversion (directive bytes &optional signed base)
  "Convert vector of BYTES into another representation using DIRECTIVE.
DIRECTIVE must be a string from `asm-data--directives'.  The
return value is a list of cons cells (DIRECTIVE . STRING), where
DIRECTIVE is the actually used directive and STRING is a
human-readable string created from bytes.

If SIGNED is non-nil, then numbers are output as signed integers.

If BASE is non-nil, then numbers are output in that base."
  (cl-labels ((take
               (n)
               (let ((first (seq-subseq bytes 0 n)))
                 (setq bytes (seq-subseq bytes n))
                 first)))
    (let (ret)
      (while (> (length bytes) 0)
        (cond
         ((or (string= directive ".ascii") (string= directive ".asciz"))
          (with-temp-buffer
            (let* ((print-escape-nonascii t)
                   (print-escape-control-characters t)
                   (print-escape-multibyte nil)
                   (directive directive)
                   (values (take (min asm-data-max-bytes-per-line
                                      (or (and (string= directive ".asciz")
                                               (1+ (or (cl-position 0 bytes)
                                                       (1- (length bytes)))))
                                          (length bytes))))))
              (when (string= directive ".asciz")
                (if (and (> (length values) 0)
                         (= 0 (car (last (append values nil)))))
                    (setq values (butlast (append values nil)))
                  (setq directive ".ascii")))

              (set-buffer-multibyte nil)
              (push (cons directive
                          (progn (prin1 (mapconcat #'char-to-string values "")
                                        (current-buffer))
                                 (buffer-string)))
                    ret))))

         ((or (and (string= directive ".byte") (> (length bytes) 1))
              (= (length bytes) 1))
          (push (cons ".byte"
                      (asm-data--vector-to-numeric-string
                       (take 1) signed base))
                ret))

         ((and (string= directive ".2byte") (>= (length bytes) 2))
          (push
           (cons ".2byte"
                 (asm-data--vector-to-numeric-string (take 2) signed base))
           ret))

         ((and (string= directive ".4byte") (>= (length bytes) 4))
          (push
           (cons ".4byte"
                 (asm-data--vector-to-numeric-string (take 4) signed base))
           ret))

         ((and (string= directive ".8byte") (>= (length bytes) 8)
               asm-data--big-values-support)
          (push
           (cons ".8byte"
                 (asm-data--vector-to-numeric-string (take 8) signed base))
           ret))

         ((and (string= directive ".octa") (>= (length bytes) 16)
               asm-data--big-values-support)
          (push
           (cons ".octa"
                 (asm-data--vector-to-numeric-string (take 16) signed base))
           ret))

         ((and (string= directive ".single") (>= (length bytes) 4))
          (push
           (cons ".single"
                 (replace-regexp-in-string
                  "0\\(0+$\\)" "0"
                  (format "%.17g" (asm-data--vector-to-float (take 4) 8 23))))
           ret))

         ((and (string= directive ".zero") (zerop (aref bytes 0)))
          (let* ((n (or (cl-position-if-not #'zerop bytes)
                        (length bytes)))
                 (values (take n)))
            (push (cons ".zero" (number-to-string (length values))) ret)))

         (t
          (push (cons ".byte"
                      (asm-data--vector-to-numeric-string (take 1) signed base))
                ret))))
      (nreverse ret))))

(defun asm-data--goto-beginning ()
  "Move point to beginning of asm data block at point, or return nil."
  (save-match-data
    (let ((ops (mapcar #'regexp-quote asm-data--directives))
          (pt nil))
      (catch 'done
        (while (cl-loop for op in ops
                        do (back-to-indentation)
                        if (looking-at-p op)
                        return op)
          (goto-char (point-at-bol))
          (setq pt (point))
          (when (bobp) (throw 'done t))
          (forward-line -1)))
      (when pt
        (goto-char pt)
        (goto-char (point-at-bol))))))

(defun asm-data--goto-end ()
  "Move point to end of asm data block at point, or return nil."
  (when (asm-data--goto-beginning)
    (let ((ops (mapcar #'regexp-quote asm-data--directives))
          (pt nil))
      (catch 'done
        (while (cl-loop for op in ops
                        do (back-to-indentation)
                        if (looking-at-p op)
                        return op)
          (goto-char (point-at-eol))
          (setq pt (point))
          (when (eobp) (throw 'done t))
          (forward-line 1)
          (goto-char (point-at-bol))))
      (when pt
        (goto-char pt)
        (goto-char (point-at-eol))))))

(defun asm-data--at-point ()
  "Return asm data block at point, or nil."
  (save-excursion
    (when (asm-data--goto-beginning)
      (buffer-substring (point) (asm-data--goto-end)))))

(put 'asm-data 'beginning-op #'asm-data--goto-beginning)
(put 'asm-data 'end-op #'asm-data--goto-end)
(put 'asm-data 'thing-at-point #'asm-data--at-point)

(defun asm-data--to-bytes (data)
  "Convert an asm data string DATA to byte vector."
  (with-temp-buffer
    (insert data)
    (goto-char (point-min))
    (let (res)
      (while (not (eobp))
        (cl-loop with found = nil
                 do (back-to-indentation)
                 for directive in asm-data--directives
                 when (looking-at (concat (regexp-quote directive) "\\s-*"))
                 do (progn
                      (push
                       (asm-data--parse-value
                        directive (buffer-substring-no-properties
                                   (match-end 0)
                                   (point-at-eol)))
                       res)
                      (setq found t)
                      (cl-return))
                 finally do (unless found
                              (error "Invalid ASM data: %S" data)))
        (forward-line 1))
      (cl-reduce #'vconcat (nreverse res)))))

(defun asm-data-convert (beg end directive &optional signed base)
  "Replace region of asm data BEG END with the same data converted to DIRECTIVE.

If SIGNED is non-nil, printed numbers may be negative.
If BASE is either 2 or 16, output numbers in that base.

Interactively, with a negative numeric prefix argument, printed
numbers may be negative.  With a numeric prefix argument BASE,
the numbers are printed in that base.  Only BASE 2 and 16 are allowed."
  (interactive
   (if-let ((reg (or
                  (if (region-active-p) (car (region-bounds))
                    (bounds-of-thing-at-point 'asm-data)))))
       (list (car reg) (cdr reg)
             (completing-read "Convert to representation: "
                              asm-data--directives)
             (and (numberp current-prefix-arg) (cl-minusp current-prefix-arg))
             (and current-prefix-arg
                  (if (listp current-prefix-arg)
                      16
                    current-prefix-arg)))
     (user-error "There is no asm-data at point")))
  (save-restriction
    (widen)
    (let* ((bytes (asm-data--to-bytes (buffer-substring beg end)))
           (converted (asm-data--do-conversion directive bytes signed base))
           (buffer-read-only nil))
      (narrow-to-region beg end)
      (delete-region (point-min) (point-max))
      (pcase-dolist (`(,directive . ,string) converted)
        (insert "\t")
        (insert directive)
        (insert " ")
        (insert string)
        (insert "\n"))

      (while (eq (char-before) ?\n)
        (delete-char -1))

      (goto-char (point-min))
      (widen)
      (back-to-indentation)
      (while (eq (char-before) ?\t) (delete-char -1))
      (insert "\t"))))

(defun asm-data-calculate-offset (beg end point &optional interactive)
  "Get the offset in bytes of POINT to start of asm-data in region BEG END.
When INTERACTIVE, print how many bytes are before this line."
  (interactive
   (if-let ((bounds (bounds-of-thing-at-point 'asm-data)))
       (list (car bounds) (cdr bounds) (point-at-bol) t)
     (user-error "There is no asm-data at point")))
  (unless (<= beg point end)
    (error "POINT should be between BEG and END."))
  (save-restriction
    (widen)
    (let* ((bytes (asm-data--to-bytes (buffer-substring beg point)))
           (len (length bytes)))
      (when interactive
        (message "This line is at offset %d (0x%x)" len len))
      len)))

(provide 'asm-data)
;;; asm-data.el ends here
