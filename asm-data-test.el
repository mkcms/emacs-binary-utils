;;; asm-data-test.el --- Tests for asm-data.el     -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Michał Krzywkowski

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
(require 'asm-data)

(defun asm-data-test--binary-str-to-bytes (str nbytes)
  (asm-data--integer-to-vector (string-to-number str 2) nbytes))

(ert-deftest asm-data-conversion-to-bytes ()
  (should (equal (asm-data--to-bytes ".zero 0   ") []))
  (should (equal (asm-data--to-bytes ".zero 4") [0 0 0 0]))

  (should (equal (asm-data--to-bytes ".byte 125") [125]))
  (should (equal (asm-data--to-bytes ".byte 128, 255, -2") [128 255 254]))
  (should (equal (asm-data--to-bytes ".byte 0x0") [0]))
  (should (equal (asm-data--to-bytes ".byte 0xff") [255]))
  (should (equal (asm-data--to-bytes ".byte 0xf") [15]))
  (should (equal (asm-data--to-bytes ".byte 0b1") [1]))
  (should (equal (asm-data--to-bytes ".byte 0b10") [2]))
  (should (equal (asm-data--to-bytes ".byte 0b101") [5]))

  (should-error (asm-data--to-bytes ".byte 256"))
  (should-error (asm-data--to-bytes ".byte -129"))
  (should-error (asm-data--to-bytes ".byte -+129"))

  (should-error (asm-data--to-bytes ".byte"))
  (should-error (asm-data--to-bytes ".byte ,"))
  (should-error (asm-data--to-bytes ".byte 1,"))
  (should-error (asm-data--to-bytes ".byte 1 1"))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--to-bytes ".2byte 12345") [57 48]))
    (should (equal (asm-data--to-bytes ".short 12345") [57 48]))
    (should (equal (asm-data--to-bytes ".2byte 65535") [255 255]))
    (should (equal (asm-data--to-bytes ".hword 65535") [255 255]))
    (should (equal (asm-data--to-bytes ".2byte -1") [255 255]))
    (should (equal (asm-data--to-bytes ".2byte -2") [254 255]))
    (should (equal (asm-data--to-bytes ".2byte 0xff") [255 0]))
    (should (equal (asm-data--to-bytes ".2byte 0xff00") [0 255]))
    (should (equal (asm-data--to-bytes ".2byte 0b1001") [9 0]))
    (should (equal (asm-data--to-bytes ".2byte 0b1111111100000000") [0 255])))
  (let ((asm-data-endianness 'big))
    (should (equal (asm-data--to-bytes ".2byte 12345") [48 57]))
    (should (equal (asm-data--to-bytes ".2byte 65535") [255 255]))
    (should (equal (asm-data--to-bytes ".2byte -2") [255 254]))
    (should (equal (asm-data--to-bytes ".2byte 0xff00") [255 0])))

  (should-error (asm-data--to-bytes ".2byte 65536"))
  (should-error (asm-data--to-bytes ".2byte -32769"))
  (should-error (asm-data--to-bytes ".2byte -0xffff"))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--to-bytes ".4byte 1234567") [135 214 18 0]))
    (should (equal (asm-data--to-bytes ".int 1234567") [135 214 18 0]))
    (should (equal (asm-data--to-bytes ".long 1234567") [135 214 18 0]))
    (should (equal (asm-data--to-bytes ".4byte 0xff") [255 0 0 0]))
    (should (equal (asm-data--to-bytes ".4byte 0xfffefdfc") [252 253 254 255]))
    (should (equal (asm-data--to-bytes ".4byte -0") [0 0 0 0]))
    (should (equal (asm-data--to-bytes ".4byte -1") [255 255 255 255]))
    (should (equal (asm-data--to-bytes ".4byte -2") [254 255 255 255])))
  (let ((asm-data-endianness 'big))
    (should (equal (asm-data--to-bytes ".4byte 1234567") [0 18 214 135]))
    (should (equal (asm-data--to-bytes ".4byte 0xff") [0 0 0 255]))
    (should (equal (asm-data--to-bytes ".4byte -2") [255 255 255 254])))

  (should-error (asm-data--to-bytes ".4byte \"string\""))
  (should-error (asm-data--to-bytes ".4byte 1.0"))
  (should-error (asm-data--to-bytes ".4byte 4294967296"))
  (should-error (asm-data--to-bytes ".4byte -2147483649"))
  (should-error (asm-data--to-bytes ".4byte -+2147483649"))

  (when asm-data--big-values-support
    (let ((asm-data-endianness 'little))
      (should (equal (asm-data--to-bytes ".8byte 4294967296")
                     [0 0 0 0 1 0 0 0]))
      (should (equal (asm-data--to-bytes ".8byte 18446744073709551615")
                     [255 255 255 255 255 255 255 255]))
      (should (equal (asm-data--to-bytes ".8byte 18446462598732840960")
                     [0 0 0 0 0 0 255 255]))
      (should (equal (asm-data--to-bytes ".8byte -1")
                     [255 255 255 255 255 255 255 255]))
      (should (equal (asm-data--to-bytes ".8byte -2")
                     [254 255 255 255 255 255 255 255])))
    (let ((asm-data-endianness 'big))
      (should (equal (asm-data--to-bytes ".8byte 18446462598732840960")
                     [255 255 0 0 0 0 0 0]))
      (should (equal (asm-data--to-bytes ".8byte -256")
                     [255 255 255 255 255 255 255 0])))

    (should-error (asm-data--to-bytes ".8byte 18446744073709551616"))
    (should-error (asm-data--to-bytes ".8byte -9223372036854775809"))
    (should-error (asm-data--to-bytes ".8byte -+9223372036854775809")))

  (let ((asm-data-endianness 'little))

    (should (equal (asm-data--to-bytes ".single 0.0")
                   (asm-data-test--binary-str-to-bytes
                    "00000000000000000000000000000000" 4)))

    (should (equal (asm-data--to-bytes ".single -0.0")
                   (asm-data-test--binary-str-to-bytes
                    "10000000000000000000000000000000" 4)))

    (should (equal (asm-data--to-bytes ".single 1.0")
                   (asm-data-test--binary-str-to-bytes
                    "00111111100000000000000000000000" 4)))

    (should (equal (asm-data--to-bytes ".single 1.00000011920928955")
                   (asm-data-test--binary-str-to-bytes
                    "00111111100000000000000000000001" 4)))

    (should (equal (asm-data--to-bytes ".single 0.333333343267440796")
                   (asm-data-test--binary-str-to-bytes
                    "00111110101010101010101010101011" 4)))

    (should (equal (asm-data--to-bytes ".single 0.999999940395355225")
                   (asm-data-test--binary-str-to-bytes
                    "00111111011111111111111111111111" 4)))

    (should (equal (asm-data--to-bytes ".single 0.125")
                   (asm-data-test--binary-str-to-bytes
                    "00111110000000000000000000000000" 4)))

    (should (equal (asm-data--to-bytes ".single -2.0")
                   (asm-data-test--binary-str-to-bytes
                    "11000000000000000000000000000000" 4)))

    (should (equal (asm-data--to-bytes ".single 3.14159274101257324")
                   (asm-data-test--binary-str-to-bytes
                    "01000000010010010000111111011011" 4)))

    (should (equal (asm-data--to-bytes ".single 1.401298464324817e-45")
                   (asm-data-test--binary-str-to-bytes
                    "00000000000000000000000000000001" 4)))

    (should (equal (asm-data--to-bytes ".single 2.802596928649634e-45")
                   (asm-data-test--binary-str-to-bytes
                    "00000000000000000000000000000010" 4)))

    (should (equal (asm-data--to-bytes ".single 1.3242270487869521e-42")
                   (asm-data-test--binary-str-to-bytes
                    ;; 945
                    "00000000000000000000001110110001" 4)))

    (should (equal (asm-data--to-bytes ".single 1.1754942107e-38")
                   (asm-data-test--binary-str-to-bytes
                    "00000000011111111111111111111111" 4)))

    (should (equal (asm-data--to-bytes ".single 1.1754940705701536e-38")
                   (asm-data-test--binary-str-to-bytes
                    "00000000011111111111111111111110" 4)))

    (should (equal (asm-data--to-bytes ".single 5.877473155409902e-39")
                   (asm-data-test--binary-str-to-bytes
                    "00000000010000000000000000000001" 4)))

    (should (equal (asm-data--to-bytes ".single 5.880341613366375e-39")
                   (asm-data-test--binary-str-to-bytes
                    "00000000010000000000100000000000" 4)))

    (dotimes (i 2048)
      (should (equal (asm-data--to-bytes
                      (format ".single %.32g" (* (1+ i) 1.401298464324817e-45)))
                     (asm-data--integer-to-vector (1+ i) 4))))

    (should (equal (asm-data--to-bytes ".single inf")
                   (asm-data-test--binary-str-to-bytes
                    "01111111100000000000000000000000" 4)))

    (should (equal (asm-data--to-bytes ".single -inf")
                   (asm-data-test--binary-str-to-bytes
                    "11111111100000000000000000000000" 4)))

    (should (equal (asm-data--to-bytes ".single nan")
                   (asm-data-test--binary-str-to-bytes
                    "01111111110000000000000000000000" 4)))

    (should (equal (asm-data--to-bytes ".single -nan")
                   (asm-data-test--binary-str-to-bytes
                    "11111111110000000000000000000000" 4))))

  (should (equal (asm-data--to-bytes ".ascii \"abcd\"") [?a ?b ?c ?d]))
  (should (equal (asm-data--to-bytes ".asciz \"abcd\"") [?a ?b ?c ?d 0]))
  (should (equal (asm-data--to-bytes ".ascii \"abcd\", \"e\"")
                 [?a ?b ?c ?d ?e]))
  (should (equal (asm-data--to-bytes ".asciz \"abcd\", \"e\"")
                 [?a ?b ?c ?d 0 ?e 0]))
  (should (equal (asm-data--to-bytes ".ascii \"\\0\"") [0]))
  (should (equal (asm-data--to-bytes ".asciz \"\"") [0]))
  (should (equal (asm-data--to-bytes ".ascii \" \"") [?\ ]))
  (should (equal (asm-data--to-bytes ".ascii \"\\\"\"") [?\"]))

  (should (equal (asm-data--to-bytes ".ascii \"\\x00ff\"") [255]))
  (should (equal (asm-data--to-bytes ".ascii \"\\x00ff\\x00fe\"") [255 254]))
  (should (equal (asm-data--to-bytes ".ascii \"\\010\"") [8]))
  (should (equal (asm-data--to-bytes ".ascii \"\\010\\077\"") [8 63]))

  (should (equal (asm-data--to-bytes (string-join
                                      '(".ascii \"\\x00ff\""
                                        ".byte 123"
                                        ".zero 3"
                                        ) "\n\t"))
                 [255 123 0 0 0]))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--to-bytes (string-join
                                        '(".asciz \"a\", \"b\""
                                          ".byte 123"
                                          ".zero 3"
                                          ".2byte 0x7ff0"
                                          ".4byte 12345678"
                                          ) "\n\t"))
                   [97 0 98 0 123 0 0 0 240 127 78 97 188 0]))))

(ert-deftest asm-data-conversion-from-bytes ()

  (should (equal (asm-data--do-conversion ".ascii" [0 0 0 0])
                 '((".ascii" . "\"\\0\\0\\0\\0\""))))

  (should (equal (asm-data--do-conversion ".ascii" [?a ?b ?c ?d])
                 '((".ascii" . "\"abcd\""))))

  (should (equal (asm-data--do-conversion ".ascii" [200 205 0])
                 '((".ascii" . "\"\\x00c8\\x00cd\\0\""))))

  (should (equal (asm-data--do-conversion ".asciz" [?a 0 0])
                 '((".asciz" . "\"a\"")
                   (".asciz" . "\"\""))))

  (should (equal (asm-data--do-conversion ".byte" [0 0 0 0])
                 '((".byte" . "0")
                   (".byte" . "0")
                   (".byte" . "0")
                   (".byte" . "0"))))

  (should (equal (asm-data--do-conversion ".byte" [127])
                 '((".byte" . "127"))))

  (should (equal (asm-data--do-conversion ".byte" [255] t)
                 '((".byte" . "-1"))))

  (should (equal (asm-data--do-conversion ".byte" [254] t)
                 '((".byte" . "-2"))))

  (should (equal (asm-data--do-conversion ".byte" [255])
                 '((".byte" . "255"))))

  (should (equal (asm-data--do-conversion ".byte" [255] nil 16)
                 '((".byte" . "0xff"))))

  (should (equal (asm-data--do-conversion ".byte" [254] nil 16)
                 '((".byte" . "0xfe"))))

  (should (equal (asm-data--do-conversion ".byte" [255] nil 2)
                 '((".byte" . "0b11111111"))))

  (should (equal (asm-data--do-conversion ".byte" [254] nil 2)
                 '((".byte" . "0b11111110"))))

  (should (equal (asm-data--do-conversion ".byte" [1] nil 2)
                 '((".byte" . "0b00000001"))))

  (should (equal (asm-data--do-conversion ".2byte" [0 0 0 0])
                 '((".2byte" . "0") (".2byte" . "0"))))

  (should (equal (asm-data--do-conversion ".2byte" [0])
                 '((".byte" . "0"))))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion ".2byte" [0 1 2])
                   '((".2byte" . "256") (".byte" . "2")))))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion ".short" [0 1 2])
                   '((".short" . "256") (".byte" . "2")))))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion ".2byte" [1 0 1 0])
                   '((".2byte" . "1") (".2byte" . "1")))))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion ".hword" [1 0 1 0])
                   '((".hword" . "1") (".hword" . "1")))))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion ".2byte" [1 2 254 255])
                   '((".2byte" . "513") (".2byte" . "65534")))))

  (let ((asm-data-endianness 'big))
    (should (equal (asm-data--do-conversion ".2byte" [1 0 1 0])
                   '((".2byte" . "256") (".2byte" . "256")))))

  (let ((asm-data-endianness 'big))
    (should (equal (asm-data--do-conversion ".2byte" [1 255 0 128])
                   '((".2byte" . "511") (".2byte" . "128")))))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion ".2byte" [255 255])
                   '((".2byte" . "65535"))))

    (should (equal (asm-data--do-conversion ".2byte" [255 255] t)
                   '((".2byte" . "-1"))))

    (should (equal (asm-data--do-conversion ".2byte" [255 127] t)
                   '((".2byte" . "32767"))))

    (should (equal (asm-data--do-conversion ".2byte" [0 128] t)
                   '((".2byte" . "-32768"))))

    (should (equal (asm-data--do-conversion ".2byte" [254 255] t)
                   '((".2byte" . "-2"))))

    (should (equal (asm-data--do-conversion ".2byte" [253 255] t)
                   '((".2byte" . "-3"))))

    (should (equal (asm-data--do-conversion ".2byte" [252 255] t)
                   '((".2byte" . "-4"))))

    (should (equal (asm-data--do-conversion ".2byte" [255 255] nil 16)
                   '((".2byte" . "0xffff"))))

    (should (equal (asm-data--do-conversion ".2byte" [254 255] nil 16)
                   '((".2byte" . "0xfffe"))))

    (should (equal (asm-data--do-conversion ".2byte" [255 0] nil 16)
                   '((".2byte" . "0x00ff")))))

  (let ((asm-data-endianness 'big))
    (should (equal (asm-data--do-conversion ".2byte" [255 255])
                   '((".2byte" . "65535"))))
    (should (equal (asm-data--do-conversion ".2byte" [255 255] t)
                   '((".2byte" . "-1"))))

    (should (equal (asm-data--do-conversion ".2byte" [255 127] t)
                   '((".2byte" . "-129"))))

    (should (equal (asm-data--do-conversion ".2byte" [0 128] t)
                   '((".2byte" . "128")))))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion ".4byte" [255 255 255 255])
                   '((".4byte" . "4294967295"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 255 255 255] nil 2)
                   '((".4byte" . "0b11111111111111111111111111111111"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 1 255 255] nil 2)
                   '((".4byte" . "0b11111111111111110000000111111111"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 255 255 255] t)
                   '((".4byte" . "-1"))))

    (should (equal (asm-data--do-conversion ".4byte" [254 255 255 255] t)
                   '((".4byte" . "-2"))))

    (should (equal (asm-data--do-conversion ".4byte" [253 255 255 255] t)
                   '((".4byte" . "-3"))))

    (should (equal (asm-data--do-conversion ".4byte" [1 0 0 0])
                   '((".4byte" . "1"))))

    (should (equal (asm-data--do-conversion ".int" [1 0 0 0])
                   '((".int" . "1"))))

    (should (equal (asm-data--do-conversion ".long" [1 0 0 0])
                   '((".long" . "1"))))

    (should (equal (asm-data--do-conversion ".4byte" [0 1 0 0])
                   '((".4byte" . "256"))))

    (should (equal (asm-data--do-conversion ".4byte" [0 2 0 0])
                   '((".4byte" . "512"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 255 255 255] nil 16)
                   '((".4byte" . "0xffffffff"))))

    (should (equal (asm-data--do-conversion ".4byte" [254 255 255 255] nil 16)
                   '((".4byte" . "0xfffffffe"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 0 255 255] nil 16)
                   '((".4byte" . "0xffff00ff"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 0 255 127] nil 16)
                   '((".4byte" . "0x7fff00ff")))))

  (let ((asm-data-endianness 'big))
    (should (equal (asm-data--do-conversion ".4byte" [255 255 255 255])
                   '((".4byte" . "4294967295"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 255 255 255] t)
                   '((".4byte" . "-1"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 255 255 254] t)
                   '((".4byte" . "-2"))))

    (should (equal (asm-data--do-conversion ".4byte" [255 255 255 253] t)
                   '((".4byte" . "-3"))))

    (should (equal (asm-data--do-conversion ".4byte" [127 255 255 255] t)
                   '((".4byte" . "2147483647"))))

    (should (equal (asm-data--do-conversion ".4byte" [128 0 0 0] t)
                   '((".4byte" . "-2147483648"))))

    (should (equal (asm-data--do-conversion ".4byte" [1 0 0 0])
                   '((".4byte" . "16777216"))))

    (should (equal (asm-data--do-conversion ".4byte" [0 0 0 1])
                   '((".4byte" . "1"))))

    (should (equal (asm-data--do-conversion ".4byte" [0 1 0 0])
                   '((".4byte" . "65536"))))

    (should (equal (asm-data--do-conversion ".4byte" [0 0 1 0])
                   '((".4byte" . "256")))))

  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion ".4byte" [255 128 0])
                   '((".byte" . "255") (".byte" . "128") (".byte" . "0")))))

  (when asm-data--big-values-support
    (let ((asm-data-endianness 'little))
      (should (equal (asm-data--do-conversion
                      ".8byte" [255 255 255 255 255 255 255 255])
                     '((".8byte" . "18446744073709551615"))))
      (should (equal (asm-data--do-conversion
                      ".8byte" [255 255 255 255 255 0 0 0])
                     '((".8byte" . "1099511627775"))))
      (should (equal (asm-data--do-conversion
                      ".8byte" [255 255 255 255 255 255 255 255] t)
                     '((".8byte" . "-1"))))
      (should (equal (asm-data--do-conversion
                      ".8byte" [128 255 255 255 255 255 255 255] t)
                     '((".8byte" . "-128")))))

    (let ((asm-data-endianness 'little))
      (should (equal (asm-data--do-conversion
                      ".octa" [255 255 255 255 255 255 255 255
                                   255 255 255 255 255 255 255 255])
                     '((".octa" . "340282366920938463463374607431768211455"))))
      (should (equal (asm-data--do-conversion
                      ".octa" [0 0 255 255 255 255 255 255
                                   255 255 255 255 255 255 255 255]
                      t)
                     '((".octa" . "-65536")))))

    (let ((asm-data-endianness 'big))
      (should (equal (asm-data--do-conversion
                      ".octa" [255 255 255 255 255 255 255 255
                                   255 255 255 255 255 255 255 255])
                     '((".octa" . "340282366920938463463374607431768211455"))))
      (should (equal (asm-data--do-conversion
                      ".octa" [0 0 255 255 255 255 255 255
                                   255 255 255 255 255 255 255 255]
                      t)
                     '((".octa" . "5192296858534827628530496329220095"))))
      (should (equal (asm-data--do-conversion
                      ".octa" [128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                      t)
                     '((".octa" . "-170141183460469231731687303715884105728"))))))


  (let ((asm-data-endianness 'little))
    (should (equal (asm-data--do-conversion
                    ".single" (asm-data-test--binary-str-to-bytes
                               "11111111100000000000000000000000" 4))
                   '((".single" . "-inf"))))
    (should (equal (asm-data--do-conversion
                    ".single" (asm-data-test--binary-str-to-bytes
                               "01111111100000000000000000000000" 4))
                   '((".single" . "inf"))))

    (should (equal (asm-data--do-conversion
                    ".single" (asm-data-test--binary-str-to-bytes
                               "01111111110000000000000000000000" 4))
                   '((".single" . "nan"))))
    (should (equal (asm-data--do-conversion
                    ".single" (asm-data-test--binary-str-to-bytes
                               "11111111110000000000000000000000" 4))
                   '((".single" . "-nan")))))

  (should (equal (asm-data--do-conversion ".zero" [0 0 0 0 0])
                 '((".zero" . "5"))))

  (should (equal (asm-data--do-conversion ".zero" [0 0 0 0 1])
                 '((".zero" . "4") (".byte" . "1"))))

  (should (equal (asm-data--do-conversion ".zero" [0 0 0 0 1 0 0 0 2])
                 '((".zero" . "4") (".byte" . "1")
                   (".zero" . "3") (".byte" . "2"))))

  (should (equal (asm-data--do-conversion ".zero" [0 0 0 0 1 2 3 0])
                 '((".zero" . "4") (".byte" . "1")
                   (".byte" . "2") (".byte" . "3")
                   (".byte" . "0")))))

(provide 'asm-data-test)
;;; asm-data-test.el ends here
