;;; asm-jump.el --- Buttons for jumps in ASM mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: languages, tools
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
;;
;; This package provides the function `asm-jump-process-buffer' which scans the
;; current buffer for ASM jump statements and makes buttons for them.
;;
;; The action for buttons created by this function will search the current
;; buffer for the referenced label and move the point there.
;;
;; Another command, `asm-jump-reverse' finds the first jump statement to the
;; current label.
;;

;;; Code:

(require 'cl-lib)
(require 'rx)

(defvar asm-jump-regexp ".+ \\([.]L[0-9]+\\)\\s-*$"
  "Regexp matching a jump label.  Group 1 is the label.")

(defvar asm-jump-target-regexp "^\\([.]L[0-9]+\\):$"
  "Regexp matching a jump target.  Group 1 is the label.")

(defun asm-jump-target (label)
  "Find the target for jump to LABEL, or return nil.
The return value is the start of the target label."
  (let ((re (format "^%s:\\s-*$" (regexp-quote label))))
    (save-restriction
      (widen)
      (narrow-to-region
       ;; Narrow to current function
       (or (save-excursion (re-search-backward "^$" (point-min) t))
           (point-min))
       (or (save-excursion (re-search-forward "^$" (point-max) t))
           (point-max)))
      (save-excursion
        (and (or (re-search-forward re nil t)
                 (progn (goto-char (point-min))
                        (re-search-forward label nil t)))
             (line-beginning-position))))))

(define-button-type 'asm-jump
  'action #'asm-jump-action
  'keymap (let ((map (make-sparse-keymap)))
            (define-key map [mouse-1] #'push-button)
            (define-key map (kbd "RET") #'push-button)
            map)
  'mouse-face 'highlight
  'help-echo "mouse-1, RET: jump to target")

(defun asm-jump-process-buffer ()
  "Make buttons for ASM jumps in current buffer."
  (remove-overlays nil nil 'type 'asm-jump)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((buffer-read-only nil)
            label)
        (while (and (re-search-forward asm-jump-regexp nil t)
                    (setq label (match-string-no-properties 1)))
          (let ((beg (match-beginning 1))
                (end (match-end 1)))
            (make-button beg end :type 'asm-jump
                         'label label)))))))

(defun asm-jump-action (button)
  "Go to jump target location of BUTTON."
  (interactive)
  (goto-char (asm-jump-target (button-get button 'label))))

(defun asm-jump-reverse ()
  "Go to place which jumps to current label."
  (interactive)
  (goto-char (line-end-position))
  (save-restriction
    (widen)
    (narrow-to-region
     ;; Narrow to current function
     (or
      (save-excursion (re-search-backward "^$" (point-min) t))
      (point-min))
     (or
      (save-excursion (re-search-forward "^$" (point-max) t))
      (point-max)))
    (when (re-search-backward asm-jump-target-regexp nil t)
      (let ((label (match-string 1))
            (pt (match-beginning 0)))
        (goto-char (point-min))
        (re-search-forward (format "%s$" (regexp-quote label)))
        (when (= (match-beginning 0) pt)
          (re-search-forward (format "%s$" (regexp-quote label))))))))

(provide 'asm-jump)
;;; asm-jump.el ends here
