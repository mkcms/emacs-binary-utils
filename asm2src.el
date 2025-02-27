;;; asm2src.el --- ASM to source file overlays     -*- lexical-binding: t; -*-

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
;; This package allows you to jump from ASM buffer to a source file and vice
;; versa.
;;
;; `asm2src-process-buffer' is the main function which scans the current ASM
;; buffer for file mappings, it must be called once before further usage.
;;
;; When using `binfile' and the rest of the packages here, you should add:
;;
;;   (setq bdx-disassembler-options
;;         (concat bdx-disassembler-options " --line-numbers"))
;;   (add-hook 'binfile-buffer-postprocessing-functions
;;             #'asm2src-process-buffer)
;;
;; To your init file.  The first sexp makes objdump output source file mappings
;; when dumping disassembly, and the second makes sure we can parse and use
;; that in Emacs.
;;
;; `asm2src-jump' allows you to jump to the source buffer from the preprocessed
;; ASM buffer.  It sets a transient keymap, `asm2src-jump-keymap' for the
;; duration of the command: `C-c C-c' or `RET' goes to the source file, `p'
;; goes to the previous mapped location, `n' goes to the next.
;;
;; `asm2src-jump-to-asm' is the inverse, it should be invoked in a source file
;; and will go to the first ASM buffer containing the source line.
;;
;; `asm2src-add-mapping' can be used to add a custom ASM<->source directory
;; mapping.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'pulse)
(require 'rx)
(require 'seq)
(require 'subr-x)

(defvar asm2src-location-regexp
  "^\\([^:\n]+\\):\\([0-9]+\\)\\s-*\\((discriminator.*)\\)?$"
  "Regexp matching a source file location in ASM buffer.")

(defun asm2src-scan ()
  "Scan the buffer for source mapping."
  (let ((re asm2src-location-regexp))
    (when (re-search-forward re nil t)
      (let ((filename (match-string-no-properties 1))
            (line (string-to-number (match-string-no-properties 2)))
            (this-line-start (move-marker (make-marker) (match-beginning 0)))
            (this-line-end (move-marker (make-marker) (1+ (match-end 0))))
            (end (save-excursion
                   (apply #'min
                          (remq nil
                                (list
                                 (save-excursion
                                   (and (re-search-forward
                                         asm2src-location-regexp nil t)
                                        (line-beginning-position)))
                                 (save-excursion
                                   (re-search-forward "^\\s-*$" nil t))
                                 (point-max)))))))

        (prog1
            (list :file (expand-file-name filename) :line line
                  :asm-info-line-start-point this-line-start
                  :asm-info-line-end-point this-line-end
                  :asm-start-point (move-marker (make-marker) this-line-end)
                  :asm-end-point (move-marker (make-marker) end))
          (goto-char end))))))

(defun asm2src-propertize--asm-region (beg end file line)
  "Mark region BEG END as mapping to LINE in FILE."
  (let ((buffer-read-only nil))
    (add-text-properties beg end
                         `(
                           asm2src-file ,file
                           asm2src-line ,line
                           asm2src-id   ,(cl-gensym)
                           ))))

(defun asm2src-process-buffer ()
  "Process source mappings in current buffer.
This adds text properties in current buffer and removes visible
source mapping information.  Afterwards, `asm2src-jump' can be
used to jump to source."
  (save-excursion
    (goto-char (point-min))
    (let (scan
          (buffer-read-only nil))
      (remove-list-of-text-properties (point-min) (point-max)
                                      '(asm2src-file asm2src-line asm2src-id))
      (while (setq scan (asm2src-scan))
        (cl-destructuring-bind
            (&key file line
                  asm-start-point asm-end-point
                  asm-info-line-start-point asm-info-line-end-point)
            scan

          ;; Remove function names which objdump outputs
          (save-excursion
            (goto-char asm-info-line-start-point)
            (forward-line -1)
            (while (< (point) asm-end-point)
              (when (looking-at-p "^\\(.*[(].*[)].*\\):\\s-*$")
                (delete-region
                 (line-beginning-position) (1+ (line-end-position))))
              (forward-line 1)))

          (delete-region asm-info-line-start-point asm-info-line-end-point)
          (asm2src-propertize--asm-region asm-start-point asm-end-point
                                         file line))))))

(defvar asm2src-mapping)

(defun asm2src--find-mapped-file (filename)
  "Try to map FILENAME to the actual filename using user provided mapping."
  (let ((dir (file-name-directory filename))
        ent)
    (setq ent (cl-find-if (lambda (elem)
                            (string-match-p
                             (concat "^" (regexp-quote (car elem)) ".*") dir))
                          asm2src-mapping))
    (or
     (when ent
       (string-match (concat "^" (regexp-quote (car ent)) "\\(.*\\)") filename)
       (expand-file-name
        (match-string 1 filename)
        (cdr ent)))
     filename)))

(defun asm2src--region-at (point)
  "Return a plist for the asm region at POINT.
The plist contains :file, :line, :beg, :end keys.

The return value is nil if there is no mapped region at POINT."
  (when-let* ((id (get-text-property point 'asm2src-id)))
    (list
     :file (get-text-property point 'asm2src-file)
     :line (get-text-property point 'asm2src-line)
     :beg (let ((end-prev
                 (previous-single-char-property-change point 'asm2src-id)))
            (if (eq (get-text-property end-prev 'asm2src-id) id)
                end-prev (point)))
     :end (next-single-char-property-change point 'asm2src-id))))

(defun asm2src--find-next-region ()
  "Move point to the next region mapped to a file, and return it or nil.
On success, the point will be moved to the very beginning of the
region, and the return value will be that of
`asm2src--region-at'.

On failure, nil is returned and the point will be moved to `point-max'."
  (catch 'res
    (while (progn
             (goto-char
              (or (next-single-char-property-change (point) 'asm2src-id)
                  (point-max)))
             (not (eobp)))
      (when (get-text-property (point) 'asm2src-file)
        (throw 'res (asm2src--region-at (point)))))))

(defun asm2src--find-previous-region ()
  "Move point to the previous region mapped to a file, and return it or nil.
On success, the point will be moved to the very beginning of the
region, and the return value will be that of
`asm2src--region-at'.

On failure, nil is returned and the point will be moved to `point-min'."
  (catch 'res
    (while (progn
             (goto-char
              (or (previous-single-char-property-change (point) 'asm2src-id)
                  (point-min)))
             (not (bobp)))
      (when (get-text-property (point) 'asm2src-file)
        (throw 'res (asm2src--region-at (point)))))))

(defun asm2src--make-overlays ()
  "Make temporary overlays for source mapping."
  (let (overlays region)
    (save-excursion
      (goto-char (point-min))
      (while (setq region (asm2src--find-next-region))
        (cl-destructuring-bind (&key beg end file line) region
          (let ((ov (make-overlay beg end))
                (mapped-file (asm2src--find-mapped-file file)))
            (push ov overlays)
            (overlay-put ov 'before-string
                         (propertize (format "%s:%s\n" mapped-file line)
                                     'face 'default))
            (overlay-put ov 'face 'shadow)
            (overlay-put ov 'priority '100)))))
    overlays))

(defvar asm2src-jump-keymap
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") #'asm2src-jump-to-source-file-at-point)
    (define-key map (kbd "RET") #'asm2src-jump-to-source-file-at-point)
    (define-key map (kbd "p") #'asm2src-prev-location)
    (define-key map (kbd "n") #'asm2src-next-location)
    map)
  "Keymap used for jumping from ASM buffer.")

(defvar asm2src--current-asm-buffer nil)
(defvar asm2src--current-asm-window nil)
(defvar asm2src--filename-filter nil)

(defun asm2src-jump (&optional filename)
  "Jump to source from current ASM buffer.
This displays temporary overlays showing source file names.

If FILENAME is given, only show source overlays for that file.
Interactively, FILENAME can be given by using the prefix
argument.

While the overlays are active, you can press keys to move to
next/previous overlay or to jump to this source line in another
window:

\\{asm2src-jump-keymap}"
  (interactive
   (when current-prefix-arg
     (list
      (completing-read
       "Filter in on file: "
       (save-excursion
         (save-restriction)
         (widen)
         (goto-char (point-min))
         (cl-loop for region = (asm2src--find-next-region)
                  while region
                  collect (plist-get region :file) into files
                  finally return (cl-remove-duplicates files :test #'string=)))
       nil t))))
  (save-excursion
    (save-restriction
      (widen)

      (setq asm2src--current-asm-buffer (current-buffer))
      (setq asm2src--current-asm-window (selected-window))
      (setq asm2src--filename-filter filename)

      (let* ((buffer-read-only nil)
             (overlays (asm2src--make-overlays))
             (delete-overlays (lambda () (mapc #'delete-overlay overlays))))
        (condition-case err
            (progn
              (when filename
                (dolist (ov overlays)
                  (unless (equal
                           (get-text-property (overlay-start ov) 'asm2src-file)
                           filename)
                    (overlay-put ov 'display "\n")
                    (overlay-put ov 'before-string nil))))

              (set-transient-map asm2src-jump-keymap t delete-overlays))
          (error (funcall delete-overlays) (signal (car err) (cdr err)))
          (quit (funcall delete-overlays) (signal 'quit nil)))))))

(defun asm2src-jump-to-source-file-at-point ()
  "Jump to source file at point."
  (interactive)
  (if-let* ((file (get-text-property (point) 'asm2src-file))
            (line (get-text-property (point) 'asm2src-line)))
      (progn
        (setq file (asm2src--find-mapped-file file))
        (with-current-buffer
            (pop-to-buffer (find-file-noselect file))
          (widen)
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter-top-bottom)
          (let ((pulse-iterations 10)
                (pulse-delay 0.1))
            (pulse-momentary-highlight-one-line (point)))))
    (error "There is no source mapping overlay at point")))

(defun asm2src-next-location ()
  "Go to next source line overlay."
  (interactive)
  (select-window asm2src--current-asm-window)
  (switch-to-buffer asm2src--current-asm-buffer)
  (cl-loop for region = (asm2src--find-next-region)
           while region
           for region-file = (plist-get region :file)
           until (or (null asm2src--filename-filter)
                     (equal region-file
                            asm2src--filename-filter)))
  (let ((recenter-positions '(middle)))
    (recenter-top-bottom)))

(defun asm2src-prev-location ()
  "Go to previous source line overlay."
  (interactive)
  (select-window asm2src--current-asm-window)
  (switch-to-buffer asm2src--current-asm-buffer)
  (cl-loop for region = (asm2src--find-previous-region)
           while region
           for region-file = (plist-get region :file)
           until (or (null asm2src--filename-filter)
                     (equal region-file
                            asm2src--filename-filter)))
  (let ((recenter-positions '(middle)))
    (recenter-top-bottom)))

(defun asm2src-jump-to-asm ()
  "Jump from source buffer to asm buffer."
  (interactive)
  (pcase-let*
      ((filename (buffer-file-name))
       (current-line (line-number-at-pos (point)))
       (`(,buf ,beg ,end)
        (catch 'done
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (cl-loop
                   for region = (asm2src--find-next-region)
                   while region
                   for region-file
                   = (asm2src--find-mapped-file (plist-get region :file))
                   for region-line = (plist-get region :line)
                   if (and (file-equal-p region-file filename)
                           (= region-line current-line))
                   do (throw
                       'done
                       (list buf
                             (plist-get region :beg)
                             (plist-get region :end)))))))))))
    (if buf
        (with-current-buffer (pop-to-buffer buf)
          (goto-char beg)
          (let ((pulse-iterations 10)
                (pulse-delay 0.1))
            (pulse-momentary-highlight-region beg end)))
      (error "No location found for %s:%s" filename current-line))))

(defvar asm2src-mapping nil
  "Alist of source directory mappings.

Keys are strings which are matched against the filename in ASM buffer.
Values are names of real directories.")

(defun asm2src-add-mapping (asm-dir src-dir)
  "Add a file mapping, mapping ASM-DIR from the ASM buffer to SRC-DIR on disk.
Interactively, prompts for the name of a directory that exists in
the current ASM buffer, and the name of a directory it maps to on disk."
  (interactive
   (let* ((dirs
           (cl-delete-duplicates
            (mapcar #'file-name-directory
                    (save-excursion
                      (goto-char (point-min))
                      (cl-loop for data = (asm2src--find-next-region)
                               while data
                               collect (plist-get data :file))))
            :test #'string=))
          (asm-dir
           (file-name-as-directory
            (completing-read "Map from ASM directory: " dirs)))
          (src-dir
           (expand-file-name
            (read-directory-name (format "Map %s to: " asm-dir)
                                 nil nil nil asm-dir))))
     (list asm-dir src-dir)))
  (setf (map-elt asm2src-mapping asm-dir nil #'string=) src-dir))


(provide 'asm2src)
;;; asm2src.el ends here
