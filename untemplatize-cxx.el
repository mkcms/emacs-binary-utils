;;; untemplatize-cxx.el --- Make templates readable -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: c, tools

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

;; This package provides a function, `untemplatize-cxx-buffer' which makes
;; the buffer show templated C++ symbols in a more readable way:
;;
;;   std::_Hashtable<int, std::pair<int const, char const*>,
;;     std::pmr::polymorphic_allocator<std::pair<int const, char const*>>,
;;     std::__detail::_Select1st, std::equal_to<int>, std::hash<int>,
;;     std::__detail::_Mod_range_hashing, std::__detail::_Default_ranged_hash,
;;       std::__detail::_Prime_rehash_policy,
;;       std::__detail::_Hashtable_traits<false, false, true>
;;     >::_M_rehash(unsigned long, std::integral_constant<bool, true>)
;;
;; Gets turned into:
;;
;; std::_Hashtable<...>::_M_rehash(unsigned long, std::integral_constant<...>)
;;
;; The package uses overlays to achieve this; the buffer contents are never
;; modified.
;;
;; After the buffer is processed, these commands can be used:
;;
;;    - `untemplatize-cxx-show'
;;
;;      Show one level of contents.  In the above example, when point is at the
;;      last "...", it will show the symbol as:
;;
;;      _M_rehash_aux(unsigned long, std::integral_constant<bool, true>)
;;
;;      This command only works when point is at an overlay shown as "...".
;;
;;    - `untemplatize-cxx-hide'
;;
;;      Hide the overlay at point, if there is one.  This only makes sense
;;      after calling `untemplatize-cxx-show' at point.
;;
;;    - `untemplatize-cxx-dwim'
;;
;;      If the overlay at point is showing as "..." then expand it, otherwise,
;;      try to hide the overlay at point.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defun untemplatize-cxx--scan-once ()
  "Search for a <>-delimited region.  Return (BEG . END), or nil.

BEG is the point right after the first '<' found and END is the
point right at the the matching '>'.  Point is moved right after
the end of the found region, after '>'.

Non-nil is only returned when all <> pairs are accounted for."
  (when (search-forward "<" nil t)
    (save-restriction
      (narrow-to-region (point) (line-end-position))
      (cl-loop with start = (point)
               with depth = 1
               until (or (zerop depth) (eobp))
               do (goto-char
                   (min
                    (or (save-excursion
                          (search-forward ">" nil t))
                        (point-max))
                    (or (save-excursion
                          (search-forward "<" nil t))
                        (point-max))))
               if (eq (char-before) ?<)
               do (cl-incf depth)
               else if (eq (char-before) ?>)
               do (cl-decf depth)
               finally return
               (if (zerop depth) (cons start (1- (point)))
                 (ignore (goto-char start)))))))

(defun untemplatize-cxx--scan ()
  "Search for a <>-delimited region and return that and all inner regions.

The return value is a cons (REGION . CHILDREN).  REGION is the
toplevel region as returned by `untemplatize-cxx--scan-once' and
CHILDREN is a list of such conses for all inner regions found."
  (when (search-forward "<" nil t)
    (forward-char -1)
    (when-let* ((bounds (untemplatize-cxx--scan-once)))
      (prog1 (save-restriction
               (goto-char (car bounds))
               (narrow-to-region (car bounds) (cdr bounds))
               (cons bounds
                     (cl-loop for more = (untemplatize-cxx--scan)
                              while more
                              collect more)))
        (goto-char (1+ (cdr bounds)))))))

(defvar-local untemplatize-cxx--overlays nil "Overlays in current buffer.")

(defun untemplatize-cxx--make-overlay (beg end depth)
  "Make an overlay for region BEG END and propertize it with DEPTH."
  (let ((ov (make-overlay beg end)))
    (push ov untemplatize-cxx--overlays)
    (overlay-put ov 'untemplatize-cxx t)
    (overlay-put ov 'untemplatize-cxx-depth depth)
    (overlay-put ov 'help-echo (buffer-substring beg end))
    (overlay-put ov 'display "...")))

(defun untemplatize-cxx--make-overlays (outer inner &optional depth)
  "Make an overlay for region OUTER and children INNER.

OUTER is the the toplevel region bounds; INNER is a list of
conses ((IBEG . IEND) . CHILDREN).  DEPTH defaults to 0 and is
incremented in recursive call.

This function is called recursively on each element of INNER."
  (setq depth (or depth 0))
  (untemplatize-cxx--make-overlay (car outer) (cdr outer) depth)
  (dolist (in inner)
    (untemplatize-cxx--make-overlays (car in) (cdr in) (1+ depth))))

(defun untemplatize-cxx-buffer ()
  "Make overlays for all <>-delimited regions in the current buffer."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (mapc #'delete-overlay untemplatize-cxx--overlays)
      (setq untemplatize-cxx--overlays nil)

      (cl-loop while (search-forward "<" nil t)
               do (forward-char -1)
               for obj = (untemplatize-cxx--scan)
               if obj
               do (untemplatize-cxx--make-overlays (car obj) (cdr obj))))))

(defun untemplatize-cxx--overlays-at-point ()
  "Get all the overlays at point for hiding <>-templates."
  (seq-filter (lambda (ov) (overlay-get ov 'untemplatize-cxx))
              (overlays-at (point))))

(defun untemplatize-cxx-show ()
  "If point as at an '...' overlay, show it and return non-nil."
  (interactive)
  (let ((ovs (untemplatize-cxx--overlays-at-point))
        (mindepth))
    (unless ovs (error "There are no overlays at point"))
    (setq ovs (seq-filter (lambda (ov) (overlay-get ov 'display)) ovs))
    (setq mindepth
          (car (cl-sort ovs #'> :key
                        (lambda (ov)
                          (overlay-get ov 'untemplatize-cxx-depth)))))
    (when mindepth
      (overlay-put mindepth 'display nil)
      t)))

(defun untemplatize-cxx-hide ()
  "If point as in overlay shown by `untemplatize-cxx-show', hide that region."
  (interactive)
  (let ((ovs (untemplatize-cxx--overlays-at-point))
        (maxdepth))
    (unless ovs (error "There are no overlays at point"))
    (setq ovs (seq-filter (lambda (ov) (null (overlay-get ov 'display))) ovs))
    (setq maxdepth (car (cl-sort ovs #'> :key
                                 (lambda (ov)
                                   (overlay-get ov 'untemplatize-cxx-depth)))))
    (when maxdepth
      (overlay-put maxdepth 'display "...")
      t)))

(defun untemplatize-cxx-dwim ()
  "Either show or hide the overlay at point."
  (interactive)
  (unless (untemplatize-cxx-show) (untemplatize-cxx-hide)))

(provide 'untemplatize-cxx)
;;; untemplatize-cxx.el ends here
