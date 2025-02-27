;;; emacs-binary-utils.el ---      -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2024  Michał Krzywkowski

;; Author: Michał Krzywkowski <k.michal@zoho.com>
;; Keywords: languages, tools
;; Package-Requires: ((emacs "27") (seq "2.24") (project "0.10.0"))

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

;;; Code:

(require 'asm-data)
(require 'asm-jump)
(require 'asm-x86)
(require 'asm2src)
(require 'bdx)
(require 'binfile)
(require 'compdb)
(require 'compiled-file)

(provide 'emacs-binary-utils)
;;; emacs-binary-utils.el ends here
