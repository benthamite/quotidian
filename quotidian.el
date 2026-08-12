;;; quotidian.el --- Display quotations from authoritative sources -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/quotidian
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Display quotations from the Oxford Dictionary of Quotations and other
;; authoritative sources, either in a review buffer or in the echo area.

;;; Code:

(defgroup quotidian nil
  "Display quotations from authoritative sources."
  :group 'applications
  :link '(url-link :tag "Homepage" "https://github.com/benthamite/quotidian"))

(provide 'quotidian)
;;; quotidian.el ends here
