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

(require 'seq)

(defgroup quotidian nil
  "Display quotations from authoritative sources."
  :group 'applications
  :link '(url-link :tag "Homepage" "https://github.com/benthamite/quotidian"))

(defcustom quotidian-corpus-files
  (list (expand-file-name "data/quotidian-sample.json"
                          (file-name-directory (or load-file-name buffer-file-name))))
  "Files holding quotations, read in order.
Each file is a JSON array of objects.  An object must have the keys
\"text\" and \"author\", and may have \"source\", \"date\" and \"tags\"."
  :type '(repeat file)
  :group 'quotidian)

(defvar quotidian--corpus nil
  "Parsed quotations, or nil if they have not been read yet.")

(defun quotidian-reload ()
  "Read the corpus files again, discarding the cached quotations."
  (interactive)
  (setq quotidian--corpus nil)
  (message "Quotidian: %d quotations" (length (quotidian--ensure-corpus))))

(defun quotidian--ensure-corpus ()
  "Return the quotations, reading them from disk on the first call."
  (or quotidian--corpus (setq quotidian--corpus (quotidian--load))))

(defun quotidian--load ()
  "Return the quotations in every file of `quotidian-corpus-files'."
  (apply #'append (mapcar #'quotidian--read-file quotidian-corpus-files)))

(defun quotidian--read-file (file)
  "Return the valid quotations in FILE.
Return nil, after a warning, if FILE is unreadable or is not valid
JSON."
  (if (not (file-readable-p file))
      (quotidian--warn "Corpus file not found, skipping it: %s" file)
    (condition-case error
        (quotidian--validate (quotidian--parse file) file)
      (json-parse-error
       (quotidian--warn "Corpus file is not valid JSON, skipping it: %s (%s)"
                        file (error-message-string error))))))

(defun quotidian--parse (file)
  "Return the JSON array in FILE as a list of plists."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(defun quotidian--validate (quotes file)
  "Return the entries of QUOTES that have both text and an author.
Warn if any entry from FILE is dropped."
  (if (not (listp quotes))
      (quotidian--warn "Corpus file is not a JSON array, skipping it: %s" file)
    (let* ((valid (seq-filter #'quotidian--valid-p quotes))
           (dropped (- (length quotes) (length valid))))
      (when (> dropped 0)
        (quotidian--warn "Dropped %d entries without text or author: %s"
                         dropped file))
      valid)))

(defun quotidian--valid-p (quote)
  "Return non-nil if QUOTE is a plist with a non-empty text and author."
  (and (plistp quote)
       (quotidian--non-empty-p (plist-get quote :text))
       (quotidian--non-empty-p (plist-get quote :author))))

(defun quotidian--non-empty-p (value)
  "Return non-nil if VALUE is a string with at least one character."
  (and (stringp value) (not (string-empty-p value))))

(defun quotidian--warn (format &rest args)
  "Warn with FORMAT and ARGS, and return nil.
Returning nil lets callers use this directly as a failure branch."
  (display-warning 'quotidian (apply #'format format args) :warning)
  nil)

(provide 'quotidian)
;;; quotidian.el ends here
