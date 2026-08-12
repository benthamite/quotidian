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

(require 'cl-lib)
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

(defvar quotidian--corpus 'unset
  "Parsed quotations, or `unset' if they have not been read yet.
The sentinel distinguishes a corpus that has not been read from one that
was read and turned out to be empty.")

(defun quotidian-reload ()
  "Read the corpus files again, discarding the cached quotations."
  (interactive)
  (setq quotidian--corpus 'unset)
  (message "Quotidian: %d quotations" (length (quotidian--ensure-corpus))))

(defun quotidian--ensure-corpus ()
  "Return the quotations, reading them from disk on the first call."
  (when (eq quotidian--corpus 'unset)
    (setq quotidian--corpus (quotidian--load)))
  quotidian--corpus)

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
                        file (error-message-string error)))
      (file-error
       (quotidian--warn "Corpus file could not be read, skipping it: %s (%s)"
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
  (and (stringp value) (not (string= value ""))))

(defun quotidian--warn (format &rest args)
  "Warn with FORMAT and ARGS, and return nil.
Returning nil lets callers use this directly as a failure branch."
  (display-warning 'quotidian (apply #'format format args) :warning)
  nil)

(defface quotidian-quote '((t :inherit default))
  "Face for the text of a quotation."
  :group 'quotidian)

(defface quotidian-author '((t :inherit font-lock-string-face))
  "Face for the author of a quotation."
  :group 'quotidian)

(defface quotidian-source '((t :inherit shadow))
  "Face for the source of a quotation."
  :group 'quotidian)

(defun quotidian-format (quote)
  "Return QUOTE as a propertized string.
The text comes first, then the attribution on its own line.  QUOTE is a
plist with the keys `:text', `:author', and optionally `:source' and
`:date'."
  (concat (propertize (plist-get quote :text) 'face 'quotidian-quote)
          "\n"
          (quotidian--format-attribution quote)))

(defun quotidian--format-attribution (quote)
  "Return the attribution of QUOTE as a propertized string.
The author, and the date if there is one, occupy the first line.  The
source, if there is one, occupies a second indented line."
  (let ((date (plist-get quote :date))
        (source (plist-get quote :source)))
    (concat (propertize (concat "— " (plist-get quote :author)
                                (when date (concat ", " date)))
                        'face 'quotidian-author)
            (when source
              (concat "\n" (propertize (concat "  " source)
                                       'face 'quotidian-source))))))

(defconst quotidian--echo-attempts 20
  "How many quotations `quotidian-echo' tries before it gives up.")

(defun quotidian--select ()
  "Return a quotation chosen at random, or nil if the corpus is empty."
  (when-let* ((corpus (quotidian--ensure-corpus)))
    (seq-random-elt corpus)))

(defun quotidian--select-fitting (max-lines width)
  "Return a formatted quotation of at most MAX-LINES lines, filled at WIDTH.
Return nil if `quotidian--echo-attempts' random draws all give a longer
quotation, or if the corpus is empty.  The return value is the filled
string, not the quotation itself."
  (cl-loop repeat quotidian--echo-attempts
           for quote = (quotidian--select)
           while quote
           for filled = (quotidian--fill (quotidian-format quote) width)
           when (<= (quotidian--line-count filled) max-lines)
           return filled))

(defun quotidian--fill (string width)
  "Return STRING with every line word-wrapped to WIDTH columns.
Lines are filled one at a time, so the line breaks already in STRING
survive."
  (mapconcat (lambda (line) (quotidian--fill-line line width))
             (split-string string "\n")
             "\n"))

(defun quotidian--fill-line (line width)
  "Return LINE word-wrapped to WIDTH columns."
  (with-temp-buffer
    (insert line)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun quotidian--line-count (string)
  "Return the number of lines in STRING."
  (1+ (cl-count ?\n string)))

(provide 'quotidian)
;;; quotidian.el ends here
