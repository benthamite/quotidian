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
(require 'subr-x)

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

(defcustom quotidian-echo-max-lines 4
  "Greatest height, in lines, of a quotation shown in the echo area.
Quotations that need more lines are passed over.  Emacs may still clamp
this further; see `quotidian-echo'."
  :type 'natnum
  :group 'quotidian)

(defvar quotidian--corpus 'unset
  "Parsed quotations, or `unset' if they have not been read yet.
The sentinel distinguishes a corpus that has not been read from one that
was read and turned out to be empty.")

(defvar quotidian--skipped-files 0
  "Number of corpus files skipped by the most recent `quotidian-reload'.")

(defvar quotidian--dropped-entries 0
  "Number of corpus entries dropped by the most recent `quotidian-reload'.")

;;;###autoload
(defun quotidian-reload ()
  "Read the corpus files again, discarding the cached quotations.
Report how many quotations were loaded, how many files were skipped,
and how many entries were dropped; details are logged, quietly, to
`*Warnings*'."
  (interactive)
  (setq quotidian--corpus 'unset
        quotidian--skipped-files 0
        quotidian--dropped-entries 0)
  (message "Quotidian: %d quotations, %d files skipped, %d entries dropped"
           (length (quotidian--ensure-corpus))
           quotidian--skipped-files
           quotidian--dropped-entries))

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
Return nil, after a warning, if FILE is unreadable, is not valid JSON,
or holds something other than a JSON array."
  (if (not (file-readable-p file))
      (quotidian--skip-file "Corpus file not found, skipping it: %s" file)
    (condition-case error
        (quotidian--validate (quotidian--parse file) file)
      (json-parse-error
       (quotidian--skip-file
        "Corpus file is not valid JSON, skipping it: %s (%s)"
        file (error-message-string error)))
      (file-error
       (quotidian--skip-file
        "Corpus file could not be read, skipping it: %s (%s)"
        file (error-message-string error))))))

(defun quotidian--parse (file)
  "Return the JSON value in FILE, consed onto its top-level shape.
The value is parsed with objects as plists and arrays as lists, which
makes the two indistinguishable afterwards; hence the shape, read from
the text itself by `quotidian--json-shape'."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((value (json-parse-buffer :object-type 'plist :array-type 'list)))
      (cons value (quotidian--json-shape)))))

(defun quotidian--json-shape ()
  "Return the shape of the JSON value in the current buffer.
The shape is `array' for a value opening with a bracket, `object' for
one opening with a brace, and `atom' for anything else."
  (goto-char (point-min))
  (skip-chars-forward " \t\n\r\f")
  (pcase (char-after)
    (?\[ 'array)
    (?\{ 'object)
    (_ 'atom)))

(defun quotidian--validate (parsed file)
  "Return the valid entries of PARSED, the parse of FILE.
PARSED is what `quotidian--parse' returns: the JSON value consed onto
its top-level shape.  Warn if FILE holds no JSON array, or if any of
its entries is dropped."
  (pcase-let ((`(,quotes . ,shape) parsed))
    (if (eq shape 'array)
        (quotidian--keep-valid quotes file)
      (quotidian--skip-shape shape file))))

(defun quotidian--keep-valid (quotes file)
  "Return the valid entries of QUOTES.
Warn if any entry from FILE is dropped."
  (let* ((valid (seq-filter #'quotidian--valid-p quotes))
         (dropped (- (length quotes) (length valid))))
    (when (> dropped 0)
      (cl-incf quotidian--dropped-entries dropped)
      (quotidian--warn "Dropped %d invalid entries: %s" dropped file))
    valid))

(defun quotidian--skip-shape (shape file)
  "Warn that FILE holds SHAPE where an array was wanted, and return nil.
The object case is named for what it is, because wrapping the array in
an object is a likely first attempt and the entries are not at fault."
  (if (eq shape 'object)
      (quotidian--skip-file
       "Corpus file is a JSON object, not an array, skipping it: %s" file)
    (quotidian--skip-file "Corpus file is not a JSON array, skipping it: %s"
                          file)))

(defun quotidian--valid-p (quote)
  "Return non-nil if QUOTE is a plist with valid required and optional fields.
The required `:text' and `:author' must be non-empty strings.  The
optional `:date' and `:source' must be strings, and the optional
`:tags' must be a list, whenever present."
  (and (plistp quote)
       (quotidian--non-empty-p (plist-get quote :text))
       (quotidian--non-empty-p (plist-get quote :author))
       (quotidian--optional-p quote :date #'stringp)
       (quotidian--optional-p quote :source #'stringp)
       (quotidian--optional-p quote :tags #'listp)))

(defun quotidian--non-empty-p (value)
  "Return non-nil if VALUE is a string with at least one character."
  (and (stringp value) (not (string= value ""))))

(defun quotidian--optional-p (quote key predicate)
  "Return non-nil unless QUOTE has KEY set to a value PREDICATE rejects.
QUOTE lacking KEY at all is acceptable."
  (let ((value (plist-get quote key)))
    (or (null value) (funcall predicate value))))

(defun quotidian--skip-file (format &rest args)
  "Warn with FORMAT and ARGS about a skipped file, and return nil."
  (cl-incf quotidian--skipped-files)
  (apply #'quotidian--warn format args))

(defun quotidian--warn (format &rest args)
  "Log FORMAT and ARGS to `*Warnings*' without popping the buffer.
Returning nil lets callers use this directly as a failure branch."
  (let ((warning-suppress-types (cons '(quotidian) warning-suppress-types)))
    (display-warning 'quotidian (apply #'format format args)))
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

;;;###autoload
(defun quotidian-echo ()
  "Show a quotation, chosen at random, in the echo area.
Do nothing, and always return nil, when the minibuffer is in use, when
the corpus is empty, or when no quotation fits in the echo area.
Malformed corpus entries are dropped when the corpus is read, so this
command does not signal on them; see `quotidian--valid-p'."
  (interactive)
  (unless (active-minibuffer-window)
    (when-let* ((filled (quotidian--select-fitting (quotidian--echo-max-lines)
                                                   (quotidian--echo-width))))
      (let ((message-log-max nil))
        (message "%s" filled))))
  nil)

(defun quotidian--select-fitting (max-lines width)
  "Return a formatted quotation of at most MAX-LINES lines, filled at WIDTH.
Return nil if `quotidian--echo-attempts' random draws all give a longer
quotation, or if the corpus is empty.  The return value is the filled
string, not the quotation itself."
  (cl-loop repeat quotidian--echo-attempts
           for quote = (quotidian--select)
           while quote
           for filled = (quotidian--fill (quotidian-format quote) width)
           when (<= (quotidian--line-count filled width) max-lines)
           return filled))

(defun quotidian--select ()
  "Return a quotation chosen at random, or nil if the corpus is empty."
  (when-let* ((corpus (quotidian--ensure-corpus)))
    (seq-random-elt corpus)))

(defun quotidian--fill (string width)
  "Return STRING with every line word-wrapped to WIDTH columns.
Lines are filled one at a time, so the line breaks already in STRING
survive."
  (mapconcat (lambda (line) (quotidian--fill-line line width))
             (split-string string "\n")
             "\n"))

(defun quotidian--fill-line (line width)
  "Return LINE word-wrapped to WIDTH columns.
Filling ignores the user's customized fill variables, so the result does
not depend on the Emacs configuration it runs in."
  (with-temp-buffer
    (insert line)
    (let ((fill-column width)
          (fill-prefix (quotidian--factory-value 'fill-prefix))
          (left-margin (quotidian--factory-value 'left-margin))
          (adaptive-fill-mode (quotidian--factory-value 'adaptive-fill-mode))
          (adaptive-fill-regexp (quotidian--factory-value 'adaptive-fill-regexp))
          (adaptive-fill-function (quotidian--factory-value 'adaptive-fill-function)))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun quotidian--factory-value (symbol)
  "Return the factory-default value of the customizable variable SYMBOL."
  (eval (car (get symbol 'standard-value))))

(defun quotidian--line-count (string width)
  "Return the number of display lines STRING occupies at WIDTH columns.
A line of STRING wider than WIDTH counts as more than one line, the way
`message' wraps it in the echo area."
  (cl-loop for line in (split-string string "\n")
           sum (max 1 (ceiling (string-width line) width))))

(defun quotidian--echo-max-lines ()
  "Return the lesser of `quotidian-echo-max-lines' and what Emacs will show.
The echo area cannot grow past what `resize-mini-windows' and
`max-mini-window-height' allow, no matter how tall
`quotidian-echo-max-lines' is set."
  (min quotidian-echo-max-lines (quotidian--max-echo-lines)))

(defun quotidian--max-echo-lines ()
  "Return the greatest number of lines Emacs will grow the echo area to."
  (if (not resize-mini-windows)
      1
    (let ((frame (window-frame (minibuffer-window))))
      (max 1 (if (floatp max-mini-window-height)
                 (floor (* max-mini-window-height (frame-height frame)))
               max-mini-window-height)))))

(defun quotidian--echo-width ()
  "Return the usable width, in columns, of the echo area."
  (max 1 (1- (window-body-width (minibuffer-window)))))

(provide 'quotidian)
;;; quotidian.el ends here
