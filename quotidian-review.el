;;; quotidian-review.el --- Review buffer for quotidian -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini <pablo@stafforini.com>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>
;; URL: https://github.com/benthamite/quotidian
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience

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

;; A buffer for reading quotations one at a time, one to a screen.  The
;; quotations are the ones `quotidian' reads from its corpus files.

;;; Code:

(require 'quotidian)

(defcustom quotidian-review-fill-column 64
  "Width, in columns, at which quotations are wrapped in the review buffer."
  :type 'natnum
  :group 'quotidian)

(defvar-keymap quotidian-review-mode-map
  :doc "Keymap for `quotidian-review-mode'."
  "n" #'quotidian-review-next
  "SPC" #'quotidian-review-next
  "p" #'quotidian-review-previous
  "w" #'quotidian-review-copy
  "q" #'quotidian-review-quit)

(defvar-local quotidian-review--history nil
  "Quotations shown in this buffer, oldest first.")

(defvar-local quotidian-review--index 0
  "Position in `quotidian-review--history' of the quotation now shown.")

(defvar-local quotidian-review--window-configuration nil
  "Window configuration to restore when the review ends.")

;;;###autoload
(defun quotidian ()
  "Read quotations one at a time, one to a screen."
  (interactive)
  (unless (quotidian--ensure-corpus)
    (quotidian-review--no-quotations))
  (let ((configuration (current-window-configuration)))
    (switch-to-buffer (get-buffer-create "*quotidian*"))
    (unless (derived-mode-p 'quotidian-review-mode)
      (quotidian-review-mode)
      (setq quotidian-review--window-configuration configuration))
    (delete-other-windows)
    (quotidian-review--push (quotidian-review--draw))
    (quotidian-review--render)))

(define-derived-mode quotidian-review-mode special-mode "Quotidian"
  "Major mode for reading quotations one at a time."
  (setq-local cursor-type nil)
  (setq-local revert-buffer-function #'quotidian-review--revert)
  (setq-local mode-line-format
              (propertize " n next   p previous   w copy   g redraw   q quit"
                          'face 'shadow))
  (add-hook 'window-size-change-functions #'quotidian-review--resize nil t)
  (add-hook 'kill-buffer-hook #'quotidian-review--restore-windows nil t))

(defun quotidian-review--revert (&rest _)
  "Draw the current quotation again, whatever arguments are passed.
`special-mode' binds \\`g' to `revert-buffer', which has no file to
revert here.  Drawing the quotation again is what reverting a review
buffer can sensibly mean, and it keeps the key from signalling."
  (quotidian-review--render))

(defun quotidian-review-next ()
  "Show the next quotation.
Draw a new one at random when the end of the history is reached;
otherwise step forward through quotations already seen."
  (interactive)
  (if (< quotidian-review--index (1- (length quotidian-review--history)))
      (setq quotidian-review--index (1+ quotidian-review--index))
    (quotidian-review--push (quotidian-review--draw)))
  (quotidian-review--render))

(defun quotidian-review-previous ()
  "Show the quotation seen before this one."
  (interactive)
  (if (zerop quotidian-review--index)
      (message "No earlier quotation")
    (setq quotidian-review--index (1- quotidian-review--index))
    (quotidian-review--render)))

(defun quotidian-review-copy (&optional text-only)
  "Copy the quotation and its attribution to the kill ring.
With a prefix argument, TEXT-ONLY, copy the text of the quotation
without the attribution."
  (interactive "P")
  (let ((quote (quotidian-review--current)))
    (kill-new (substring-no-properties
               (if text-only (plist-get quote :text) (quotidian-format quote))))
    (message "Copied")))

(defun quotidian-review-quit ()
  "Leave the review and put the windows back as they were."
  (interactive)
  (unless (derived-mode-p 'quotidian-review-mode)
    (user-error "Not in a quotidian review buffer"))
  (kill-buffer))

(defun quotidian-review--draw ()
  "Return a quotation chosen at random.
Signal `user-error' if the corpus is empty."
  (or (quotidian--select) (quotidian-review--no-quotations)))

(defun quotidian-review--no-quotations ()
  "Signal a `user-error' reporting that the corpus is empty.
The check is made both before the review buffer is shown and before
each draw, so that an empty corpus never leaves an empty buffer on
screen."
  (user-error "No quotations found; check `quotidian-corpus-files'"))

(defun quotidian-review--push (quote)
  "Add QUOTE to the end of the history and make it the current one."
  (setq quotidian-review--history
        (append quotidian-review--history (list quote)))
  (setq quotidian-review--index (1- (length quotidian-review--history))))

(defun quotidian-review--current ()
  "Return the quotation now shown."
  (nth quotidian-review--index quotidian-review--history))

(defun quotidian-review--render (&optional window)
  "Draw the current quotation in the middle of WINDOW.
WINDOW defaults to the selected window."
  (let* ((window (or window (selected-window)))
         (inhibit-read-only t)
         (width (window-body-width window))
         (lines (split-string (quotidian--fill
                               (quotidian-format (quotidian-review--current))
                               quotidian-review-fill-column)
                              "\n")))
    (erase-buffer)
    (insert (make-string
             (quotidian-review--top-padding (length lines) window) ?\n))
    (insert (mapconcat (lambda (line) (quotidian-review--centre line width))
                       lines "\n"))
    (goto-char (point-min))))

(defun quotidian-review--top-padding (height window)
  "Return the number of blank lines that centre HEIGHT lines in WINDOW."
  (max 0 (/ (- (window-body-height window) height) 2)))

(defun quotidian-review--centre (line width)
  "Return LINE indented to centre it in a window WIDTH columns wide."
  (concat (make-string (max 0 (/ (- width (string-width line)) 2)) ?\s)
          line))

(defun quotidian-review--restore-windows ()
  "Restore the window configuration saved when the review began.
Do nothing unless the buffer being killed is on screen, and on the frame
the review began on.  Killing a review buffer that is not displayed,
from the buffer list or from a cleanup command, would otherwise throw
away the layout the user is working in.  Killing one displayed on a
second frame would do the same to the first frame, and select it into
the bargain: `set-window-configuration' selects the frame of the
configuration it restores."
  (when (and quotidian-review--window-configuration
             (get-buffer-window (current-buffer))
             (eq (window-configuration-frame
                  quotidian-review--window-configuration)
                 (selected-frame)))
    (set-window-configuration quotidian-review--window-configuration)))

(defun quotidian-review--resize (window)
  "Draw the quotation again after WINDOW is resized."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'quotidian-review-mode)
        (save-match-data (quotidian-review--render window))))))

(provide 'quotidian-review)
;;; quotidian-review.el ends here
