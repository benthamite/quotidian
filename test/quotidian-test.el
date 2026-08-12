;;; quotidian-test.el --- Tests for quotidian -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `quotidian'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'quotidian)

(defvar quotidian-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun quotidian-test-fixture (name)
  "Return the absolute path of fixture NAME."
  (expand-file-name (concat "fixtures/" name) quotidian-test-directory))

(defun quotidian-test-capture-message ()
  "Call `quotidian-echo' and return the string it passed to `message'."
  (let (captured)
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (setq captured (apply #'format format args)))))
      (quotidian-echo))
    captured))

(defmacro quotidian-test-with-corpus (files &rest body)
  "Evaluate BODY with `quotidian-corpus-files' bound to FILES.
The corpus cache is cleared before and after, so tests do not leak
state into each other."
  (declare (indent 1))
  `(let ((quotidian-corpus-files ,files)
         (quotidian--corpus 'unset))
     ,@body))

(ert-deftest quotidian-test-reads-valid-file ()
  (quotidian-test-with-corpus (list (quotidian-test-fixture "valid.json"))
    (let ((corpus (quotidian--ensure-corpus)))
      (should (= (length corpus) 2))
      (should (equal (plist-get (car corpus) :text) "A first quotation."))
      (should (equal (plist-get (car corpus) :author) "First Author"))
      (should (equal (plist-get (car corpus) :source) "First Source"))
      (should (equal (plist-get (car corpus) :date) "1900")))))

(ert-deftest quotidian-test-optional-fields-are-nil ()
  (quotidian-test-with-corpus (list (quotidian-test-fixture "valid.json"))
    (let ((second (nth 1 (quotidian--ensure-corpus))))
      (should (null (plist-get second :source)))
      (should (null (plist-get second :date))))))

(ert-deftest quotidian-test-missing-file-is-skipped ()
  (quotidian-test-with-corpus (list (quotidian-test-fixture "does-not-exist.json"))
    (should (null (quotidian--ensure-corpus)))))

(ert-deftest quotidian-test-malformed-file-is-skipped ()
  (quotidian-test-with-corpus (list (quotidian-test-fixture "malformed.json"))
    (should (null (quotidian--ensure-corpus)))))

(ert-deftest quotidian-test-invalid-entries-are-dropped ()
  (quotidian-test-with-corpus (list (quotidian-test-fixture "invalid-entries.json"))
    (let ((corpus (quotidian--ensure-corpus)))
      (should (= (length corpus) 1))
      (should (equal (plist-get (car corpus) :author) "Valid Author")))))

(ert-deftest quotidian-test-files-are-concatenated ()
  (quotidian-test-with-corpus (list (quotidian-test-fixture "valid.json")
                                    (quotidian-test-fixture "invalid-entries.json"))
    (should (= (length (quotidian--ensure-corpus)) 3))))

(ert-deftest quotidian-test-corpus-is-cached ()
  (quotidian-test-with-corpus (list (quotidian-test-fixture "valid.json"))
    (quotidian--ensure-corpus)
    (let ((quotidian-corpus-files nil))
      (should (= (length (quotidian--ensure-corpus)) 2)))))

(ert-deftest quotidian-test-reload-rereads-files ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"One.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (should (= (length (quotidian--ensure-corpus)) 1))
          (with-temp-file file
            (insert "[{\"text\": \"One.\", \"author\": \"A\"},"
                    " {\"text\": \"Two.\", \"author\": \"B\"}]"))
          (quotidian-reload)
          (should (= (length (quotidian--ensure-corpus)) 2)))
      (delete-file file))))

(ert-deftest quotidian-test-format-with-all-fields ()
  (should (equal (substring-no-properties
                  (quotidian-format '(:text "Text." :author "Author"
                                      :date "1900" :source "Source")))
                 "Text.\n— Author, 1900\n  Source")))

(ert-deftest quotidian-test-format-without-source ()
  (should (equal (substring-no-properties
                  (quotidian-format '(:text "Text." :author "Author" :date "1900")))
                 "Text.\n— Author, 1900")))

(ert-deftest quotidian-test-format-without-date ()
  (should (equal (substring-no-properties
                  (quotidian-format '(:text "Text." :author "Author" :source "Source")))
                 "Text.\n— Author\n  Source")))

(ert-deftest quotidian-test-format-with-author-only ()
  (should (equal (substring-no-properties
                  (quotidian-format '(:text "Text." :author "Author")))
                 "Text.\n— Author")))

(ert-deftest quotidian-test-format-applies-faces ()
  (let ((formatted (quotidian-format '(:text "Text." :author "Author"))))
    (should (eq (get-text-property 0 'face formatted) 'quotidian-quote))
    (should (eq (get-text-property 6 'face formatted) 'quotidian-author))))

(ert-deftest quotidian-test-fill-wraps-at-width ()
  (should (equal (quotidian--fill "one two three four five" 9)
                 "one two\nthree\nfour five")))

(ert-deftest quotidian-test-fill-preserves-existing-line-breaks ()
  (should (equal (quotidian--fill "short\nalso short" 40)
                 "short\nalso short")))

(ert-deftest quotidian-test-fill-does-not-split-words ()
  (should (equal (split-string (quotidian--fill "aaa bbb ccc ddd eee" 10))
                 (list "aaa" "bbb" "ccc" "ddd" "eee"))))

(ert-deftest quotidian-test-fill-keeps-source-indented-and-faced ()
  (let* ((formatted (quotidian-format
                      '(:text "Short." :author "Author"
                        :source "A Long Source Title That Wraps Around")))
         (filled (quotidian--fill formatted 20)))
    (should (equal (split-string filled "\n")
                   (list "Short." "— Author" "  A Long Source"
                         "  Title That Wraps" "  Around")))
    (should (seq-every-p (lambda (line) (eq (get-text-property 0 'face line)
                                            'quotidian-source))
                         (last (split-string filled "\n") 3)))))

(ert-deftest quotidian-test-line-count ()
  (should (= (quotidian--line-count "one" 40) 1))
  (should (= (quotidian--line-count "one\ntwo" 40) 2))
  (should (= (quotidian--line-count "one\ntwo\nthree" 40) 3))
  (should (= (quotidian--line-count (make-string 50 ?a) 20) 3)))

(ert-deftest quotidian-test-select-returns-corpus-member ()
  (quotidian-test-with-corpus (list (quotidian-test-fixture "valid.json"))
    (should (member (quotidian--select) (quotidian--ensure-corpus)))))

(ert-deftest quotidian-test-select-on-empty-corpus ()
  (quotidian-test-with-corpus nil
    (should (null (quotidian--select)))))

(ert-deftest quotidian-test-select-fitting-returns-filled-string ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Short.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (should (equal (substring-no-properties (quotidian--select-fitting 4 40))
                         "Short.\n— A")))
      (delete-file file))))

(ert-deftest quotidian-test-select-fitting-on-empty-corpus ()
  (quotidian-test-with-corpus nil
    (should (null (quotidian--select-fitting 4 40)))))

(ert-deftest quotidian-test-select-fitting-rejects-long-quotes ()
  (let ((file (make-temp-file
               "quotidian-test" nil ".json"
               (concat "[{\"text\": \"" (string-join (make-list 60 "word") " ")
                       "\", \"author\": \"A\"}]"))))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (should (null (quotidian--select-fitting 2 40))))
      (delete-file file))))

(ert-deftest quotidian-test-echo-displays-a-quotation ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Short.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (should (string-match-p "Short\\." (quotidian-test-capture-message))))
      (delete-file file))))

(ert-deftest quotidian-test-echo-on-empty-corpus-is-silent ()
  (quotidian-test-with-corpus nil
    (should (null (quotidian-test-capture-message)))))

(ert-deftest quotidian-test-echo-with-no-fitting-quote-is-silent ()
  (let ((file (make-temp-file
               "quotidian-test" nil ".json"
               (concat "[{\"text\": \"" (string-join (make-list 200 "word") " ")
                       "\", \"author\": \"A\"}]"))))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (let ((quotidian-echo-max-lines 2))
            (should (null (quotidian-test-capture-message)))))
      (delete-file file))))

(ert-deftest quotidian-test-echo-does-not-log ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Unlogged.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (let ((before (with-current-buffer (get-buffer-create "*Messages*")
                          (buffer-string))))
            (quotidian-echo)
            (with-current-buffer "*Messages*"
              (should (equal (buffer-string) before)))))
      (delete-file file))))

(ert-deftest quotidian-test-echo-does-nothing-with-active-minibuffer ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Short.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (cl-letf (((symbol-function 'active-minibuffer-window)
                     (lambda () t)))
            (should (null (quotidian-test-capture-message)))))
      (delete-file file))))

(ert-deftest quotidian-test-echo-returns-nil ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Short.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (should (null (quotidian-echo))))
      (delete-file file))))

(ert-deftest quotidian-test-echo-does-not-error-on-narrow-frame ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Short.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 1)))
            (should (null (quotidian-echo)))))
      (delete-file file))))

(ert-deftest quotidian-test-invalid-optional-fields-are-dropped ()
  (quotidian-test-with-corpus
      (list (quotidian-test-fixture "invalid-optional-fields.json"))
    (let ((corpus (quotidian--ensure-corpus)))
      (should (= (length corpus) 1))
      (should (equal (plist-get (car corpus) :author) "Valid Author")))))

(ert-deftest quotidian-test-reload-reports-skipped-and-dropped ()
  (let (captured
        (files (list (quotidian-test-fixture "does-not-exist.json")
                     (quotidian-test-fixture "invalid-entries.json"))))
    (quotidian-test-with-corpus files
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (setq captured (apply #'format format args)))))
        (quotidian-reload)))
    (should (string-match-p "1 files skipped" captured))
    (should (string-match-p "3 entries dropped" captured))))

(ert-deftest quotidian-test-echo-max-lines-clamped-by-resize-mini-windows ()
  (let ((resize-mini-windows nil)
        (quotidian-echo-max-lines 4))
    (should (= (quotidian--echo-max-lines) 1))))

(ert-deftest quotidian-test-echo-max-lines-clamped-by-max-mini-window-height ()
  (let ((resize-mini-windows t)
        (max-mini-window-height 0.2)
        (quotidian-echo-max-lines 10))
    (cl-letf (((symbol-function 'frame-height) (lambda (&rest _) 20)))
      (should (= (quotidian--echo-max-lines) 4)))))

(ert-deftest quotidian-test-warn-logs-without-popping ()
  (when (get-buffer "*Warnings*") (kill-buffer "*Warnings*"))
  (quotidian--warn "Probe message for warn logging test")
  (should (get-buffer "*Warnings*"))
  (should (string-match-p "Probe message for warn logging test"
                          (with-current-buffer "*Warnings*" (buffer-string)))))

(require 'quotidian-review)

(defmacro quotidian-test-with-review (text &rest body)
  "Open a review buffer on a one-quotation corpus of TEXT and run BODY.
The buffer is killed afterwards.  A single-quotation corpus makes
selection deterministic, so the tests need no control over randomness."
  (declare (indent 1))
  `(let ((file (make-temp-file "quotidian-test" nil ".json"
                               (format "[{\"text\": %S, \"author\": \"A\"}]" ,text))))
     (unwind-protect
         (quotidian-test-with-corpus (list file)
           (save-window-excursion
             (quotidian)
             (unwind-protect (progn ,@body)
               (kill-buffer "*quotidian*"))))
       (delete-file file))))

(ert-deftest quotidian-test-review-shows-the-quotation ()
  (quotidian-test-with-review "Displayed."
    (should (string-match-p "Displayed\\." (buffer-string)))
    (should (string-match-p "— A" (buffer-string)))))

(ert-deftest quotidian-test-review-buffer-is-read-only ()
  (quotidian-test-with-review "Read only."
    (should buffer-read-only)
    (should (eq major-mode 'quotidian-review-mode))))

(ert-deftest quotidian-test-review-centres-the-text ()
  (quotidian-test-with-review "Centred."
    (goto-char (point-min))
    (should (re-search-forward "^ +Centred\\." nil t))))

(ert-deftest quotidian-test-review-signals-on-empty-corpus ()
  (quotidian-test-with-corpus nil
    (should-error (quotidian) :type 'user-error)))

(defun quotidian-test-window-buffers ()
  "Return the names of the buffers shown in the windows of this frame."
  (mapcar (lambda (window) (buffer-name (window-buffer window)))
          (window-list)))

(ert-deftest quotidian-test-review-quit-restores-windows ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Quit.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (save-window-excursion
            (delete-other-windows)
            (split-window)
            (let ((before (quotidian-test-window-buffers)))
              (quotidian)
              (quotidian-review-quit)
              (should (equal (quotidian-test-window-buffers) before))
              (should (null (get-buffer "*quotidian*"))))))
      (delete-file file))))

(ert-deftest quotidian-test-review-second-call-does-not-clobber-configuration ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"One.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (save-window-excursion
            (delete-other-windows)
            (split-window)
            (let ((before (quotidian-test-window-buffers)))
              (quotidian)
              (quotidian-review-next)
              (quotidian)
              (quotidian-review-quit)
              (should (equal (quotidian-test-window-buffers) before))
              (should (null (get-buffer "*quotidian*"))))))
      (delete-file file))))

(ert-deftest quotidian-test-review-kill-buffer-restores-windows ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Killed.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (save-window-excursion
            (delete-other-windows)
            (split-window)
            (let ((before (quotidian-test-window-buffers)))
              (quotidian)
              (kill-buffer "*quotidian*")
              (should (equal (quotidian-test-window-buffers) before)))))
      (delete-file file))))

(ert-deftest quotidian-test-review-next-signals-on-empty-corpus ()
  (let ((file (make-temp-file "quotidian-test" nil ".json"
                              "[{\"text\": \"Only.\", \"author\": \"A\"}]")))
    (unwind-protect
        (quotidian-test-with-corpus (list file)
          (save-window-excursion
            (quotidian)
            (unwind-protect
                (progn
                  (let ((quotidian-corpus-files nil)
                        (quotidian--corpus 'unset))
                    (should-error (quotidian-review-next) :type 'user-error))
                  (should (equal (plist-get (quotidian-review--current) :text)
                                 "Only.")))
              (kill-buffer "*quotidian*"))))
      (delete-file file))))

(provide 'quotidian-test)
;;; quotidian-test.el ends here
