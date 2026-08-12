;;; quotidian-test.el --- Tests for quotidian -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `quotidian'.

;;; Code:

(require 'ert)
(require 'quotidian)

(defvar quotidian-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun quotidian-test-fixture (name)
  "Return the absolute path of fixture NAME."
  (expand-file-name (concat "fixtures/" name) quotidian-test-directory))

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

(provide 'quotidian-test)
;;; quotidian-test.el ends here
