;;; go-snaps.el --- managing go snapshot testing -*- lexical-binding: t; -*-
;;;
;; Author: Oliver Pauffley <mrpauffley@gmail.com>
;; Maintainer: Oliver Pauffley <mrpauffley@gmail.com>
;; Created: October 28, 2025
;; Modified: October 28, 2025
;; Version: 0.0.1
;; Keywords: testing programming
;; Homepage: https://github.com/oliverpauffley/go-snaps.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; A set of functionality for managing go snapshot tests within emacs.
;;
;;
;;; Code:

(defvar go-snaps--snap-file-extension "snap"
  "The default snapshot file name extension.")

(defun go-snaps--snap-file-regex ()
  "The regexp pattern to use for snapshot files.
Is based of the value provided in `go-snaps--snap-file-extension'"
  (format "\\.%s$" go-snaps--snap-file-extension))

(defun go-snaps--find-test-content (dir file-name)
  "For the FILE-NAME find the snapshot test data from within the current DIR recursively.
Will return nil if no snapshot file exists."
  (let* ((filen-base (file-name-sans-extension (file-name-base file-name)))
	 ;; find all snapshot files in the directory
	 (snapshot-files (directory-files-recursively dir (go-snaps--snap-file-regex)))
	 ;; find the file
	 (snapshot-file (seq-find (lambda (file) (equal filen-base (file-name-base file))) snapshot-files))
	 ;; get the file as a string
	 (snapshot-file-str (go-snaps--buffer-whole-string (find-file-noselect snapshot-file)))
	 ;; convert the file into an alist like ("testName - number" . "output")
	 (tests (go-snaps--snap-content-parse snapshot-file-str)))
    (message "%s" snapshot-file-str)))

(defun go-snaps--buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
	(widen)
	(buffer-substring-no-properties (point-min) (point-max))))))

(defun go-snaps--snap-content-parse (buf-str)
  "Convert the snapshot BUF-STR into an alist of test names and snapshot output."
  (let* ((header-re "\\[\\([^]]+\\)\\]") 
         (pos 0)
         result)
    (while (string-match header-re buf-str
			 (let* ((key (match-string 1 buf-str))
				(start (match-end 0))
				(next (string-match header-re buf-str start))
				(end (or next (length buf-str)))
				(content (string-trim
					  (substring buf-str start end))))
			   (push (cons key content) result)
			   (setq pos end)))
      (nreverse result))))

(defun go-snaps-overlay-file ()
  "For the current buffer, if it is a test file containing `go-snaps-match-regexp' functions, annotate the
buffer with overlays showing the test outputs."
  (interactive)
  (let* ((name (buffer-file-name))
	 (dir (file-name-directory name))
	 (test-for-file (go-snaps--find-test-content dir name))
	 (seen-funcs))
    (save-excursion
      ;; go to the start of the file and find the nearest function
      (goto-char (point-min))
      (go-goto-function t)
      (go-goto-function-name t)
      (let* ((func-name (symbol-name (symbol-at-point)))
	     ;; get the test output for this function.
	     (func-snaps (go-snaps--alist-filter-by-prefix test-for-file func-name)))
	(if (not (member func-name seens-funcs))
	    (progn
	      (go-snaps--overlay-func func-snaps)
	      (push func-name seen-funcs))))
      (message "%s" test-for-file))))

(defun go-snaps--overlay-func (func-snaps)
  "Traverse the function the point is in and annotate snapshots.
and for each `MatchSnapshot' overlay the text with the snapshot from FUNC-SNAPS."
  ;; TODO get this regexp to work if the import is aliased.
  (let ((end (save-excursion
	       (re-search-forward (rx bol "}") nil t)))
	(snap-re (rx (minimal-match (seq "snaps.MatchSnapshot"
					 "("
					 (0+ anything)
					 eol))))
	(snaps func-snaps))
    ;; figure out a way to indent the comment.
    ;; maybe by actually just adding the indent manually?
    (while (re-search-forward snap-re end t)
      (let* ((fi (match-end 0))
	     (overlay (make-overlay fi (1+ fi)))
	     (snap (format "\n %s" (car snaps)))
	     (spec '(space :align-to 5)))
	(overlay-put overlay 'before-string
		     (propertize snap 'face 'shadow))
	(overlay-put overlay 'go-snaps t)
	(overlay-put overlay 'evaporate t)
	(setq snaps (cdr snaps)))))
  (forward-line))

(defun go-snaps--alist-filter-by-prefix (alist prefix)
  "Return entries in ALIST whose key begins with PREFIX,
sorted by the trailing number in the key."
  (let (matches)
    ;; collect matches
    (dolist (entry alist)
      (let ((key (car entry)))
        (when (string-match (format "^%s[[:space:]]*-?[[:space:]]*\\([0-9]+\\)$"
                                    (regexp-quote prefix))
                            key)
          (push (cons (string-to-number (match-string 1 key))
                      entry)
                matches))))

    ;; sort by extracted number
    (setq matches (sort matches (lambda (a b) (< (car a) (car b)))))

    ;; return only the original alist entries (discard numeric key)
    (mapcar #'cdr matches)))

;; walk the file and every time we see a test function
;; store the function name
;; get all test outputs that match the fuction name
;; every time we see a `MatchSnapshot' pull the first match from the alist
;; use an overlay to put in on the line below

(provide 'go-snaps)
;;; go-snaps.el ends here
