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
;; A set of functionality for managing go snapshot tests within Emacs.
;;
;;
;;; Code:

(require 'go-ts-mode)

(defvar go-snaps--snap-file-extension "snap"
  "The default snapshot file name extension.")

(defun go-snaps--snap-file-regex ()
  "The regexp pattern to use for snapshot files.
Is based off the value provided in `go-snaps--snap-file-extension'"
  (format "\\.%s$" go-snaps--snap-file-extension))

(defun go-snaps--find-test-content (dir file-name)
  "For the FILE-NAME find the snapshot test data from the DIR recursively.
Will return nil if no snapshot file exists."
  (let* ((filen-base (file-name-sans-extension (file-name-base file-name)))
	     ;; find all snapshot files in the directory
	     (snapshot-files (directory-files-recursively dir (go-snaps--snap-file-regex)))
	     ;; find the file
	     (snapshot-file (seq-find (lambda (file) (equal filen-base (file-name-base file))) snapshot-files))
	     ;; get the file as a string
	     (snapshot-file-str (go-snaps--buffer-whole-string (find-file-noselect snapshot-file))))
    ;; convert the file into an alist like ("testName - number" . "output")
    (go-snaps--snap-content-parse snapshot-file-str)))

(defun go-snaps--buffer-whole-string (buffer)
  "Get the entire BUFFER as a string."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
	    (widen)
	    (buffer-substring-no-properties (point-min) (point-max))))))

(defun go-snaps--snap-content-parse (buf-str)
  "Convert the snapshot BUF-STR into an alist of test names and snapshot output."
  (let* ((header-re "\n\\[\\([^]]+\\)\\]$")
         (pos 0)
         result)
    (while (string-match header-re buf-str pos)
	  (let* ((key (match-string 1 buf-str))
			 (start (match-end 0))
			 (next (string-match header-re buf-str start))
			 (end (or next (length buf-str)))
			 (content (string-trim (string-trim
					                (substring buf-str start end)) nil "\n---")))
		(push (cons key content) result)
        (setq pos end)))
    (nreverse result)))

(defun go-snaps-overlay-file ()
  "Annotate the current buffer with test overlays.
Must be a test file and must contain functions matching
 the `go-snaps-match-regexp'."
  (interactive)
  (let* ((name (buffer-file-name))
	     (dir (file-name-directory name))
	     (test-for-file (go-snaps--find-test-content dir name))
         (root-node (treesit-buffer-root-node))
         (query '((function_declaration name: (identifier) @func-name)))
         (matches (treesit-query-capture root-node query)))
    (save-excursion
      (dolist (match matches)
        (let* ((node (cdr match))
               (func-name (treesit-node-text node))
               (start-pos (treesit-node-start node))
               ;; get the specific test output for this function
	           (func-snaps (go-snaps--alist-filter-by-prefix test-for-file func-name)))
          (goto-char start-pos)
	      (go-snaps--overlay-func func-snaps))))))

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
    ;; TODO figure out a way to indent the comment.
    ;; maybe by actually just adding the indent manually?
    (while (re-search-forward snap-re end t)
      (let* ((fi (match-end 0))
	         (overlay (make-overlay fi (1+ fi)))
	         (snap (format "\n %s" (cdar snaps)))
	         (spec '(space :align-to 5)))
	    (overlay-put overlay 'before-string
	    	         (propertize snap 'face 'shadow))
	    ;; (overlay-put overlay 'display snap)
	    (overlay-put overlay 'go-snaps t)
	    (overlay-put overlay 'evaporate t)
	    (setq snaps (cdr snaps)))))
  (search-forward "^\\}")
  (forward-line 3))

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
