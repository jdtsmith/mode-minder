;;; mode-minder.el --- List all major and minor modes  -*- lexical-binding: t -*-
;; Copyright (C) 2022 JDS
;; Author: J.D. Smith <jdtsmith@gmail.com>
;; URL: https://github.com/jdtsmith/mode-minder
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.2
;; Keywords: modes

;; Usage: M-x mode-minder
;;; Code:
(require 'help-fns)
(require 'button)
(eval-when-compile
  (require 'cl-lib)
  (require 'seq))

(defvar mode-minder-ht (make-hash-table :test 'eq))
(defvar mode-minder-alias-ht (make-hash-table :test 'eq))
(defconst mode-minder-pad 38)

(defun mode-minder--describe-function (func)
  (let (help-xref-following) ; do not open locally
    (describe-function func)))

(define-button-type 'mode-minder-help-function
  :supertype 'help-xref
  'help-function 'mode-minder--describe-function
  'help-echo (purecopy "mouse-2, RET: describe this function"))

(defun mode-minder--sym-sort (a b)
  (string-lessp (downcase (symbol-name a)) (downcase (symbol-name b))))

(defconst mode-minder--builtin-dir
  (file-name-directory (directory-file-name data-directory)))

(defun mode-minder--map-tree (mode children depth)
  (let* ((mstr (concat (make-string (* 2 depth) ?\s)
		       (if (= (% depth 2) 0) "•" "-") " "
		       (symbol-name mode)))
	 (sfile (symbol-file mode))
	 (aliases (gethash mode mode-minder-alias-ht))
	 (tag
	  (if sfile
	      (concat
	       (cond
		((file-in-directory-p sfile package-user-dir)
		 "[P]")
		((not (or (not (eq (aref sfile 0) ?/)) ;relative filename
			  (file-in-directory-p sfile mode-minder--builtin-dir)))
		 "[O]"))
	       (if aliases
		   (concat " (" (string-join (mapcar #'symbol-name aliases) ", ") ")")))))
	 (pad (max 0 (- mode-minder-pad (length mstr) (length tag)))))
    (princ mstr)
    (with-current-buffer standard-output
      (save-excursion
	(re-search-backward " \\([^ ]+\\)")
	(help-xref-button 1 'mode-minder-help-function mode))
      (if tag (princ tag))
      (princ (make-string pad ?\s))
      (when-let ((doc (documentation mode))
		 (doc (substring doc 0 (string-match (rx (or (group ?. space) ?\n)) doc)))
		 (beg (point)))
	(princ (concat " " doc))
	(add-text-properties beg (point) '(font-lock-face custom-documentation))))
    (princ "\n")
    (mapc (lambda (c) (mode-minder--map-tree c (gethash c mode-minder-ht) (1+ depth)))
	  (sort children #'mode-minder--sym-sort))))


(defun mode-minder--count (syms)
  (+ (length syms)
     (apply #'+ (mapcar (lambda (x)
			  (if-let ((children (gethash x mode-minder-ht)))
			      (mode-minder--count children)
			    0))
			syms))))

(defvar mode-minder--warmed nil)
(defun mode-minder ()
  "Show heirarchy of all major and minor modes."
  (interactive)
  (clrhash mode-minder-ht) (clrhash mode-minder-alias-ht)
  (unless mode-minder--warmed
    (message "Mode-Minder: Loading all mode libraries...")
    (mapatoms
     (lambda (x)
       (when (and (commandp x) (string-suffix-p "-mode" (symbol-name x)))
	 (when-let ((file (symbol-file x))
		    (package (file-name-base (symbol-file x))))
	   (condition-case nil
	       (if-let ((psym (intern-soft package)))
		   (require psym)
		 (load package))
	     (error nil))))))
    (setq mode-minder--warmed t)
    (message "Mode-Minder: Loading all mode libraries...done"))
  (with-help-window "*Mode-Minder*"
    (let (roots minors)
      (mapatoms
       (lambda (x)
	 (when (and (commandp x) (string-suffix-p "-mode" (symbol-name x)))
	   (seq-let (_ _ aliased real-def) (help-fns--analyze-function x)
	     (if aliased (push x (gethash real-def mode-minder-alias-ht))
	       (if (or (memq x minor-mode-list)
		       (alist-get x minor-mode-alist)
		       (string-suffix-p "minor-mode" (symbol-name x))) ; trust the name
		   (push x minors)
		 (if-let ((parent (get x 'derived-mode-parent)))
		     (push x (gethash parent mode-minder-ht))
		   (push x roots))))))))
      (cl-loop for list in (append (mapcar #'cdr (seq-group-by
						  (lambda (x) (null (gethash x mode-minder-ht)))
						  roots))
				   (list minors))
	       for heading in '("Major Mode Hierarchies" "\nStandalone Modes" "\nMinor Modes")
	       do
	       (princ (format "%s (%d):" heading (mode-minder--count list)))
	       (with-current-buffer standard-output
		 (add-text-properties (line-beginning-position) (point) '(font-lock-face info-title-2)))
	       (princ "\n\n")
	       (mapc (lambda (x) (mode-minder--map-tree x (gethash x mode-minder-ht) 0))
		     (sort list #'mode-minder--sym-sort))))))

(provide 'mode-minder)
;;; mode-minder.el ends here.
