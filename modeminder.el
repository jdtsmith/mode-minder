;;; mm.el --- Print graph of major and minor modes  -*- lexical-binding: t -*-
;; Usage: M-x mm-show-modes
;; J.D. Smith, 2022
(require 'button)
(eval-when-compile
  (require 'cl-lib)
  (require 'seq))

(defvar mm-ht (make-hash-table :test 'eq))
(defconst mm-pad 36)

(defun mm-describe-function (func)
  (let (help-xref-following) ; do not open locally
    (describe-function func)))

(define-button-type 'mm-help-function
  :supertype 'help-xref
  'help-function 'mm-describe-function
  'help-echo (purecopy "mouse-2, RET: describe this function"))

(defun mm-sym-sort (a b)
  (string-lessp (downcase (symbol-name a)) (downcase (symbol-name b))))

(defun mm-map-tree (mode children depth)
  (let* ((mstr (concat (make-string (* 2 depth) ?\s)
		       (if (= (% depth 2) 0) "•" "-") " "
		       (symbol-name mode)))
	 (pad (max 0 (- mm-pad (length mstr)))))
    (princ mstr)
    (with-current-buffer standard-output
      (save-excursion
	(re-search-backward " \\([^ ]+\\)")
	(help-xref-button 1 'mm-help-function mode)))
    (when (file-in-directory-p (symbol-file mode) package-user-dir)
      (princ "(P)")
      (setq pad (max 0 (- pad 3))))
    (princ (make-string pad ?\s))
    (if-let* ((doc (documentation mode))
	      (doc (substring doc 0 (string-match (rx (or (group ?. space) ?\n)) doc))))
	(princ (concat " " doc)))
    (princ "\n")
    (mapc (lambda (c) (mm-map-tree c (gethash c mm-ht) (1+ depth)))
	  (sort children #'mm-sym-sort))))

(defun mm-show-modes ()
  "Show heirarchy of major and minor modes"
  (interactive)
  (clrhash mm-ht)
  (with-help-window "*Modes*"
    (let (roots minors)
      (mapatoms
       (lambda (x)
	 (when (and (commandp x) (string-suffix-p "-mode" (symbol-name x)))
	   (if (memq x minor-mode-list) (push x minors)
	     (when-let ((package (intern-soft (file-name-base (symbol-file x)))))
	       (condition-case nil (require package) (error nil)))
	     (if-let ((parent (get x 'derived-mode-parent)))
		 (push x (gethash parent mm-ht))
	       (push x roots))))))
      (cl-loop for list in
	       (append (mapcar #'cdr (seq-group-by (lambda (x) (null (gethash x mm-ht))) roots))
		       (list minors))
	       for i upfrom 0 do
	       (princ (aref '["Major Mode Hierarchies:" "\nStandalone Major Modes:" "\nMinor Modes:"] i))
	       (with-current-buffer standard-output
		 (add-text-properties (line-beginning-position) (point) '(font-lock-face info-title-2)))
	       (princ "\n\n")
	       (mapc (lambda (x) (mm-map-tree x (gethash x mm-ht) 0))
		     (sort list #'mm-sym-sort))))))
