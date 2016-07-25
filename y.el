;;; -*- lexical-binding: t -*-

(eval-when-compile
  ;; (save-buffer)
  (let* ((path (or load-file-name (buffer-file-name)))
	 (name (file-name-base path))
	 (dir (file-name-directory path))
	 (file (expand-file-name (concat "bin/" name ".el") dir)))
    (load-file file)
    (let* ((forms (with-temp-buffer
    		    (insert-file-contents-literally path)
    		    (car (read-from-string (concat "(" (buffer-string) ")"))))))
      (with-temp-buffer
    	(insert ";;; -*- lexical-binding: t -*-\n")
    	(insert (with-output-to-string
    		  (pp (y-expand `(progn ,@(cdr forms))))))
    	(write-file file)))
    nil))

(setq-local lexical-binding t)

(require 'cl)

(cl-assert lexical-binding)
(let ((h (make-hash-table :test 'eq)))
  (defun y-unique (&optional x)
    (let* ((s (or x 'gs))
	   (n (gethash s h 0))
	   (s1 (make-symbol (format "%s%d" (symbol-name s) n))))
      (puthash s (+ n 1) h)
      s1)))

(defmacro y-let-unique (vars &rest body)
  `(let (,@(mapcar (lambda (x)
		     (list x `(y-unique ',x)))
		   vars))
     ,@body))

(defun y-next (h)
  (if (keywordp (car h))
      (cddr h)
    (cdr h)))

(defmacro y-%for (h k v &rest body)
  (y-let-unique (i)
    `(let ((,i -1))
       (while ,h
	 (let ((,k (if (keywordp (car ,h)) (car ,h) (incf ,i)))
	       (,v (if (keywordp (car ,h)) (cadr ,h) (car ,h))))
	   ,@body)
	 (setq ,h (if (keywordp (car ,h)) (cddr ,h) (cdr ,h))))
       nil)))

(defmacro y-set (a &optional b)
  `(setf ,a ,b))

(defsubst y-get (h k)
  (declare (gv-expander
	    (lambda (f)
	      (gv-letplace (getter setter) h
		(macroexp-let2 nil i k
		  (funcall f `(y-get ,getter ,i)
			   (lambda (val)
			     (macroexp-let2 nil v val
			       `(progn ,(funcall setter
						 `(y-put ,getter ,i ,v))
				       ,v)))))))))
  (if (hash-table-p h)
      (gethash k h)
    (if (listp h)
	(catch 'y-break
	  (y-%for h var val
	    (when (eq var k)
	      (throw 'y-break val))))
      (elt h k))))

(defun y-put (h k &optional v)
  (if (hash-table-p h)
      (progn (puthash k v h) h)
    (let ((l h))
      (if (listp h)
	  (catch 'y-break
	    (when (or (keywordp k) (>= k 0))
	      (when (null l)
		(setq l (if (keywordp k) (list k nil) (list nil)))
		(setq h l))
	      (y-%for h var val
		(when (eq var k)
		  (if (keywordp k)
		      (setcar (cdr h) v)
		    (setq h (setcar h v)))
		  (throw 'y-break l))
		(when (null (y-next h))
		  (if (keywordp k)
		      (nconc h (list k v))
		    (nconc h (list nil)))))))
	(aset h k v))
      l)))

(defun y-length (h)
  (let ((n -1))
    (y-%for h k v
      (when (integerp k)
	(setq n (max n k))))
    (+ n 1)))

(defvar y-environment (list (make-hash-table :test 'equal)))

(defun y-setenv (k &rest keys)
  (let* ((i (if (y-get keys :toplevel) 0 (- (y-length y-environment) 1)))
	 (frame (y-get y-environment i))
	 (entry (y-get frame k)))
    (y-%for keys k v
      (y-set (y-get entry k) v))
    (y-set (y-get frame k) entry)))

(defun y-getenv (k &optional p)
  (let ((i (- (y-length y-environment) 1)))
    (catch 'y-break
      (while (>= i 0)
	(let ((b (y-get (y-get y-environment 0) k)))
	  (if b
	      (throw 'y-break (if p (y-get b p) b))
	    (decf i)))))))

(defun y-symbol-expansion (k)
  (y-getenv k :symbol))

(defun y-symbol-p (k)
  (y-symbol-expansion k))

(defun y-macro-function (k)
  (y-getenv k :macro))

(defun y-macro-p (k)
  (y-macro-function k))

(defun y-macroexpand (form)
  (if (y-symbol-p form)
      (y-macroexpand (y-symbol-expansion form))
    (if (atom form) form
      (let ((x (y-macroexpand (y-get form 0))))
	(if (eq x 'quote)
	    form
	  (if (eq x '\`)
	      (y-quasiexpand (y-get form 1))
	    (if (y-macro-p x)
		(y-macroexpand (apply (y-macro-function x) (cdr form)))
	      (mapcar 'y-macroexpand form))))))))

(defun y-expand (form)
  (macroexpand-all (y-macroexpand form)))

(defun y-eval (form)
  (eval (y-expand form) t))

(defun y-quasiexpand (form)
  (list '\` (y-quasiexpand-1 form 1)))

(defun y-quasiexpand-1 (x depth)
  (cond ((= depth 0)
	 (y-macroexpand x))
	((and (consp x) (eq (car x) '\,))
	 (list '\, (y-quasiexpand-1 (cadr x) (- depth 1))))
	((and (consp x) (eq (car x) '\,@) (= depth 1))
	 (list '\,@ (y-quasiexpand-1 (cadr x) (- depth 1))))
	((and (consp x) (eq (car x) '\`))
	 (list '\` (y-quasiexpand-1 (cadr x) (+ depth 1))))
	((consp x)
	 (mapcar (lambda (form) (y-quasiexpand-1 form depth)) x))
	(t x)))

(defmacro y-do (&rest body)
  (macroexpand-all (y-macroexpand `(progn ,@body))))

(y-do
 (define-macro fn (args &rest body)
   `(lambda ,(if (atom args) `(&rest ,args) args)
      (y-do ,@body)))

 (define-macro define-macro (name args &rest body)
   (let ((form `(y-setenv ',name :macro (fn ,args ,@body))))
     (y-eval form)
     form)))

(provide 'y)
 
