;;; -*- lexical-binding: t -*-

(eval-and-compile
  ;; (save-buffer)
  (defvar y-module nil)
  (let* ((y-module "y")
	 (path (or load-file-name (buffer-file-name)))
	 (name (file-name-base path))
	 (dir (file-name-directory path))
	 (file (expand-file-name (concat "bin/" name ".el") dir)))
    (load-file file)
    (unless load-file-name
      (let* ((forms (with-temp-buffer
		      (insert-file-contents-literally path)
		      (car (read-from-string (concat "(" (buffer-string) ")"))))))
	(with-temp-buffer
	  (insert ";;; -*- lexical-binding: t -*-\n")
	  (insert (with-output-to-string
		    (pp (funcall 'y-expand `(progn ,@(cdr forms))))))
	  (untabify (point-min) (point-max))
	  (write-file file))))
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

(defun y-get (h k)
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
  (if (listp h)
      (let ((n -1))
	(y-%for h k v
	  (when (integerp k)
	    (setq n (max n k))))
	(+ n 1))
    (length h)))

(defvar y-environment (list (make-hash-table :test 'equal)))

(y-do
 (define-global setenv (k &rest keys)
   (let* ((i (if (y-get keys :toplevel) 0 (- (y-length y-environment) 1)))
	  (frame (y-get y-environment i))
	  (entry (y-get frame k)))
     (y-%for keys k v
       (y-set (y-get entry k) v))
     (y-set (y-get frame k) entry)))

 (define-global getenv (k &optional p)
   (let ((i (- (y-length y-environment) 1)))
     (catch 'y-break
       (while (>= i 0)
	 (let ((b (y-get (y-get y-environment 0) k)))
	   (if b
		(throw 'y-break (if p (y-get b p) b))
	      (decf i)))))))

 (define-global edge (x)
   (- (y-length x) 1))

 (define id (x)
   (let ((s (append (if (symbolp x) (symbol-name x) x) nil)))
     (when (eq ?? (y-get s (edge s)))
       (if (memq ?- s)
	    (progn (y-set (y-get s (edge s)) ?-)
		   (y-set (y-get s (y-length s)) ?p))
	  (y-set (y-get s (edge s)) ?p)))
     (intern (concat s))))

 (defvar y-module nil)

 (define module-name ()
   (or y-module
       (let ((file (or load-file-name (buffer-file-name))))
	 (if file (file-name-base file) (buffer-name)))))

 (define global-id (prefix name)
   (let ((s (if (stringp name) name (symbol-name name))))
     (if (eq 0 (string-match (regexp-quote prefix) s))
	  name
	(id (concat prefix s)))))

 (define symbol-expansion (k)
   (getenv k :symbol))

 (define symbol? (k)
   (let ((v (symbol-expansion k)))
     (and v (not (eq v k)))))

 (define macro-function (k)
   (getenv k :macro))

 (define macro? (k)
   (macro-function k))

 (define quasiexpand-1 (x depth)
   (cond ((= depth 0)
	  (macroexpand x))
	 ((and (consp x) (eq (car x) '\,))
	  (list '\, (quasiexpand-1 (cadr x) (- depth 1))))
	 ((and (consp x) (eq (car x) '\,@) (= depth 1))
	  (list '\,@ (quasiexpand-1 (cadr x) (- depth 1))))
	 ((and (consp x) (eq (car x) '\`))
	  (list '\` (quasiexpand-1 (cadr x) (+ depth 1))))
	 ((consp x)
	  (mapcar (lambda (form) (quasiexpand-1 form depth)) x))
	 (t x)))

 (define quasiexpand (form)
   (list '\` (quasiexpand-1 form 1)))

 (define-global macroexpand (form)
   (if (symbol? form)
	(macroexpand (symbol-expansion form))
      (if (atom form) form
	(let ((x (macroexpand (y-get form 0))))
	  (if (eq x 'quote)
	      form
	    (if (eq x '\`)
		(quasiexpand (y-get form 1))
	      (if (macro? x)
		  (macroexpand (apply (macro-function x) (cdr form)))
		(mapcar 'y-macroexpand form))))))))

 (define-global expand (form)
   (macroexpand-all (macroexpand form)))

 (define-global eval (form)
   (funcall 'eval (expand form) t))

 (define-macro fn (args &rest body)
   `(lambda ,(if (atom args) `(&rest ,args) args)
      (y-do ,@body)))

 (define-macro define-macro (name args &rest body)
   (let ((form `(setenv ',name :macro (fn ,args ,@body))))
     (eval form)
     form))

 (define-macro define (name x &rest body)
   (let ((var (global-id (concat (module-name) "--") name)))
     (setenv name :symbol var)
     (setenv var :variable t)
     `(defalias ',var (fn ,x ,@body))))

 (define-macro define-global (name x &rest body)
   (let ((var (global-id (concat (module-name) "-") name)))
     (setenv name :symbol var)
     (setenv var :variable t :toplevel t)
     `(defalias ',var (fn ,x ,@body))))
)

(defmacro y-do (&rest body)
  (macroexpand-all (y-macroexpand `(progn ,@body))))

(provide 'y)
 
