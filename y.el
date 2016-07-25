;;; -*- lexical-binding: t -*-

(eval-and-compile
  ;; (save-buffer)
  (defvar y-module nil)
  (let* ((y-module "y")
         (path (or load-file-name (buffer-file-name)))
         (name (file-name-base path))
         (dir (file-name-directory path))
         (file (expand-file-name (concat "bin/" name ".el") dir)))
    (if (file-exists-p (concat file "c"))
        (load-file (concat file "c"))
      (load-file file))
    (when (file-newer-than-file-p path file)
      (let* ((forms (with-temp-buffer
                      (insert-file-contents-literally path)
                      (car (read-from-string (concat "(" (buffer-string) ")"))))))
        (with-temp-buffer
          (insert ";;; -*- lexical-binding: t -*-\n")
          (insert (with-output-to-string
                    ;; (message "expanding...")
                    (let ((form (funcall 'y-expand `(progn ,@(cdr forms)))))
                      ;; (message "pretty-printing...")
                      (pp form))))
          (untabify (point-min) (point-max))
          (write-region (point-min) (point-max) file nil t)
          (byte-compile-file file))))
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
    (if (hash-table-p h)
        (let ((n -1))
          (maphash (lambda (k v)
                     (if (integerp k)
                         (setq n (max n k))))
                   h)
          (+ n 1))
      (length h))))

(y-do
 (defvar y-environment (list (obj)))

 (define-global setenv (k &rest ks)
   (let* ((i (if (get ks :toplevel) 0 (- (\# environment) 1)))
          (frame (get environment i))
          (entry (get frame k)))
     (each (k v) ks
       (set (get entry k) v))
     (set (get frame k) entry)))

 (define-global getenv (k &optional p)
   (let ((i (- (\# environment) 1)))
     (catch 'y-break
       (while (>= i 0)
         (let ((b (get (at environment 0) k)))
           (if b
	       (throw 'y-break (if p (get b p) b))
	      (decf i)))))))

 (define-symbol unique y-unique)
 (define-symbol let-unique y-let-unique)
 (define-symbol at get)
 (define-symbol get y-get)
 (define-symbol set y-set)
 (define-symbol \# y-length)
 (define-symbol = eql)
 (define-symbol environment y-environment)

 (define-global nil? (x)
   (= x nil))

 (define-global is? (x)
   (not (nil? x)))

 (define-global none? (x) (= (\# x) 0))
 (define-global some? (x) (> (\# x) 0))
 (define-global one? (x) (= (\# x) 1))
 (define-global two? (x) (= (\# x) 2))

 (define-global hd (l) (at l 0))

 (define-global type (x) (type-of x))

 (define-global string? (x) (stringp x))
 (define-global symbol? (x) (symbolp x))
 (define-global number? (x) (or (integerp x) (numberp x)))
 (define-global function? (x) (and (not (symbol? x)) (functionp x)))

 (define-global obj? (x) (hash-table-p x))

 (define-macro obj ()
   `(make-hash-table :test 'equal))

 (define-global atom? (x)
   (or (nil? x) (symbol? x) (string? x) (number? x)))

 (define-global clamp (n a b)
   (if (< n a) a
     (if (> n b) b
       n)))

 (define-global clip (s from &optional upto)
   (let* ((n (length s))
          (i (clamp from 0 n))
          (j (if upto (clamp upto i n))))
     (substring s i j)))

 (define-global cut (x &optional from upto)
   (with l (if (obj? x) (obj) ())
     (let* ((j 0)
            (i (if (or (nil? from) (< from 0)) 0 from))
            (n (\# x))
            (upto (if (or (nil? upto) (> upto n)) n upto)))
       (while (< i upto)
         (set (at l j) (at x i))
         (inc i)
         (inc j))
       (each (k v) x
         (unless (number? k)
           (set (get l k) v))))))

 (define-global keys (x)
   (with l (if (obj? x) (obj) ())
     (each (k v) x
       (unless (number? k)
         (set (get l k) v)))))

 (define-global edge (x)
   (- (\# x) 1))

 (define-global tl (l) (cut l 1))

 (define-global last (l)
   (at l (edge l)))

 (define-global almost (l)
   (cut l 0 (edge l)))

 (define-macro add (l x)
   (cl-assert (atom? l))
   `(progn (set (at ,l (\# ,l)) ,x) nil))

 (define-macro drop (l)
   (cl-assert (atom? l))
   `(prog1 (at ,l (edge ,l))
      (set ,l (almost ,l))))

 (define-global reverse (l)
   (with l1 (keys l)
     (let ((i (edge l)))
       (while (>= i 0)
         (add l1 (at l i))
         (dec i)))))

 (define-global reduce (f x)
   (if (none? x) nil
       (one? x) (hd x)
     (funcall f (hd x) (reduce f (tl x)))))

 (define-global join ls
   (if (two? ls)
       (let ((a (at ls 0))
             (b (at ls 1)))
         (if (and a b)
             (let ((c (if (obj? a) (obj) ()))
                   (o (\# a)))
               (each (k v) a
                 (set (get c k) v))
               (each (k v) b
                 (when (number? k)
                   (inc k o))
                 (set (get c k) v))
               c)
           (or a b ())))
     (or (reduce 'y-join ls) ())))

 (define-global find (f l)
   (catch 'y-break
     (each x l
       (let ((y (funcall f x)))
         (if y (throw 'y-break y))))))

 (define-global first (f l)
   (catch 'y-break
     (step x l
       (let ((y (funcall f x)))
         (if y (throw 'y-break y))))))

 (define-global in? (x l)
   (find (fn (y) (= x y)) l))

 (define-global pair (l)
   (with l1 (if (obj? l) (obj) ())
     (let ((n (\# l)))
       (for i n
         (add l1 (list (at l i) (at l (+ i 1))))
         (inc i)))))

 (define-global map (f x)
   (with l (if (obj? x) (obj) ())
     (step v x
       (let ((y (funcall f v)))
         (if (is? y)
           (add l y))))
     (each (k v) x
       (unless (number? k)
         (let ((y (funcall f v)))
           (when (is? y)
             (set (get l k) y)))))))

 (define-global keep (f x)
   (map (fn (v) (when (funcall f v) v)) x))

 (define-global keys? (l)
   (catch 'y-break
     (each (k v) l
       (unless (number? k)
         (throw 'y-break t)))
     nil))

 (define-global empty? (l)
   (catch 'y-break
     (each x l
       (throw 'y-break nil))
     t))

 (define-global stash (args)
   (let ((l ()))
     (each (k v) args
       (when (number? k)
         (add l v)))
     (when (keys? args)
       (let ((p (if (obj? args) (obj) ())))
         (each (k v) args
           (unless (number? k)
             (set (get p k) v)))
         (set (get p :_stash) t)
         (add l p)))
     l))

 (define-global unstash (args)
   (if (none? args) ()
     (let ((l (last args)))
       (if (get l :_stash)
           (with args1 (almost args)
             (each (k v) l
               (unless (= k :_stash)
                 (set (get args1 k) v))))
         args))))

 (define-global destash! (l args1)
   (if (and (or (listp l) (obj? l)) (get l :_stash))
       (each (k v) l
         (unless (= k :_stash)
           (set (get args1 k) v)))
     l))

 (define-global apply (f args)
   (let ((args1 (stash args)))
     (funcall 'apply f args1)))

 (define id (x)
   (let ((s (append (if (symbolp x) (symbol-name x) x) nil)))
     (when (eq ?? (get s (edge s)))
       (if (memq ?- s)
            (progn (set (get s (edge s)) ?-)
                   (set (get s (\# s)) ?p))
          (set (get s (edge s)) ?p)))
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
        (let ((x (macroexpand (get form 0))))
          (if (eq x 'quote)
              form
            (if (eq x '\`)
                (quasiexpand (get form 1))
              (if (macro? x)
                  (macroexpand (apply (macro-function x) (cdr form)))
                (mapcar 'y-macroexpand form))))))))

 (define-global expand (form)
   (macroexpand-all (macroexpand form)))

 (define-global eval (form)
   (funcall 'eval (expand form) t))

 (define-macro with (x v &rest body)
   `(let ((,x ,v)) ,@body ,x))

 (define-macro fn (args &rest body)
   `(lambda ,(if (not (listp args)) `(&rest ,args) args)
      (y-do ,@body)))

 (define-macro define-macro (name args &rest body)
   (let ((form `(setenv ',name :macro (fn ,args ,@body))))
     (eval form)
     form))

 (define-macro define-symbol (name expansion)
   (setenv name :symbol expansion)
   `(setenv ',name :symbol ',expansion))

 (define-macro define (name x &rest body)
   (let ((var (global-id (concat (module-name) "--") name)))
     (setenv name :symbol var)
     (setenv var :variable t)
     `(defalias ',var (fn ,x ,@body))))

 (define-macro define-global (name x &rest body)
   (let ((var (global-id (concat (module-name) "-") name)))
     (setenv name :symbol var)
     (setenv var :variable t :toplevel t)
     `(progn (defalias ',var (fn ,x ,@body))
             (setenv ',name :symbol ',var))))

 (define-macro inc (n &optional by)
   `(set ,n (+ ,n ,(or by 1))))

 (define-macro dec (n &optional by)
   `(set ,n (- ,n ,(or by 1))))

 (define-macro for (i to &rest body)
   `(let ((,i 0))
      (while (< ,i ,to)
        ,@body
        (inc ,i))))

 (define-macro step (v l &rest body)
   (let-unique (x n i)
     `(let* ((,x ,l) (,n (\# ,x)))
        (for ,i ,n
          (let ((,v (at ,x ,i)))
            ,@body)))))

 (define-macro each (x l &rest body)
   (let-unique (o n i f)
     (cl-destructuring-bind (k v) (if (atom? x) (list i x)
                                    (if (> (\# x) 1) x
                                      (list i (hd x))))
       `(let ((,o ,l)
              (,f (lambda (,k ,v) ,@body)))
          (if (hash-table-p ,o)
              (maphash ,f ,o)
            (if (listp ,o)
                (y-%for ,o ,k ,v
                  (funcall ,f ,k ,v))
              (let ((,n (\# ,o)))
                (for ,k ,n
                  (let ((,v (at ,o ,k)))
                    (funcall ,f ,k ,v))))))))))
)

(defmacro y-do (&rest body)
  (macroexpand-all (y-macroexpand `(progn ,@body))))

(provide 'y)
 
