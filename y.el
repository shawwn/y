;;; -*- lexical-binding: t -*-

(eval-and-compile
  (let* ((input (or load-file-name (buffer-file-name))))
    (when (and input (file-exists-p input))
      (let* ((name (file-name-base input))
             (dir (file-name-directory input))
             (output (expand-file-name (concat "bin/" name ".el") dir)))
        (when (file-exists-p output)
          (load-file output)
          (unless y-module
            (when (file-newer-than-file-p input output)
              (y-write-file output (y-compile-file input name))))))))
  nil)

(setq-local lexical-binding t)

(eval-when-compile
  (require 'cl))

(cl-assert lexical-binding)
(let* ((h (make-hash-table :test 'eq)))
  (defun y-unique (&optional x)
    (let* ((s (or x 'gs))
           (n (gethash s h 0))
           (s1 (make-symbol (format "%s%d" (symbol-name s) n))))
      (puthash s (+ n 1) h)
      s1)))

(defmacro y-let-unique (vars &rest body)
  `(let* (,@(mapcar (lambda (x)
                     (list x `(y-unique ',x)))
                   vars))
     ,@body))

(defsubst y-next (h)
  (if (keywordp (car h))
      (cddr h)
    (cdr h)))

(cl-assert lexical-binding)
(let* ((h (make-hash-table :test 'eq))
       (unset (list nil)))
  (defsubst y-%key (x)
    (if (keywordp x)
        (let* ((x1 (gethash x h unset)))
          (when (eq x1 unset)
            (setq x1 (intern (substring (symbol-name x) 1)))
            (puthash x x1 h))
          x1)
      x)))

(defmacro y-%for (h k v &rest body)
  (y-let-unique (i)
    `(let* ((,i -1))
       (while ,h
         (let* ((,k (if (keywordp (car ,h)) (y-%key (car ,h)) (setq ,i (1+ ,i))))
                (,v (if (keywordp (car ,h)) (cadr ,h) (car ,h))))
           ,@body)
         (setq ,h (if (keywordp (car ,h)) (cddr ,h) (cdr ,h))))
       nil)))

(defmacro y-set (a &optional b)
  `(setf ,a ,b))

(defsubst y-get (h k)
  (if (hash-table-p h)
      (gethash k h)
    (if (listp h)
        (catch 'y-break
          (y-%for h var val
            (when (eq var k)
              (throw 'y-break val))))
      (elt h k))))

(put 'y-get 'gv-expander
     #'(lambda (f place idx)
         (gv-letplace (getter _setter) place
           (funcall f `(y-get ,getter ,idx)
                    #'(lambda (val)
                        (y-let-unique (k v)
                          `(let* ((,k ,idx)
                                  (,v ,val))
                             (y-set ,getter (y-put ,getter ,k ,v))
                             ,v)))))))

(defun y-put (h k &rest args)
  (let ((v (car args))
        (wipe? (null args)))
    (if (hash-table-p h)
        (progn
          (setq k (y-%key k))
          (if wipe?
              (if (integerp k)
                  (let ((n (y-length h))
                        (i k)
                        (unset (list nil)))
                    (if (and (>= i 0) (<= i (1- n)))
                        (progn
                          (while (< i n)
                            (let ((x (gethash (1+ i) h unset)))
                              (if (eq x unset)
                                  (remhash i h)
                                (puthash i x h)))
                            (incf i))
                          (remhash i h))
                      (remhash k h)))
                (remhash k h))
            (puthash k v h))
          h)
      (let* ((l h))
        (if (and (symbolp k) (not (keywordp k)))
            (setq k (intern (concat ":" (symbol-name k)))))
        (if (listp h)
            (catch 'y-break
              (if wipe?
                  (let ((l1 h)
                        (p nil)
                        (head? t))
                    (y-%for h var _val
                      (if (eq var k)
                          (if head?
                              (progn (setq l1 (if (keywordp k) (cddr h) (cdr h)))
                                     (setq h l1))
                            (progn (setcdr (if (keywordp (car p)) (cdr p) p)
                                           (if (keywordp k) (cddr h) (cdr h)))
                                   (setq h p)))
                        (setq head? nil))
                      (setq p h))
                    (setq l l1))
                (when (or (keywordp k) (>= k 0))
                  (when (null l)
                    (setq l (if (keywordp k) (list k nil) (list nil)))
                    (setq h l))
                  (y-%for h var _val
                    (when (eq var (y-%key k))
                      (if (integerp k)
                          (setcar h v)
                        (setcar (cdr h) v))
                      (throw 'y-break l))
                    (when (null (y-next h))
                      (if (keywordp k)
                          (nconc h (list k v))
                        (nconc h (list nil))))))))
          (if wipe?
              (error (format "Can't wipe index %s of %S" k h))
            (aset h k v)))
        l))))

(defmacro y-wipe (place)
  (gv-letplace (getter _setter) place
    (if (eq 'y-get (car-safe getter))
        `(y-set ,(cadr getter) (y-put ,@(cdr getter)))
      (error (format "Can't wipe %S" place)))))

(defun y-length (h &optional upto)
  (catch 'y-break
    (if (listp h)
        (let* ((n -1))
          (y-%for h k _v
            (when (integerp k)
              (setq n (max n k))
              (when (and upto (>= n upto))
                (throw 'y-break (+ n 1)))))
          (+ n 1))
      (if (hash-table-p h)
          (let* ((n -1))
            (maphash (lambda (k _v)
                       (when (integerp k)
                         (setq n (max n k))
                         (when (and upto (>= n upto))
                           (throw 'y-break (+ n 1)))))
                     h)
            (+ n 1))
        (length h)))))

(defmacro y-%if (&rest args)
  `(if ,@args))

(defmacro y-do (&rest body)
  (let* ((max-lisp-eval-depth 4450)
         (max-specpdl-size 13350)
         (y-environment (apply 'vector (append y-environment nil))))
    (macroexp-progn (mapcar 'y-macroexpand body))))

(y-do
  (defvar y-environment (list (obj)))

  (define-global setenv (k :rest ks)
    (let (i (if (memq :toplevel ks) 0 (- (\# environment) 1))
          frame (at environment i)
          entry (or (get frame k) (obj)))
      (each (k v) ks
        (set (get entry k) v))
      (set (get frame k) entry)))

  (define-global getenv (k &optional p)
    (let i (- (\# environment) 1)
      (catch 'y-break
        (while (>= i 0)
          (let b (get (at environment i) k)
            (if b (throw 'y-break (if p (get b p) b))
              (dec i)))))))

  (define-symbol unique y-unique)
  (define-symbol let-unique y-let-unique)
  (define-symbol at get)
  (define-macro set args `(y-set ,@args))
  (define-macro wipe args `(y-wipe ,@args))
  (define-symbol get y-get)
  (define-symbol \# y-length)
  (define-symbol environment y-environment)
  (define-symbol %if y-%if)

  (define-global nil? (x)
    (= x nil))

  (define-global is? (x)
    (not (nil? x)))

  (define-global none? (x) (= (\# x 0) 0))
  (define-global some? (x) (> (\# x 0) 0))
  (define-global one? (x) (= (\# x 1) 1))
  (define-global two? (x) (= (\# x 2) 2))

  (define-global hd (l) (at l 0))

  (define-global type (x) (type-of x))

  (define-global number? (x) (or (integerp x) (numberp x)))

  (define-global obj? (x) (hash-table-p x))

  (define-macro obj ()
    `(make-hash-table :test 'eq))

  (define-global atom? (x)
    (or (nil? x) (symbolp x) (stringp x) (number? x)))

  (define-global clip (s from &optional upto)
    (let (n (length s)
          i (if (or (nil? from) (< from 0)) 0 from)
          j (if (or (nil? upto) (> upto n)) n (max upto i)))
      (substring s i j)))

  (define chop? (x from upto)
    (and (consp x) (= from 1) (null upto) (not (keywordp (car x)))))

  (define-global cut (x &optional from upto)
    (if (chop? x from upto)
        (cdr x)
      (with l (if (obj? x) (obj) ())
        (let (j 0
              i (if (or (nil? from) (< from 0)) 0 from)
              n (\# x)
              upto (if (or (nil? upto) (> upto n)) n upto))
          (while (< i upto)
            (set (at l j) (at x i))
            (inc i)
            (inc j))
          (each (k v) x
            (unless (number? k)
              (set (get l k) v)))))))

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
      (let i (edge l)
        (while (>= i 0)
          (add l1 (at l i))
          (dec i)))))

  (define-global reduce (f x)
    (if (none? x) nil
        (one? x) (hd x)
      (funcall f (hd x) (reduce f (tl x)))))

  (define-global join ls
    (if (two? ls)
        (let ((a b) ls)
          (if (and a b)
              (let (c (if (obj? a) (obj) ())
                    o (\# a))
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
        (let y (funcall f x)
          (if y (throw 'y-break y))))))

  (define-global first (f l)
    (catch 'y-break
      (step x l
        (let y (funcall f x)
          (if y (throw 'y-break y))))))

  (define-global in? (x l)
    (find (fn (y) (= x y)) l))

  (define-global pair (l)
    (with l1 (if (obj? l) (obj) ())
      (let n (\# l)
        (for i n
          (add l1 (list (at l i) (at l (+ i 1))))
          (inc i)))))

  (define-global map (f x)
    (with l (if (obj? x) (obj) ())
      (step v x
        (let y (funcall f v)
          (if (is? y)
              (add l y))))
      (each (k v) x
        (unless (number? k)
          (let y (funcall f v)
            (when (is? y)
              (set (get l k) y)))))))

  (define-global keep (f x)
    (map (fn (v) (when (funcall f v) v)) x))

  (define-global keys? (l)
    (catch 'y-break
      (each (k v) l
        (setq v v)
        (unless (number? k)
          (throw 'y-break t)))
      nil))

  (define-global empty? (l)
    (catch 'y-break
      (each x l
        (setq x x)
        (throw 'y-break nil))
      t))

  (define-global toplevel? ()
    (one? environment))

  (define-global print (x)
    (princ (format "%s\n" x))
    nil)

  (define id (x)
    (let s (append (if (symbolp x) (symbol-name x) x) nil)
      (when (= ?? (get s (edge s)))
        (if (memq ?- s)
            (progn (set (get s (edge s)) ?-)
                   (set (get s (\# s)) ?p))
          (set (get s (edge s)) ?p)))
      (intern (concat s))))

  (defvar y-module nil)

  (define module-name ()
    (or y-module
        (let file (or load-file-name (buffer-file-name))
          (if file (file-name-base file) (buffer-name)))))

  (define global-id (prefix name)
    (let s (if (stringp name) name (symbol-name name))
      (if (= 0 (string-match (regexp-quote prefix) s))
          name
        (id (concat prefix s)))))

  (define macro-function (k)
    (getenv k 'macro))

  (define macro? (k)
    (macro-function k))

  (define symbol-expansion (k)
    (getenv k 'symbol))

  (define symbol? (k)
    (let v (symbol-expansion k)
      (and v (not (= v k)))))

  (define variable? (k)
    (let i (edge environment)
      (catch 'y-break
        (while (>= i 0)
          (let b (get (at environment i) k)
            (if b (throw 'y-break (get b 'variable))
              (decf i)))))))

  (define bound? (x)
    (or (macro? x)
        (symbol? x)
        (variable? x)))

  (define bind (lh rh)
    (if (atom? lh) `(,lh ,rh)
      (let-unique (var)
        (with bs (list var rh)
          (each (k v) lh
            (let x (if (= k 'rest)
                       `(cut ,var ,(\# lh))
                     `(get ,var ',k))
              (when (is? k)
                (let k (if (= v t) k v)
                  (join! bs (bind k x))))))))))

  (define bind* (args body)
    (if (and args (atom? args))
        (bind* `(&rest ,args) body)
      (let (rest nil args1 () bs () ks ())
        (for i (\# args)
          (let arg (at args i)
            (if (= arg '&rest)
                (set rest (at args (inc i)))
                (atom? arg)
                (add args1 arg)
              (let-unique (id1)
                (add args1 id1)
                (join! bs (list arg id1))))))
        (let-when x (get args 'rest)
          (set rest x))
        (each (k v) args
          (unless (or (number? k) (= k 'rest))
            (when (nil? rest)
              (set rest (unique 'args)))
            (let (v1 (if (= v t) k v)
                  k1 (intern (format ":%s" k)))
               (join! ks (list k1 v1)))))
        (when rest
          (add args1 '&rest)
          (add args1 rest))
        (when ks
          (join! bs (list ks rest)))
        (with l (list args1)
          (let x (hd body)
            (when (stringp x)
              (add l x)
              (set body (tl body))))
          (if (is? bs)
              (add l `(let ,bs ,@body))
            (join! l body))))))

  (define-global macroexpand (form)
    (let s (symbol-expansion form)
      (if s (macroexpand s)
        (if (atom form) form
          (let x (macroexpand (hd form))
            (if (= x 'quote) form
                (= x '\`)
                (macroexpand (funcall 'macroexpand form))
                (macro? x)
                (macroexpand (apply (macro-function x) (tl form)))
              (cons x (mapcar 'y-macroexpand (tl form)))))))))

  (define-global eval (form)
    (let* ((max-lisp-eval-depth 4450)
           (max-specpdl-size 13350))
      (funcall 'eval (macroexpand form) t)))

  (define-macro = args
    (if (find 'stringp args)
        `(string= ,@args)
      `(eql ,@args)))

  (define expand-if ((a b :rest c))
    (if (some? c) `((%if ,a ,b ,@(expand-if c)))
        (is? b) `((%if ,a ,b))
        (is? a) (list a)))

  (define-macro if branches
    (hd (expand-if branches)))

  (define-macro with (x v :rest body)
    `(let (,x ,v) ,@body ,x))

  (define-macro let-when (x v :rest body)
    (let-unique (y)
      `(let ,y ,v
         (when ,y
           (let (,x ,y)
             ,@body)))))

  (define-macro fn (args :rest body)
    `(lambda ,@(bind* args body)))

  (define-macro define-macro (name args :rest body)
    (let* ((form `(setenv ',name :macro (fn ,args ,@body))))
      (eval form)
      form))

  (define-macro define-symbol (name expansion)
    (setenv name :symbol expansion)
    `(setenv ',name :symbol ',expansion))

  (define-macro define (name x :rest body)
    (let var (global-id (concat (module-name) "--") name)
      (setenv name :symbol var)
      (setenv var :variable t)
      `(prog1 (defalias ',var (fn ,x ,@body))
              (setenv ',name :symbol ',var))))

  (define-macro define-global (name x :rest body)
    (let var (global-id (concat (module-name) "-") name)
      (setenv name :symbol var)
      (setenv var :variable t :toplevel t)
      `(prog1 (defalias ',var (fn ,x ,@body))
              (setenv ',name :symbol ',var))))

  (define-macro with-frame body
    (let-unique (x)
      `(progn (set environment (apply 'vector (append environment (list (obj)))))
              (with ,x (progn ,@body)
                (set environment (apply 'vector (almost environment)))))))

  (define-macro let-macro (definitions :rest body)
    (with-frame
      (step m definitions
        (macroexpand `(define-macro ,@m)))
      `(progn ,@(macroexpand body))))

  (define-macro let-symbol (expansions :rest body)
    (if (none? expansions)
        `(progn ,@(macroexpand body))
      (with-frame
        (step x (pair expansions)
          (macroexpand `(define-symbol ,@x)))
        `(progn ,@(macroexpand body)))))

  (define-macro when-compiling body
    (eval `(progn ,@body)))

  (define-macro let (bs :rest body)
    (if (and bs (atom bs)) `(let (,bs ,(hd body)) ,@(tl body))
        (none? bs) `(progn ,@body)
      (let ((lh rh :rest bs2) bs
            (var val :rest bs1) (bind lh rh))
        (let renames ()
          (if (or (bound? var) (toplevel?))
              (let var1 (unique var)
                (set renames (list var var1))
                (set var var1))
            (setenv var :variable t))
          (let form `(let ,(join bs1 bs2) ,@body)
            (unless (none? renames)
              (set form `(let-symbol ,renames ,form)))
            `(let* ((,var ,val))
               ,(macroexpand form)))))))

  (define-macro join! (a :rest bs)
    `(set ,a (join ,a ,@bs)))

  (define-macro inc (n &optional by)
    `(set ,n (+ ,n ,(or by 1))))

  (define-macro dec (n &optional by)
    `(set ,n (- ,n ,(or by 1))))

  (define-macro for (i to :rest body)
    `(let ,i 0
       (while (< ,i ,to)
         ,@body
         (inc ,i))))

  (define-macro step (v l :rest body)
    (let-unique (x n i)
      `(let (,x ,l ,n (\# ,x))
         (for ,i ,n
           (let (,v (at ,x ,i))
             ,@body)))))

  (define-macro each (x l :rest body)
    (let-unique (o n f a)
      (let ((k v) (if (atom? x) (list (unique 'i) x)
                    (if (> (\# x) 1) x
                      (list (unique 'i) (hd x)))))
        `(let (,o ,l ,f (fn (,k ,v) ,@body))
           (if (hash-table-p ,o)
               (maphash ,f ,o)
               (listp ,o)
               (y-%for ,o ,k ,a
                 (funcall ,f ,k ,a))
             (let ,n (\# ,o)
               (for ,k ,n
                 (let ,a (at ,o ,k)
                   (funcall ,f ,k ,a)))))))))

  (define eval-print (form)
    (condition-case err
        (let x (eval form)
          (if (is? x) (print (format "%S" x))))
      (error (print (format "error: %s" (error-message-string err))))))

  (define read-string (s &optional more)
    (if more
        (condition-case nil
            (car (read-from-string s))
          (end-of-file more))
      (car (read-from-string s))))

  (define rep (s)
    (eval-print (read-string s)))

  (define repl ()
    (let buf ""
      (let rep1 (fn (s)
                  (set buf (concat buf s))
                  (let (more (obj)
                        form (read-string buf more))
                    (unless (= form more)
                      (eval-print form)
                      (set buf "")
                      (princ "> "))))
        (princ "> ")
        (catch 'y-break
          (while t
            (let s (read-from-minibuffer "")
              (if (and s (not (= s ":a")))
                  (funcall rep1 (concat s "\n"))
                (throw 'y-break nil))))))))

  (define-global compile-file (path &optional module-name)
    (let* ((name (or module-name (file-name-base path)))
           (y-module name)
           (forms (with-temp-buffer
                    (insert-file-contents-literally path)
                    (car (read-from-string (concat "(" (buffer-string) ")")))))
           (exprs (funcall 'macroexpand-all `(progn ,@forms))))
      (with-temp-buffer
        (insert ";;; -*- lexical-binding: t -*-\n")
        (insert (with-output-to-string
                  (prin1 exprs)))
        (untabify (point-min) (point-max))
        (buffer-string))))

  (define-global write-file (path data)
    (with-temp-buffer
      (insert data)
      (write-region (point-min) (point-max) path nil t)))

  (define run-file (path)
    (funcall 'load-file path))

  (define usage ()
    (print "usage: y [options] <object files>")
    (print "options:")
    (print "  -c <input>\tCompile input file")
    (print "  -o <output>\tOutput file")
    (print "  -e <expr>\tExpression to evaluate"))

  (define-global main (args)
    (let arg (hd args)
      (if (or (= arg "-h")
              (= arg "--help"))
          (usage)
        (let (pre ()
              input nil
              output nil
              target1 nil
              expr nil
              n (\# args))
          (for i n
            (let a (at args i)
              (if (or (= a "-c")
                      (= a "-o")
                      (= a "-e"))
                  (if (= i (- n 1))
                      (print (format "missing argument for %S" a))
                    (progn (inc i)
                           (let val (at args i)
                             (if (= a "-c") (set input val)
                                 (= a "-o") (set output val)
                                 (= a "-e") (set expr val)))))
                (add pre a))))
          (step file pre
            (run-file file))
          (if (nil? input) (if expr (rep expr) (repl))
            (let code (compile-file input)
              (if (or (nil? output) (= output "-"))
                  (print code)
                (write-file output code))))))))

  (when noninteractive
    (let args command-line-args-left
      (set command-line-args-left nil)
      (main args)))
)

(provide 'y)
 
