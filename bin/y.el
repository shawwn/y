;;; -*- lexical-binding: t -*-
(quote nil)
(set (make-local-variable (quote lexical-binding)) t)
(quote cl)
(or lexical-binding (cl--assertion-failed (quote lexical-binding)))
nil
(let* ((h (make-hash-table :test (quote eq)))) (defalias (quote y-unique) (function (lambda (&optional x) (let* ((s (or x (quote gs))) (n (gethash s h 0)) (s1 (make-symbol (format "%s%d" (symbol-name s) n)))) (puthash s (+ n 1) h) s1)))))

(defalias (quote y-let-unique) (cons (quote macro) (function (lambda (vars &rest body) (cons (quote let*) (cons (mapcar (function (lambda (x) (list x (list (quote y-unique) (list (quote quote) x))))) vars) body))))))

(defalias (quote y-next) (function (lambda (h) (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))))
(quote byte-compile-inline-expand)
(or lexical-binding (cl--assertion-failed (quote lexical-binding)))
nil
(let* ((h (make-hash-table :test (quote eq))) (unset (list nil))) (prog1 (defalias (quote y-%key) (function (lambda (x) (if (keywordp x) (let* ((x1 (gethash x h unset))) (if (eq x1 unset) (progn (setq x1 (intern (substring (symbol-name x) 1))) (puthash x x1 h))) x1) x)))) (quote byte-compile-inline-expand)))

(defalias (quote y-%for) (cons (quote macro) (function (lambda (h k v &rest body) (let* ((i (y-unique (quote i)))) (cons (quote let*) (cons (list (cons i (quote (-1)))) (cons (list (quote while) h (cons (quote let*) (cons (list (list k (list (quote if) (list (quote keywordp) (list (quote car) h)) (list (quote y-%key) (list (quote car) h)) (list (quote setq) i (list (quote 1+) i)))) (list v (list (quote if) (list (quote keywordp) (list (quote car) h)) (list (quote cadr) h) (list (quote car) h)))) body)) (list (quote setq) h (list (quote if) (list (quote keywordp) (list (quote car) h)) (list (quote cddr) h) (list (quote cdr) h)))) (quote (nil))))))))))

(defalias (quote y-set) (cons (quote macro) (function (lambda (a &optional b) (list (quote setf) a b)))))

(defalias (quote y-get) (function (lambda (h k) (if (hash-table-p h) (gethash k h) (if (listp h) (catch (quote y-break) (let* ((i0 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i0 (1+ i0)))) (val (if (keywordp (car h)) (car (cdr h)) (car h)))) (if (eq var k) (progn (throw (quote y-break) val)))) (setq h (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))) nil)) (elt h k))))))
(quote byte-compile-inline-expand)
(put (quote y-get) (quote gv-expander) (function (lambda (f place idx) (gv-get place (function (lambda (getter _setter) (funcall f (list (quote y-get) getter idx) (function (lambda (val) (let* ((k (y-unique (quote k))) (v (y-unique (quote v)))) (list (quote let*) (list (list k idx) (list v val)) (list (quote y-set) getter (list (quote y-put) getter k v)) v)))))))))))

(defalias (quote y-put) (function (lambda (h k &rest args) (let ((v (car args)) (wipe\? (null args))) (if (hash-table-p h) (progn (setq k (y-%key k)) (if wipe\? (if (integerp k) (let ((n (y-length h)) (i k) (unset (list nil))) (if (and (>= i 0) (<= i (1- n))) (progn (while (< i n) (let ((x (gethash (1+ i) h unset))) (if (eq x unset) (remhash i h) (puthash i x h))) (setq i (1+ i))) (remhash i h)) (remhash k h))) (remhash k h)) (puthash k v h)) h) (let* ((l h)) (if (and (symbolp k) (not (keywordp k))) (setq k (intern (concat ":" (symbol-name k))))) (if (listp h) (catch (quote y-break) (if wipe\? (let ((l1 h) (p nil) (head\? t)) (let* ((i1 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i1 (1+ i1)))) (_val (if (keywordp (car h)) (car (cdr h)) (car h)))) (if (eq var k) (if head\? (progn (setq l1 (if (keywordp k) (cdr (cdr h)) (cdr h))) (setq h l1)) (progn (setcdr (if (keywordp (car p)) (cdr p) p) (if (keywordp k) (cdr (cdr h)) (cdr h))) (setq h p))) (setq head\? nil)) (setq p h)) (setq h (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))) nil) (setq l l1)) (if (or (keywordp k) (>= k 0)) (progn (if (null l) (progn (setq l (if (keywordp k) (list k nil) (list nil))) (setq h l))) (let* ((i2 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i2 (1+ i2)))) (_val (if (keywordp (car h)) (car (cdr h)) (car h)))) (if (eq var (y-%key k)) (progn (if (integerp k) (setcar h v) (setcar (cdr h) v)) (throw (quote y-break) l))) (if (null (y-next h)) (progn (if (keywordp k) (nconc h (list k v)) (nconc h (list nil)))))) (setq h (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))) nil))))) (if wipe\? (error (format "Can't wipe index %s of %S" k h)) (aset h k v))) l))))))

(defalias (quote y-wipe) (cons (quote macro) (function (lambda (place) (gv-get place (function (lambda (getter _setter) (if (eq (quote y-get) (car-safe getter)) (list (quote y-set) (car (cdr getter)) (cons (quote y-put) (cdr getter))) (error (format "Can't wipe %S" place))))))))))

(defalias (quote y-length) (function (lambda (h &optional upto) (catch (quote y-break) (if (listp h) (let* ((n -1)) (let* ((i3 -1)) (while h (let* ((k (if (keywordp (car h)) (y-%key (car h)) (setq i3 (1+ i3)))) (_v (if (keywordp (car h)) (car (cdr h)) (car h)))) (if (integerp k) (progn (setq n (max n k)) (if (and upto (>= n upto)) (progn (throw (quote y-break) (+ n 1))))))) (setq h (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))) nil) (+ n 1)) (if (hash-table-p h) (let* ((n -1)) (maphash (function (lambda (k _v) (if (integerp k) (progn (setq n (max n k)) (if (and upto (>= n upto)) (progn (throw (quote y-break) (+ n 1)))))))) h) (+ n 1)) (length h)))))))

(defalias (quote y-%if) (cons (quote macro) (function (lambda (&rest args) (cons (quote if) args)))))

(defalias (quote y-do) (cons (quote macro) (function (lambda (&rest body) (let* ((max-lisp-eval-depth 4450) (max-specpdl-size 13350) (y-environment (apply (quote vector) (append y-environment nil)))) (macroexp-progn (mapcar (quote y-macroexpand) body)))))))
(defvar y-environment (list (make-hash-table :test (quote eq))))

(defalias (quote y-setenv) (function (lambda (k &rest ks) (let* ((i (if (memq :toplevel ks) 0 (- (y-length y-environment) 1)))) (let* ((frame (y-get y-environment i))) (let* ((entry (or (y-get frame k) (make-hash-table :test (quote eq))))) (progn (let* ((o0 ks)) (let* ((f0 (function (lambda (k v) (let* ((k0 k) (v0 v)) (setq entry (y-put entry k0 v0)) v0))))) (progn (if (hash-table-p o0) (maphash f0 o0) (if (listp o0) (let* ((i14 -1)) (while o0 (let* ((k (if (keywordp (car o0)) (y-%key (car o0)) (setq i14 (1+ i14)))) (a0 (if (keywordp (car o0)) (car (cdr o0)) (car o0)))) (funcall f0 k a0)) (setq o0 (if (keywordp (car o0)) (cdr (cdr o0)) (cdr o0)))) nil) (let* ((n0 (y-length o0))) (progn (let* ((k 0)) (progn (while (< k n0) (let* ((a0 (y-get o0 k))) (progn (funcall f0 k a0))) (setq k (+ k 1)))))))))))) (let* ((k1 k) (v1 entry)) (setq frame (y-put frame k1 v1)) v1))))))))
(y-setenv (quote setenv) :symbol (quote y-setenv))

(defalias (quote y-getenv) (function (lambda (k &optional p) (let* ((i (- (y-length y-environment) 1))) (progn (catch (quote y-break) (while (>= i 0) (let* ((b (y-get (y-get y-environment i) k))) (progn (if b (throw (quote y-break) (if p (y-get b p) b)) (setq i (- i 1))))))))))))
(y-setenv (quote getenv) :symbol (quote y-getenv))
(y-setenv (quote unique) :symbol (quote y-unique))
(y-setenv (quote let-unique) :symbol (quote y-let-unique))
(y-setenv (quote at) :symbol (quote get))

(y-setenv (quote set) :macro (function (lambda (&rest args) (cons (quote y-set) args))))

(y-setenv (quote wipe) :macro (function (lambda (&rest args) (cons (quote y-wipe) args))))
(y-setenv (quote get) :symbol (quote y-get))
(y-setenv (quote \#) :symbol (quote y-length))
(y-setenv (quote environment) :symbol (quote y-environment))
(y-setenv (quote %if) :symbol (quote y-%if))

(defalias (quote y-nil-p) (function (lambda (x) (eql x nil))))
(y-setenv (quote nil\?) :symbol (quote y-nil-p))

(defalias (quote y-is-p) (function (lambda (x) (not (y-nil-p x)))))
(y-setenv (quote is\?) :symbol (quote y-is-p))

(defalias (quote y-none-p) (function (lambda (x) (eql (y-length x 0) 0))))
(y-setenv (quote none\?) :symbol (quote y-none-p))

(defalias (quote y-some-p) (function (lambda (x) (> (y-length x 0) 0))))
(y-setenv (quote some\?) :symbol (quote y-some-p))

(defalias (quote y-one-p) (function (lambda (x) (eql (y-length x 1) 1))))
(y-setenv (quote one\?) :symbol (quote y-one-p))

(defalias (quote y-two-p) (function (lambda (x) (eql (y-length x 2) 2))))
(y-setenv (quote two\?) :symbol (quote y-two-p))

(defalias (quote y-hd) (function (lambda (l) (y-get l 0))))
(y-setenv (quote hd) :symbol (quote y-hd))

(defalias (quote y-type) (function (lambda (x) (type-of x))))
(y-setenv (quote type) :symbol (quote y-type))

(defalias (quote y-number-p) (function (lambda (x) (numberp x))))
(y-setenv (quote number\?) :symbol (quote y-number-p))

(defalias (quote y-obj-p) (function (lambda (x) (hash-table-p x))))
(y-setenv (quote obj\?) :symbol (quote y-obj-p))

(y-setenv (quote obj) :macro (function (lambda nil (quote (make-hash-table :test (quote eq))))))

(defalias (quote y-dup) (function (lambda (x) (if (y-obj-p x) (make-hash-table :test (quote eq))))))
(y-setenv (quote dup) :symbol (quote y-dup))

(defalias (quote y-atom-p) (function (lambda (x) (or (y-nil-p x) (symbolp x) (stringp x) (y-number-p x)))))
(y-setenv (quote atom\?) :symbol (quote y-atom-p))

(defalias (quote y-clip) (function (lambda (s from &optional upto) (let* ((n (length s))) (let* ((i (if (or (y-nil-p from) (< from 0)) 0 from))) (let* ((j (if (or (y-nil-p upto) (> upto n)) n (max upto i)))) (progn (substring s i j))))))))
(y-setenv (quote clip) :symbol (quote y-clip))

(defalias (quote y-cut) (function (lambda (x &optional from upto) (let* ((l (y-dup x))) (progn (let* ((j 0)) (let* ((i (if (or (y-nil-p from) (< from 0)) 0 from))) (let* ((n (y-length x))) (let* ((upto (if (or (y-nil-p upto) (> upto n)) n upto))) (progn (while (< i upto) (let* ((k2 j) (v2 (y-get x i))) (setq l (y-put l k2 v2)) v2) (setq i (+ i 1)) (setq j (+ j 1))) (let* ((o1 x)) (let* ((f1 (function (lambda (k v) (if (y-number-p k) nil (let* ((k3 k) (v3 v)) (setq l (y-put l k3 v3)) v3)))))) (progn (if (hash-table-p o1) (maphash f1 o1) (if (listp o1) (let* ((i15 -1)) (while o1 (let* ((k (if (keywordp (car o1)) (y-%key (car o1)) (setq i15 (1+ i15)))) (a1 (if (keywordp (car o1)) (car (cdr o1)) (car o1)))) (funcall f1 k a1)) (setq o1 (if (keywordp (car o1)) (cdr (cdr o1)) (cdr o1)))) nil) (let* ((n1 (y-length o1))) (progn (let* ((k 0)) (progn (while (< k n1) (let* ((a1 (y-get o1 k))) (progn (funcall f1 k a1))) (setq k (+ k 1))))))))))))))))) l)))))
(y-setenv (quote cut) :symbol (quote y-cut))

(defalias (quote y-keys) (function (lambda (x) (let* ((l (y-dup x))) (progn (let* ((o2 x)) (let* ((f2 (function (lambda (k v) (if (y-number-p k) nil (let* ((k4 k) (v4 v)) (setq l (y-put l k4 v4)) v4)))))) (progn (if (hash-table-p o2) (maphash f2 o2) (if (listp o2) (let* ((i16 -1)) (while o2 (let* ((k (if (keywordp (car o2)) (y-%key (car o2)) (setq i16 (1+ i16)))) (a2 (if (keywordp (car o2)) (car (cdr o2)) (car o2)))) (funcall f2 k a2)) (setq o2 (if (keywordp (car o2)) (cdr (cdr o2)) (cdr o2)))) nil) (let* ((n2 (y-length o2))) (progn (let* ((k 0)) (progn (while (< k n2) (let* ((a2 (y-get o2 k))) (progn (funcall f2 k a2))) (setq k (+ k 1)))))))))))) l)))))
(y-setenv (quote keys) :symbol (quote y-keys))

(defalias (quote y-edge) (function (lambda (x) (- (y-length x) 1))))
(y-setenv (quote edge) :symbol (quote y-edge))

(defalias (quote y--chop-p) (function (lambda (x) (and (listp x) (not (keywordp (car x)))))))
(y-setenv (quote chop\?) :symbol (quote y--chop-p))

(defalias (quote y-tl) (function (lambda (l) (if (y--chop-p l) (cdr l) (y-cut l 1)))))
(y-setenv (quote tl) :symbol (quote y-tl))

(defalias (quote y-last) (function (lambda (l) (y-get l (y-edge l)))))
(y-setenv (quote last) :symbol (quote y-last))

(defalias (quote y-almost) (function (lambda (l) (y-cut l 0 (y-edge l)))))
(y-setenv (quote almost) :symbol (quote y-almost))

(y-setenv (quote add) :macro (function (lambda (l x) (progn (or (y-atom-p l) (cl--assertion-failed (quote (y-atom-p l)))) nil) (cons (quote progn) (cons (list (quote set) (list (quote at) l (list (quote \#) l)) x) (quote (nil)))))))

(y-setenv (quote drop) :macro (function (lambda (l) (progn (or (y-atom-p l) (cl--assertion-failed (quote (y-atom-p l)))) nil) (list (quote prog1) (list (quote at) l (list (quote edge) l)) (list (quote set) l (list (quote almost) l))))))

(defalias (quote y-reverse) (function (lambda (l) (let* ((l1 (y-keys l))) (progn (let* ((i (y-edge l))) (progn (while (>= i 0) (progn (let* ((k5 (y-length l1)) (v5 (y-get l i))) (setq l1 (y-put l1 k5 v5)) v5) nil) (setq i (- i 1))))) l1)))))
(y-setenv (quote reverse) :symbol (quote y-reverse))

(defalias (quote y-reduce) (function (lambda (f x) (if (y-none-p x) nil (if (y-one-p x) (y-hd x) (funcall f (y-hd x) (y-reduce f (y-tl x))))))))
(y-setenv (quote reduce) :symbol (quote y-reduce))

(defalias (quote y-join) (function (lambda (&rest ls) (let* ((r (y-dup (y-hd ls)))) (progn (let* ((x0 ls)) (let* ((n3 (y-length x0))) (progn (let* ((i4 0)) (progn (while (< i4 n3) (let* ((l (y-get x0 i4))) (progn (if l (progn (let* ((n (y-length r))) (progn (let* ((o3 l)) (let* ((f3 (function (lambda (k v) (if (y-number-p k) (setq k (+ k n))) (let* ((k6 k) (v6 v)) (setq r (y-put r k6 v6)) v6))))) (progn (if (hash-table-p o3) (maphash f3 o3) (if (listp o3) (let* ((i17 -1)) (while o3 (let* ((k (if (keywordp (car o3)) (y-%key (car o3)) (setq i17 (1+ i17)))) (a3 (if (keywordp (car o3)) (car (cdr o3)) (car o3)))) (funcall f3 k a3)) (setq o3 (if (keywordp (car o3)) (cdr (cdr o3)) (cdr o3)))) nil) (let* ((n4 (y-length o3))) (progn (let* ((k 0)) (progn (while (< k n4) (let* ((a3 (y-get o3 k))) (progn (funcall f3 k a3))) (setq k (+ k 1)))))))))))))))))) (setq i4 (+ i4 1)))))))) r)))))
(y-setenv (quote join) :symbol (quote y-join))

(defalias (quote y-find) (function (lambda (f l) (catch (quote y-break) (let* ((o4 l)) (let* ((f4 (function (lambda (i5 x) (let* ((y (funcall f x))) (progn (if y (throw (quote y-break) y)))))))) (progn (if (hash-table-p o4) (maphash f4 o4) (if (listp o4) (let* ((i18 -1)) (while o4 (let* ((i5 (if (keywordp (car o4)) (y-%key (car o4)) (setq i18 (1+ i18)))) (a4 (if (keywordp (car o4)) (car (cdr o4)) (car o4)))) (funcall f4 i5 a4)) (setq o4 (if (keywordp (car o4)) (cdr (cdr o4)) (cdr o4)))) nil) (let* ((n5 (y-length o4))) (progn (let* ((i5 0)) (progn (while (< i5 n5) (let* ((a4 (y-get o4 i5))) (progn (funcall f4 i5 a4))) (setq i5 (+ i5 1))))))))))))))))
(y-setenv (quote find) :symbol (quote y-find))

(defalias (quote y-first) (function (lambda (f l) (catch (quote y-break) (let* ((x1 l)) (let* ((n6 (y-length x1))) (progn (let* ((i6 0)) (progn (while (< i6 n6) (let* ((x (y-get x1 i6))) (progn (let* ((y (funcall f x))) (progn (if y (throw (quote y-break) y)))))) (setq i6 (+ i6 1))))))))))))
(y-setenv (quote first) :symbol (quote y-first))

(defalias (quote y-in-p) (function (lambda (x l) (y-find (function (lambda (y) (eql x y))) l))))
(y-setenv (quote in\?) :symbol (quote y-in-p))

(defalias (quote y-pair) (function (lambda (l) (let* ((l1 (y-dup l))) (progn (let* ((n (y-length l))) (progn (let* ((i 0)) (progn (while (< i n) (progn (let* ((k7 (y-length l1)) (v7 (list (y-get l i) (y-get l (+ i 1))))) (setq l1 (y-put l1 k7 v7)) v7) nil) (setq i (+ i 1)) (setq i (+ i 1))))))) l1)))))
(y-setenv (quote pair) :symbol (quote y-pair))

(defalias (quote y-map) (function (lambda (f x) (let* ((l (y-dup x))) (progn (let* ((x2 x)) (let* ((n7 (y-length x2))) (progn (let* ((i7 0)) (progn (while (< i7 n7) (let* ((v (y-get x2 i7))) (progn (let* ((y (funcall f v))) (progn (if (y-is-p y) (progn (let* ((k8 (y-length l)) (v8 y)) (setq l (y-put l k8 v8)) v8) nil)))))) (setq i7 (+ i7 1)))))))) (let* ((o5 x)) (let* ((f5 (function (lambda (k v) (if (y-number-p k) nil (let* ((y (funcall f v))) (progn (if (y-is-p y) (progn (let* ((k9 k) (v9 y)) (setq l (y-put l k9 v9)) v9)))))))))) (progn (if (hash-table-p o5) (maphash f5 o5) (if (listp o5) (let* ((i19 -1)) (while o5 (let* ((k (if (keywordp (car o5)) (y-%key (car o5)) (setq i19 (1+ i19)))) (a5 (if (keywordp (car o5)) (car (cdr o5)) (car o5)))) (funcall f5 k a5)) (setq o5 (if (keywordp (car o5)) (cdr (cdr o5)) (cdr o5)))) nil) (let* ((n8 (y-length o5))) (progn (let* ((k 0)) (progn (while (< k n8) (let* ((a5 (y-get o5 k))) (progn (funcall f5 k a5))) (setq k (+ k 1)))))))))))) l)))))
(y-setenv (quote map) :symbol (quote y-map))

(defalias (quote y-keep) (function (lambda (f x) (y-map (function (lambda (v) (if (funcall f v) (progn v)))) x))))
(y-setenv (quote keep) :symbol (quote y-keep))

(defalias (quote y-keys-p) (function (lambda (l) (catch (quote y-break) (let* ((o6 l)) (let* ((f6 (function (lambda (k v) (if (y-number-p k) nil (throw (quote y-break) (quote t))))))) (progn (if (hash-table-p o6) (maphash f6 o6) (if (listp o6) (let* ((i20 -1)) (while o6 (let* ((k (if (keywordp (car o6)) (y-%key (car o6)) (setq i20 (1+ i20)))) (a6 (if (keywordp (car o6)) (car (cdr o6)) (car o6)))) (funcall f6 k a6)) (setq o6 (if (keywordp (car o6)) (cdr (cdr o6)) (cdr o6)))) nil) (let* ((n9 (y-length o6))) (progn (let* ((k 0)) (progn (while (< k n9) (let* ((a6 (y-get o6 k))) (progn (funcall f6 k a6))) (setq k (+ k 1)))))))))))) nil))))
(y-setenv (quote keys\?) :symbol (quote y-keys-p))

(defalias (quote y-empty-p) (function (lambda (l) (catch (quote y-break) (let* ((o7 l)) (let* ((f7 (function (lambda (i8 x) (throw (quote y-break) nil))))) (progn (if (hash-table-p o7) (maphash f7 o7) (if (listp o7) (let* ((i21 -1)) (while o7 (let* ((i8 (if (keywordp (car o7)) (y-%key (car o7)) (setq i21 (1+ i21)))) (a7 (if (keywordp (car o7)) (car (cdr o7)) (car o7)))) (funcall f7 i8 a7)) (setq o7 (if (keywordp (car o7)) (cdr (cdr o7)) (cdr o7)))) nil) (let* ((n10 (y-length o7))) (progn (let* ((i8 0)) (progn (while (< i8 n10) (let* ((a7 (y-get o7 i8))) (progn (funcall f7 i8 a7))) (setq i8 (+ i8 1)))))))))))) (quote t)))))
(y-setenv (quote empty\?) :symbol (quote y-empty-p))

(defalias (quote y-toplevel-p) (function (lambda nil (y-one-p y-environment))))
(y-setenv (quote toplevel\?) :symbol (quote y-toplevel-p))

(defalias (quote y-print) (function (lambda (x) (princ (format "%s
" x)) nil)))
(y-setenv (quote print) :symbol (quote y-print))

(defalias (quote y--id) (function (lambda (x) (let* ((s (append (if (symbolp x) (symbol-name x) x) nil))) (progn (if (eql 63 (y-get s (y-edge s))) (progn (if (memq 45 s) (progn (let* ((k10 (y-edge s)) (v10 45)) (setq s (y-put s k10 v10)) v10) (let* ((k11 (y-length s)) (v11 112)) (setq s (y-put s k11 v11)) v11)) (let* ((k12 (y-edge s)) (v12 112)) (setq s (y-put s k12 v12)) v12)))) (intern (concat s)))))))
(y-setenv (quote id) :symbol (quote y--id))
(defvar y-module nil)
(y-setenv (quote module) :symbol (quote y-module))

(defalias (quote y--module-name) (function (lambda nil (or y-module (let* ((file (or load-file-name (buffer-file-name)))) (progn (if file (file-name-base file) (buffer-name))))))))
(y-setenv (quote module-name) :symbol (quote y--module-name))

(defalias (quote y--global-id) (function (lambda (prefix name) (let* ((s (if (stringp name) name (symbol-name name)))) (progn (if (eql 0 (string-match (regexp-quote prefix) s)) name (y--id (concat prefix s))))))))
(y-setenv (quote global-id) :symbol (quote y--global-id))

(defalias (quote y--macro-function) (function (lambda (k) (y-getenv k (quote macro)))))
(y-setenv (quote macro-function) :symbol (quote y--macro-function))

(defalias (quote y--macro-p) (function (lambda (k) (y--macro-function k))))
(y-setenv (quote macro\?) :symbol (quote y--macro-p))

(defalias (quote y--symbol-expansion) (function (lambda (k) (y-getenv k (quote symbol)))))
(y-setenv (quote symbol-expansion) :symbol (quote y--symbol-expansion))

(defalias (quote y--symbol-p) (function (lambda (k) (let* ((v (y--symbol-expansion k))) (progn (and v (not (eql v k))))))))
(y-setenv (quote symbol\?) :symbol (quote y--symbol-p))

(defalias (quote y--variable-p) (function (lambda (k) (let* ((i (y-edge y-environment))) (progn (catch (quote y-break) (while (>= i 0) (let* ((b (y-get (y-get y-environment i) k))) (progn (if b (throw (quote y-break) (y-get b (quote variable))) (setq i (1- i))))))))))))
(y-setenv (quote variable\?) :symbol (quote y--variable-p))

(defalias (quote y--bound-p) (function (lambda (x) (or (y--macro-p x) (y--symbol-p x) (y--variable-p x)))))
(y-setenv (quote bound\?) :symbol (quote y--bound-p))

(defalias (quote y-bind) (function (lambda (lh rh) (if (y-atom-p lh) (list lh rh) (let* ((var (y-unique (quote var)))) (let* ((bs (list var rh))) (progn (let* ((o8 lh)) (let* ((f8 (function (lambda (k v) (let* ((x (if (eql k (quote rest)) (list (quote cut) var (y-length lh)) (list (quote get) var (list (quote quote) k))))) (progn (if (y-is-p k) (progn (let* ((k (if (eql v (quote t)) k v))) (progn (setq bs (y-join bs (y-bind k x))))))))))))) (progn (if (hash-table-p o8) (maphash f8 o8) (if (listp o8) (let* ((i22 -1)) (while o8 (let* ((k (if (keywordp (car o8)) (y-%key (car o8)) (setq i22 (1+ i22)))) (a8 (if (keywordp (car o8)) (car (cdr o8)) (car o8)))) (funcall f8 k a8)) (setq o8 (if (keywordp (car o8)) (cdr (cdr o8)) (cdr o8)))) nil) (let* ((n11 (y-length o8))) (progn (let* ((k 0)) (progn (while (< k n11) (let* ((a8 (y-get o8 k))) (progn (funcall f8 k a8))) (setq k (+ k 1)))))))))))) bs)))))))
(y-setenv (quote bind) :symbol (quote y-bind))

(defalias (quote y-bind*) (function (lambda (args body) (if (and args (y-atom-p args)) (y-bind* (list (quote &rest) args) body) (let* ((rest nil)) (let* ((args1 nil)) (let* ((bs nil)) (let* ((ks nil)) (progn (let* ((i 0)) (progn (while (< i (y-length args)) (let* ((arg (y-get args i))) (progn (if (eql arg (quote &rest)) (setq rest (y-get args (setq i (+ i 1)))) (if (y-atom-p arg) (progn (let* ((k13 (y-length args1)) (v13 arg)) (setq args1 (y-put args1 k13 v13)) v13) nil) (let* ((id1 (y-unique (quote id1)))) (progn (let* ((k14 (y-length args1)) (v14 id1)) (setq args1 (y-put args1 k14 v14)) v14) nil) (setq bs (y-join bs (list arg id1)))))))) (setq i (+ i 1))))) (let* ((y0 (y-get args (quote rest)))) (progn (if y0 (progn (let* ((x y0)) (progn (setq rest x))))))) (let* ((o9 args)) (let* ((f9 (function (lambda (k v) (if (or (y-number-p k) (eql k (quote rest))) nil (if (y-nil-p rest) (progn (setq rest (y-unique (quote args))))) (let* ((v1 (if (eql v (quote t)) k v))) (let* ((k1 (intern (format ":%s" k)))) (progn (setq ks (y-join ks (list k1 v1))))))))))) (progn (if (hash-table-p o9) (maphash f9 o9) (if (listp o9) (let* ((i23 -1)) (while o9 (let* ((k (if (keywordp (car o9)) (y-%key (car o9)) (setq i23 (1+ i23)))) (a9 (if (keywordp (car o9)) (car (cdr o9)) (car o9)))) (funcall f9 k a9)) (setq o9 (if (keywordp (car o9)) (cdr (cdr o9)) (cdr o9)))) nil) (let* ((n12 (y-length o9))) (progn (let* ((k 0)) (progn (while (< k n12) (let* ((a9 (y-get o9 k))) (progn (funcall f9 k a9))) (setq k (+ k 1)))))))))))) (if rest (progn (progn (let* ((k15 (y-length args1)) (v15 (quote &rest))) (setq args1 (y-put args1 k15 v15)) v15) nil) (progn (let* ((k16 (y-length args1)) (v16 rest)) (setq args1 (y-put args1 k16 v16)) v16) nil))) (if ks (progn (setq bs (y-join bs (list ks rest))))) (let* ((l (list args1))) (progn (let* ((x (y-hd body))) (progn (if (stringp x) (progn (progn (let* ((k17 (y-length l)) (v17 x)) (setq l (y-put l k17 v17)) v17) nil) (setq body (y-tl body)))))) (if (y-is-p bs) (progn (let* ((k18 (y-length l)) (v18 (cons (quote let) (cons bs body)))) (setq l (y-put l k18 v18)) v18) nil) (setq l (y-join l body))) l)))))))))))
(y-setenv (quote bind*) :symbol (quote y-bind*))

(defalias (quote y-macroexpand) (function (lambda (form) (let* ((s (y--symbol-expansion form))) (progn (if s (y-macroexpand s) (if (atom form) form (let* ((x (y-macroexpand (y-hd form)))) (progn (if (eql x (quote quote)) form (if (eql x (quote \`)) (y-macroexpand (funcall (quote macroexpand) form)) (if (y--macro-p x) (y-macroexpand (apply (y--macro-function x) (y-tl form))) (cons x (mapcar (quote y-macroexpand) (y-tl form)))))))))))))))
(y-setenv (quote macroexpand) :symbol (quote y-macroexpand))

(defalias (quote y-eval) (function (lambda (form) (let* ((max-lisp-eval-depth 4450) (max-specpdl-size 13350)) (funcall (quote eval) (y-macroexpand form) (quote t))))))
(y-setenv (quote eval) :symbol (quote y-eval))

(y-setenv (quote =) :macro (function (lambda (&rest args) (if (y-find (quote stringp) args) (cons (quote string=) args) (cons (quote eql) args)))))

(defalias (quote y--expand-if) (function (lambda (id10) (let* ((var0 id10)) (let* ((a (y-get var0 (quote 0)))) (let* ((b (y-get var0 (quote 1)))) (let* ((c (y-cut var0 2))) (progn (if (y-some-p c) (list (cons (quote %if) (cons a (cons b (y--expand-if c))))) (if (y-is-p b) (list (list (quote %if) a b)) (if (y-is-p a) (list a))))))))))))
(y-setenv (quote expand-if) :symbol (quote y--expand-if))

(y-setenv (quote if) :macro (function (lambda (&rest branches) (y-hd (y--expand-if branches)))))

(y-setenv (quote with) :macro (function (lambda (x v &rest body) (cons (quote let) (cons (list x v) (append body (list x)))))))
(function-put (quote with) (quote lisp-indent-function) (quote defun))

(y-setenv (quote let-when) :macro (function (lambda (x v &rest body) (let* ((y (y-unique (quote y)))) (list (quote let) y v (list (quote when) y (cons (quote let) (cons (list x y) body))))))))
(function-put (quote let-when) (quote lisp-indent-function) (quote defun))

(y-setenv (quote fn) :macro (function (lambda (args &rest body) (cons (quote lambda) (y-bind* args body)))))
(function-put (quote fn) (quote lisp-indent-function) (quote defun))

(defalias (quote y--body-macro-p) (function (lambda (args) (eql (quote body) (if (y-atom-p args) args (car (funcall (quote last) args)))))))
(y-setenv (quote body-macro\?) :symbol (quote y--body-macro-p))

(defalias (quote y--body-indentation) (function (lambda (name args form) (if (y--body-macro-p args) (list (quote prog1) form (cons (quote function-put) (cons (list (quote quote) name) (quote ((quote lisp-indent-function) (quote defun)))))) form))))
(y-setenv (quote body-indentation) :symbol (quote y--body-indentation))

(y-setenv (quote define-macro) :macro (function (lambda (name args &rest body) (let* ((form (list (quote setenv) (list (quote quote) name) (quote :macro) (cons (quote fn) (cons args body)))) (form (y--body-indentation name args form))) (y-eval form) form))))
(function-put (quote define-macro) (quote lisp-indent-function) (quote defun))

(y-setenv (quote define-symbol) :macro (function (lambda (name expansion) (y-setenv name :symbol expansion) (list (quote setenv) (list (quote quote) name) (quote :symbol) (list (quote quote) expansion)))))

(defalias (quote y--expand-definition) (function (lambda (name x body global\?) (let* ((sep (if global\? "-" "--"))) (let* ((name1 (y--global-id (concat (y--module-name) sep) name))) (let* ((body1 (y-keep (function (lambda (x) (not (stringp x)))) body))) (let* ((fn\? (y-some-p body1))) (let* ((var (if global\? (quote defvar) (quote defconst)))) (progn (if global\? (y-setenv name1 :variable (quote t) :toplevel (quote t)) (y-setenv name1 :variable (quote t))) (y-setenv name :symbol name1) (list (quote prog1) (if fn\? (list (quote defalias) (list (quote quote) name1) (cons (quote fn) (cons x body))) (cons var (cons name1 (cons x body)))) (list (quote setenv) (list (quote quote) name) (quote :symbol) (list (quote quote) name1))))))))))))
(y-setenv (quote expand-definition) :symbol (quote y--expand-definition))

(y-setenv (quote define) :macro (function (lambda (name &optional x &rest body) (y--expand-definition name x body nil))))
(function-put (quote define) (quote lisp-indent-function) (quote defun))

(y-setenv (quote define-global) :macro (function (lambda (name &optional x &rest body) (y--expand-definition name x body (quote t)))))
(function-put (quote define-global) (quote lisp-indent-function) (quote defun))

(y-setenv (quote with-frame) :macro (function (lambda (&rest body) (let* ((x (y-unique (quote x)))) (list (quote progn) (quote (set environment (apply (quote vector) (append environment (list (obj)))))) (cons (quote with) (cons x (cons (cons (quote progn) body) (quote ((set environment (apply (quote vector) (almost environment)))))))))))))
(function-put (quote with-frame) (quote lisp-indent-function) (quote defun))

(y-setenv (quote let-macro) :macro (function (lambda (definitions &rest body) (progn (setq y-environment (apply (quote vector) (append y-environment (list (make-hash-table :test (quote eq)))))) (let* ((x5 (progn (let* ((x6 definitions)) (let* ((n14 (y-length x6))) (progn (let* ((i10 0)) (progn (while (< i10 n14) (let* ((m (y-get x6 i10))) (progn (y-macroexpand (cons (quote define-macro) m)))) (setq i10 (+ i10 1)))))))) (cons (quote progn) (y-macroexpand body))))) (progn (setq y-environment (apply (quote vector) (y-almost y-environment))) x5))))))
(function-put (quote let-macro) (quote lisp-indent-function) (quote defun))

(y-setenv (quote let-symbol) :macro (function (lambda (expansions &rest body) (if (y-none-p expansions) (cons (quote progn) (y-macroexpand body)) (progn (setq y-environment (apply (quote vector) (append y-environment (list (make-hash-table :test (quote eq)))))) (let* ((x9 (progn (let* ((x10 (y-pair expansions))) (let* ((n16 (y-length x10))) (progn (let* ((i12 0)) (progn (while (< i12 n16) (let* ((x (y-get x10 i12))) (progn (y-macroexpand (cons (quote define-symbol) x)))) (setq i12 (+ i12 1)))))))) (cons (quote progn) (y-macroexpand body))))) (progn (setq y-environment (apply (quote vector) (y-almost y-environment))) x9)))))))
(function-put (quote let-symbol) (quote lisp-indent-function) (quote defun))

(y-setenv (quote when-compiling) :macro (function (lambda (&rest body) (y-eval (cons (quote progn) body)))))
(function-put (quote when-compiling) (quote lisp-indent-function) (quote defun))

(y-setenv (quote let) :macro (function (lambda (bs &rest body) (if (and bs (atom bs)) (cons (quote let) (cons (list bs (y-hd body)) (y-tl body))) (if (y-none-p bs) (cons (quote progn) body) (let* ((var3 bs)) (let* ((lh (y-get var3 (quote 0)))) (let* ((rh (y-get var3 (quote 1)))) (let* ((bs2 (y-cut var3 2))) (let* ((var4 (y-bind lh rh))) (let* ((var (y-get var4 (quote 0)))) (let* ((val (y-get var4 (quote 1)))) (let* ((bs1 (y-cut var4 2))) (progn (let* ((form (cons (quote let) (cons (y-join bs1 bs2) body)))) (progn (list (quote let*) (list (list var val)) (y-macroexpand form))))))))))))))))))
(function-put (quote let) (quote lisp-indent-function) (quote defun))

(y-setenv (quote join!) :macro (function (lambda (a &rest bs) (list (quote set) a (cons (quote join) (cons a bs))))))

(y-setenv (quote inc) :macro (function (lambda (n &optional by) (list (quote set) n (list (quote +) n (or by 1))))))

(y-setenv (quote dec) :macro (function (lambda (n &optional by) (list (quote set) n (list (quote -) n (or by 1))))))

(y-setenv (quote for) :macro (function (lambda (i to &rest body) (list (quote let) i 0 (cons (quote while) (cons (list (quote <) i to) (append body (list (list (quote inc) i)))))))))
(function-put (quote for) (quote lisp-indent-function) (quote defun))

(y-setenv (quote step) :macro (function (lambda (v l &rest body) (let* ((x (y-unique (quote x))) (n (y-unique (quote n))) (i (y-unique (quote i)))) (list (quote let) (list x l n (list (quote \#) x)) (list (quote for) i n (cons (quote let) (cons (list v (list (quote at) x i)) body))))))))
(function-put (quote step) (quote lisp-indent-function) (quote defun))

(y-setenv (quote each) :macro (function (lambda (x l &rest body) (let* ((o (y-unique (quote o))) (n (y-unique (quote n))) (f (y-unique (quote f))) (a (y-unique (quote a)))) (let* ((var6 (if (y-atom-p x) (list (y-unique (quote i)) x) (if (> (y-length x) 1) x (list (y-unique (quote i)) (y-hd x)))))) (let* ((k (y-get var6 (quote 0)))) (let* ((v (y-get var6 (quote 1)))) (progn (list (quote let) (list o l f (cons (quote fn) (cons (list k v) body))) (list (quote if) (list (quote hash-table-p) o) (list (quote maphash) f o) (list (quote listp) o) (list (quote y-%for) o k a (list (quote funcall) f k a)) (list (quote let) n (list (quote \#) o) (list (quote for) k n (list (quote let) a (list (quote at) o k) (list (quote funcall) f k a))))))))))))))
(function-put (quote each) (quote lisp-indent-function) (quote defun))

(defalias (quote y--eval-print) (function (lambda (form) (condition-case err (let* ((x (y-eval form))) (progn (if (y-is-p x) (y-print (format "%S" x))))) (error (y-print (format "error: %s" (error-message-string err))))))))
(y-setenv (quote eval-print) :symbol (quote y--eval-print))

(defalias (quote y--read-string) (function (lambda (s &optional more) (if more (condition-case nil (car (read-from-string s)) (end-of-file more)) (car (read-from-string s))))))
(y-setenv (quote read-string) :symbol (quote y--read-string))

(defalias (quote y--rep) (function (lambda (s) (y--eval-print (y--read-string s)))))
(y-setenv (quote rep) :symbol (quote y--rep))

(defalias (quote y--repl) (function (lambda nil (let* ((buf "")) (progn (let* ((rep1 (function (lambda (s) (setq buf (concat buf s)) (let* ((more (make-hash-table :test (quote eq)))) (let* ((form (y--read-string buf more))) (progn (if (eql form more) nil (y--eval-print form) (setq buf "") (princ "> "))))))))) (progn (princ "> ") (catch (quote y-break) (while (quote t) (let* ((s (read-from-minibuffer ""))) (progn (if (and s (not (string= s ":a"))) (funcall rep1 (concat s "
")) (throw (quote y-break) nil)))))))))))))
(y-setenv (quote repl) :symbol (quote y--repl))

(defalias (quote y-compile-toplevel) (function (lambda (form) (let* ((var7 (if (y-atom-p form) (list) form))) (let* ((x (y-get var7 (quote 0)))) (let* ((y--macro-p (y-get var7 (quote macro)))) (progn (if (eql x (quote defalias)) (terpri) (if (eql x (quote y-setenv)) (if y--macro-p (terpri)))) (if (eql x (quote progn)) (y-map (function y-compile-toplevel) (y-tl form)) (if (eql x (quote prog1)) (y-map (function y-compile-toplevel) (y-tl form)) (prog1 (prin1 form) (terpri)))))))))))
(y-setenv (quote compile-toplevel) :symbol (quote y-compile-toplevel))

(defalias (quote y-compile-file) (function (lambda (path &optional y--module-name) (let* ((name (or y--module-name (file-name-base path))) (y-module name) (forms (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert-file-contents-literally path) (car (read-from-string (concat "(" (buffer-string) ")")))) (and (buffer-name temp-buffer) (kill-buffer temp-buffer)))))) (exprs (funcall (quote macroexpand-all) (cons (quote progn) forms)))) (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert ";;; -*- lexical-binding: t -*-
") (insert (let ((standard-output (get-buffer-create (generate-new-buffer-name " *string-output*")))) (unwind-protect (progn (let ((standard-output standard-output)) (y-compile-toplevel exprs)) (save-current-buffer (set-buffer standard-output) (buffer-string))) (kill-buffer standard-output)))) (untabify (point-min) (point-max)) (buffer-string)) (and (buffer-name temp-buffer) (kill-buffer temp-buffer)))))))))
(y-setenv (quote compile-file) :symbol (quote y-compile-file))

(defalias (quote y-write-file) (function (lambda (path data) (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert (replace-regexp-in-string "" "" data)) (write-region (point-min) (point-max) path nil t)) (and (buffer-name temp-buffer) (kill-buffer temp-buffer))))))))
(y-setenv (quote write-file) :symbol (quote y-write-file))

(defalias (quote y--run-file) (function (lambda (path) (funcall (quote load-file) path))))
(y-setenv (quote run-file) :symbol (quote y--run-file))

(defalias (quote y--usage) (function (lambda nil (y-print "usage: y [options] <object files>") (y-print "options:") (y-print "  -c <input>      Compile input file") (y-print "  -o <output>    Output file") (y-print "  -e <expr>     Expression to evaluate"))))
(y-setenv (quote usage) :symbol (quote y--usage))

(defalias (quote y-main) (function (lambda (args) (let* ((arg (y-hd args))) (progn (if (or (string= arg "-h") (string= arg "--help")) (y--usage) (let* ((pre nil)) (let* ((input nil)) (let* ((output nil)) (let* ((target1 nil)) (let* ((expr nil)) (let* ((n (y-length args))) (progn (let* ((i 0)) (progn (while (< i n) (let* ((a (y-get args i))) (progn (if (or (string= a "-c") (string= a "-o") (string= a "-e")) (if (eql i (- n 1)) (y-print (format "missing argument for %S" a)) (progn (setq i (+ i 1)) (let* ((val (y-get args i))) (progn (if (string= a "-c") (setq input val) (if (string= a "-o") (setq output val) (if (string= a "-e") (setq expr val)))))))) (progn (let* ((k19 (y-length pre)) (v19 a)) (setq pre (y-put pre k19 v19)) v19) nil)))) (setq i (+ i 1))))) (let* ((x11 pre)) (let* ((n17 (y-length x11))) (progn (let* ((i13 0)) (progn (while (< i13 n17) (let* ((file (y-get x11 i13))) (progn (y--run-file file))) (setq i13 (+ i13 1)))))))) (if (y-nil-p input) (if expr (y--rep expr) (y--repl)) (let* ((code (y-compile-file input))) (progn (if (or (y-nil-p output) (string= output "-")) (y-print code) (y-write-file output code))))))))))))))))))
(y-setenv (quote main) :symbol (quote y-main))
(if noninteractive (progn (let* ((args command-line-args-left)) (progn (setq command-line-args-left nil) (y-main args)))))
(provide (quote y))
