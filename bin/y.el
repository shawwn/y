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

(defalias (quote y-get) (function (lambda (h k) (if (hash-table-p h) (gethash k h) (if (listp h) (catch (quote y-break) (let* ((i24 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i24 (1+ i24)))) (val (if (keywordp (car h)) (car (cdr h)) (car h)))) (if (eq var k) (progn (throw (quote y-break) val)))) (setq h (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))) nil)) (elt h k))))))
(quote byte-compile-inline-expand)
(put (quote y-get) (quote gv-expander) (function (lambda (f place idx) (gv-get place (function (lambda (getter _setter) (funcall f (list (quote y-get) getter idx) (function (lambda (val) (let* ((k (y-unique (quote k))) (v (y-unique (quote v)))) (list (quote let*) (list (list k idx) (list v val)) (list (quote y-set) getter (list (quote y-put) getter k v)) v)))))))))))

(defalias (quote y-put) (function (lambda (h k &rest args) (let ((v (car args)) (wipe\? (null args))) (if (hash-table-p h) (progn (setq k (y-%key k)) (if wipe\? (if (integerp k) (let ((n (y-length h)) (i k) (unset (list nil))) (if (and (>= i 0) (<= i (1- n))) (progn (while (< i n) (let ((x (gethash (1+ i) h unset))) (if (eq x unset) (remhash i h) (puthash i x h))) (setq i (1+ i))) (remhash i h)) (remhash k h))) (remhash k h)) (puthash k v h)) h) (let* ((l h)) (if (and (symbolp k) (not (keywordp k))) (setq k (intern (concat ":" (symbol-name k))))) (if (listp h) (catch (quote y-break) (if wipe\? (let ((l1 h) (p nil) (head\? t)) (let* ((i25 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i25 (1+ i25)))) (_val (if (keywordp (car h)) (car (cdr h)) (car h)))) (if (eq var k) (if head\? (progn (setq l1 (if (keywordp k) (cdr (cdr h)) (cdr h))) (setq h l1)) (progn (setcdr (if (keywordp (car p)) (cdr p) p) (if (keywordp k) (cdr (cdr h)) (cdr h))) (setq h p))) (setq head\? nil)) (setq p h)) (setq h (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))) nil) (setq l l1)) (if (or (keywordp k) (>= k 0)) (progn (if (null l) (progn (setq l (if (keywordp k) (list k nil) (list nil))) (setq h l))) (let* ((i26 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i26 (1+ i26)))) (_val (if (keywordp (car h)) (car (cdr h)) (car h)))) (if (eq var (y-%key k)) (progn (if (integerp k) (setcar h v) (setcar (cdr h) v)) (throw (quote y-break) l))) (if (null (y-next h)) (progn (if (keywordp k) (nconc h (list k v)) (nconc h (list nil)))))) (setq h (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))) nil))))) (if wipe\? (error (format "Can't wipe index %s of %S" k h)) (aset h k v))) l))))))

(defalias (quote y-wipe) (cons (quote macro) (function (lambda (place) (gv-get place (function (lambda (getter _setter) (if (eq (quote y-get) (car-safe getter)) (list (quote y-set) (car (cdr getter)) (cons (quote y-put) (cdr getter))) (error (format "Can't wipe %S" place))))))))))

(defalias (quote y-length) (function (lambda (h &optional upto) (catch (quote y-break) (if (listp h) (let* ((n -1)) (let* ((i27 -1)) (while h (let* ((k (if (keywordp (car h)) (y-%key (car h)) (setq i27 (1+ i27)))) (_v (if (keywordp (car h)) (car (cdr h)) (car h)))) (if (integerp k) (progn (setq n (max n k)) (if (and upto (>= n upto)) (progn (throw (quote y-break) (+ n 1))))))) (setq h (if (keywordp (car h)) (cdr (cdr h)) (cdr h)))) nil) (+ n 1)) (if (hash-table-p h) (let* ((n -1)) (maphash (function (lambda (k _v) (if (integerp k) (progn (setq n (max n k)) (if (and upto (>= n upto)) (progn (throw (quote y-break) (+ n 1)))))))) h) (+ n 1)) (length h)))))))

(defalias (quote y-%if) (cons (quote macro) (function (lambda (&rest args) (cons (quote if) args)))))

(defalias (quote y-do) (cons (quote macro) (function (lambda (&rest body) (let* ((max-lisp-eval-depth 4450) (max-specpdl-size 13350) (y-environment (apply (quote vector) (append y-environment nil)))) (macroexp-progn (mapcar (quote y-macroexpand) body)))))))
(defvar y-environment (list (make-hash-table :test (quote eq))))

(defalias (quote y-setenv) (function (lambda (k &rest ks) (let* ((i (if (memq :toplevel ks) 0 (- (y-length y-environment) 1)))) (let* ((frame (y-get y-environment i))) (let* ((entry (or (y-get frame k) (make-hash-table :test (quote eq))))) (progn (let* ((o10 ks)) (let* ((f10 (function (lambda (k v) (let* ((k20 k) (v20 v)) (setq entry (y-put entry k20 v20)) v20))))) (progn (if (hash-table-p o10) (maphash f10 o10) (if (listp o10) (let* ((i38 -1)) (while o10 (let* ((k (if (keywordp (car o10)) (y-%key (car o10)) (setq i38 (1+ i38)))) (a10 (if (keywordp (car o10)) (car (cdr o10)) (car o10)))) (funcall f10 k a10)) (setq o10 (if (keywordp (car o10)) (cdr (cdr o10)) (cdr o10)))) nil) (let* ((n18 (y-length o10))) (progn (let* ((k 0)) (progn (while (< k n18) (let* ((a10 (y-get o10 k))) (progn (funcall f10 k a10))) (setq k (+ k 1)))))))))))) (let* ((k21 k) (v21 entry)) (setq frame (y-put frame k21 v21)) v21))))))))
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

(defalias (quote y-cut) (function (lambda (x &optional from upto) (let* ((l (y-dup x))) (progn (let* ((j 0)) (let* ((i (if (or (y-nil-p from) (< from 0)) 0 from))) (let* ((n (y-length x))) (let* ((upto (if (or (y-nil-p upto) (> upto n)) n upto))) (progn (while (< i upto) (let* ((k22 j) (v22 (y-get x i))) (setq l (y-put l k22 v22)) v22) (setq i (+ i 1)) (setq j (+ j 1))) (let* ((o11 x)) (let* ((f11 (function (lambda (k v) (if (y-number-p k) nil (let* ((k23 k) (v23 v)) (setq l (y-put l k23 v23)) v23)))))) (progn (if (hash-table-p o11) (maphash f11 o11) (if (listp o11) (let* ((i39 -1)) (while o11 (let* ((k (if (keywordp (car o11)) (y-%key (car o11)) (setq i39 (1+ i39)))) (a11 (if (keywordp (car o11)) (car (cdr o11)) (car o11)))) (funcall f11 k a11)) (setq o11 (if (keywordp (car o11)) (cdr (cdr o11)) (cdr o11)))) nil) (let* ((n19 (y-length o11))) (progn (let* ((k 0)) (progn (while (< k n19) (let* ((a11 (y-get o11 k))) (progn (funcall f11 k a11))) (setq k (+ k 1))))))))))))))))) l)))))
(y-setenv (quote cut) :symbol (quote y-cut))

(defalias (quote y-keys) (function (lambda (x) (let* ((l (y-dup x))) (progn (let* ((o12 x)) (let* ((f12 (function (lambda (k v) (if (y-number-p k) nil (let* ((k24 k) (v24 v)) (setq l (y-put l k24 v24)) v24)))))) (progn (if (hash-table-p o12) (maphash f12 o12) (if (listp o12) (let* ((i40 -1)) (while o12 (let* ((k (if (keywordp (car o12)) (y-%key (car o12)) (setq i40 (1+ i40)))) (a12 (if (keywordp (car o12)) (car (cdr o12)) (car o12)))) (funcall f12 k a12)) (setq o12 (if (keywordp (car o12)) (cdr (cdr o12)) (cdr o12)))) nil) (let* ((n20 (y-length o12))) (progn (let* ((k 0)) (progn (while (< k n20) (let* ((a12 (y-get o12 k))) (progn (funcall f12 k a12))) (setq k (+ k 1)))))))))))) l)))))
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

(defalias (quote y-reverse) (function (lambda (l) (let* ((l1 (y-keys l))) (progn (let* ((i (y-edge l))) (progn (while (>= i 0) (progn (let* ((k25 (y-length l1)) (v25 (y-get l i))) (setq l1 (y-put l1 k25 v25)) v25) nil) (setq i (- i 1))))) l1)))))
(y-setenv (quote reverse) :symbol (quote y-reverse))

(defalias (quote y-reduce) (function (lambda (f x) (if (y-none-p x) nil (if (y-one-p x) (y-hd x) (funcall f (y-hd x) (y-reduce f (y-tl x))))))))
(y-setenv (quote reduce) :symbol (quote y-reduce))

(defalias (quote y-join) (function (lambda (&rest ls) (let* ((r (y-dup (y-hd ls)))) (progn (let* ((x12 ls)) (let* ((n21 (y-length x12))) (progn (let* ((i28 0)) (progn (while (< i28 n21) (let* ((l (y-get x12 i28))) (progn (if l (progn (let* ((n (y-length r))) (progn (let* ((o13 l)) (let* ((f13 (function (lambda (k v) (if (y-number-p k) (setq k (+ k n))) (let* ((k26 k) (v26 v)) (setq r (y-put r k26 v26)) v26))))) (progn (if (hash-table-p o13) (maphash f13 o13) (if (listp o13) (let* ((i41 -1)) (while o13 (let* ((k (if (keywordp (car o13)) (y-%key (car o13)) (setq i41 (1+ i41)))) (a13 (if (keywordp (car o13)) (car (cdr o13)) (car o13)))) (funcall f13 k a13)) (setq o13 (if (keywordp (car o13)) (cdr (cdr o13)) (cdr o13)))) nil) (let* ((n22 (y-length o13))) (progn (let* ((k 0)) (progn (while (< k n22) (let* ((a13 (y-get o13 k))) (progn (funcall f13 k a13))) (setq k (+ k 1)))))))))))))))))) (setq i28 (+ i28 1)))))))) r)))))
(y-setenv (quote join) :symbol (quote y-join))

(defalias (quote y-find) (function (lambda (f l) (catch (quote y-break) (let* ((o14 l)) (let* ((f14 (function (lambda (i29 x) (let* ((y (funcall f x))) (progn (if y (throw (quote y-break) y)))))))) (progn (if (hash-table-p o14) (maphash f14 o14) (if (listp o14) (let* ((i42 -1)) (while o14 (let* ((i29 (if (keywordp (car o14)) (y-%key (car o14)) (setq i42 (1+ i42)))) (a14 (if (keywordp (car o14)) (car (cdr o14)) (car o14)))) (funcall f14 i29 a14)) (setq o14 (if (keywordp (car o14)) (cdr (cdr o14)) (cdr o14)))) nil) (let* ((n23 (y-length o14))) (progn (let* ((i29 0)) (progn (while (< i29 n23) (let* ((a14 (y-get o14 i29))) (progn (funcall f14 i29 a14))) (setq i29 (+ i29 1))))))))))))))))
(y-setenv (quote find) :symbol (quote y-find))

(defalias (quote y-first) (function (lambda (f l) (catch (quote y-break) (let* ((x13 l)) (let* ((n24 (y-length x13))) (progn (let* ((i30 0)) (progn (while (< i30 n24) (let* ((x (y-get x13 i30))) (progn (let* ((y (funcall f x))) (progn (if y (throw (quote y-break) y)))))) (setq i30 (+ i30 1))))))))))))
(y-setenv (quote first) :symbol (quote y-first))

(defalias (quote y-in-p) (function (lambda (x l) (y-find (function (lambda (y) (eql x y))) l))))
(y-setenv (quote in\?) :symbol (quote y-in-p))

(defalias (quote y-pair) (function (lambda (l) (let* ((l1 (y-dup l))) (progn (let* ((n (y-length l))) (progn (let* ((i 0)) (progn (while (< i n) (progn (let* ((k27 (y-length l1)) (v27 (list (y-get l i) (y-get l (+ i 1))))) (setq l1 (y-put l1 k27 v27)) v27) nil) (setq i (+ i 1)) (setq i (+ i 1))))))) l1)))))
(y-setenv (quote pair) :symbol (quote y-pair))

(defalias (quote y-map) (function (lambda (f x) (let* ((l (y-dup x))) (progn (let* ((x14 x)) (let* ((n25 (y-length x14))) (progn (let* ((i31 0)) (progn (while (< i31 n25) (let* ((v (y-get x14 i31))) (progn (let* ((y (funcall f v))) (progn (if (y-is-p y) (progn (let* ((k28 (y-length l)) (v28 y)) (setq l (y-put l k28 v28)) v28) nil)))))) (setq i31 (+ i31 1)))))))) (let* ((o15 x)) (let* ((f15 (function (lambda (k v) (if (y-number-p k) nil (let* ((y (funcall f v))) (progn (if (y-is-p y) (progn (let* ((k29 k) (v29 y)) (setq l (y-put l k29 v29)) v29)))))))))) (progn (if (hash-table-p o15) (maphash f15 o15) (if (listp o15) (let* ((i43 -1)) (while o15 (let* ((k (if (keywordp (car o15)) (y-%key (car o15)) (setq i43 (1+ i43)))) (a15 (if (keywordp (car o15)) (car (cdr o15)) (car o15)))) (funcall f15 k a15)) (setq o15 (if (keywordp (car o15)) (cdr (cdr o15)) (cdr o15)))) nil) (let* ((n26 (y-length o15))) (progn (let* ((k 0)) (progn (while (< k n26) (let* ((a15 (y-get o15 k))) (progn (funcall f15 k a15))) (setq k (+ k 1)))))))))))) l)))))
(y-setenv (quote map) :symbol (quote y-map))

(defalias (quote y-keep) (function (lambda (f x) (y-map (function (lambda (v) (if (funcall f v) (progn v)))) x))))
(y-setenv (quote keep) :symbol (quote y-keep))

(defalias (quote y-keys-p) (function (lambda (l) (catch (quote y-break) (let* ((o16 l)) (let* ((f16 (function (lambda (k v) (if (y-number-p k) nil (throw (quote y-break) (quote t))))))) (progn (if (hash-table-p o16) (maphash f16 o16) (if (listp o16) (let* ((i44 -1)) (while o16 (let* ((k (if (keywordp (car o16)) (y-%key (car o16)) (setq i44 (1+ i44)))) (a16 (if (keywordp (car o16)) (car (cdr o16)) (car o16)))) (funcall f16 k a16)) (setq o16 (if (keywordp (car o16)) (cdr (cdr o16)) (cdr o16)))) nil) (let* ((n27 (y-length o16))) (progn (let* ((k 0)) (progn (while (< k n27) (let* ((a16 (y-get o16 k))) (progn (funcall f16 k a16))) (setq k (+ k 1)))))))))))) nil))))
(y-setenv (quote keys\?) :symbol (quote y-keys-p))

(defalias (quote y-empty-p) (function (lambda (l) (catch (quote y-break) (let* ((o17 l)) (let* ((f17 (function (lambda (i32 x) (throw (quote y-break) nil))))) (progn (if (hash-table-p o17) (maphash f17 o17) (if (listp o17) (let* ((i45 -1)) (while o17 (let* ((i32 (if (keywordp (car o17)) (y-%key (car o17)) (setq i45 (1+ i45)))) (a17 (if (keywordp (car o17)) (car (cdr o17)) (car o17)))) (funcall f17 i32 a17)) (setq o17 (if (keywordp (car o17)) (cdr (cdr o17)) (cdr o17)))) nil) (let* ((n28 (y-length o17))) (progn (let* ((i32 0)) (progn (while (< i32 n28) (let* ((a17 (y-get o17 i32))) (progn (funcall f17 i32 a17))) (setq i32 (+ i32 1)))))))))))) (quote t)))))
(y-setenv (quote empty\?) :symbol (quote y-empty-p))

(defalias (quote y-toplevel-p) (function (lambda nil (y-one-p y-environment))))
(y-setenv (quote toplevel\?) :symbol (quote y-toplevel-p))

(defalias (quote y-print) (function (lambda (x) (princ (format "%s
" x)) nil)))
(y-setenv (quote print) :symbol (quote y-print))

(defalias (quote y--id) (function (lambda (x) (let* ((s (append (if (symbolp x) (symbol-name x) x) nil))) (progn (if (eql 63 (y-get s (y-edge s))) (progn (if (memq 45 s) (progn (let* ((k30 (y-edge s)) (v30 45)) (setq s (y-put s k30 v30)) v30) (let* ((k31 (y-length s)) (v31 112)) (setq s (y-put s k31 v31)) v31)) (let* ((k32 (y-edge s)) (v32 112)) (setq s (y-put s k32 v32)) v32)))) (intern (concat s)))))))
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

(defalias (quote y-bind) (function (lambda (lh rh) (if (y-atom-p lh) (list lh rh) (let* ((var (y-unique (quote var)))) (let* ((bs (list var rh))) (progn (let* ((o18 lh)) (let* ((f18 (function (lambda (k v) (let* ((x (if (eql k (quote rest)) (list (quote cut) var (y-length lh)) (list (quote get) var (list (quote quote) k))))) (progn (if (y-is-p k) (progn (let* ((k (if (eql v (quote t)) k v))) (progn (setq bs (y-join bs (y-bind k x))))))))))))) (progn (if (hash-table-p o18) (maphash f18 o18) (if (listp o18) (let* ((i46 -1)) (while o18 (let* ((k (if (keywordp (car o18)) (y-%key (car o18)) (setq i46 (1+ i46)))) (a18 (if (keywordp (car o18)) (car (cdr o18)) (car o18)))) (funcall f18 k a18)) (setq o18 (if (keywordp (car o18)) (cdr (cdr o18)) (cdr o18)))) nil) (let* ((n29 (y-length o18))) (progn (let* ((k 0)) (progn (while (< k n29) (let* ((a18 (y-get o18 k))) (progn (funcall f18 k a18))) (setq k (+ k 1)))))))))))) bs)))))))
(y-setenv (quote bind) :symbol (quote y-bind))

(defalias (quote y-bind*) (function (lambda (args body) (if (and args (y-atom-p args)) (y-bind* (list (quote &rest) args) body) (let* ((rest nil)) (let* ((args1 nil)) (let* ((bs nil)) (let* ((ks nil)) (progn (let* ((i 0)) (progn (while (< i (y-length args)) (let* ((arg (y-get args i))) (progn (if (eql arg (quote &rest)) (setq rest (y-get args (setq i (+ i 1)))) (if (y-atom-p arg) (progn (let* ((k33 (y-length args1)) (v33 arg)) (setq args1 (y-put args1 k33 v33)) v33) nil) (let* ((id1 (y-unique (quote id1)))) (progn (let* ((k34 (y-length args1)) (v34 id1)) (setq args1 (y-put args1 k34 v34)) v34) nil) (setq bs (y-join bs (list arg id1)))))))) (setq i (+ i 1))))) (let* ((y1 (y-get args (quote rest)))) (progn (if y1 (progn (let* ((x y1)) (progn (setq rest x))))))) (let* ((o19 args)) (let* ((f19 (function (lambda (k v) (if (or (y-number-p k) (eql k (quote rest))) nil (if (y-nil-p rest) (progn (setq rest (y-unique (quote args))))) (let* ((v1 (if (eql v (quote t)) k v))) (let* ((k1 (intern (format ":%s" k)))) (progn (setq ks (y-join ks (list k1 v1))))))))))) (progn (if (hash-table-p o19) (maphash f19 o19) (if (listp o19) (let* ((i47 -1)) (while o19 (let* ((k (if (keywordp (car o19)) (y-%key (car o19)) (setq i47 (1+ i47)))) (a19 (if (keywordp (car o19)) (car (cdr o19)) (car o19)))) (funcall f19 k a19)) (setq o19 (if (keywordp (car o19)) (cdr (cdr o19)) (cdr o19)))) nil) (let* ((n30 (y-length o19))) (progn (let* ((k 0)) (progn (while (< k n30) (let* ((a19 (y-get o19 k))) (progn (funcall f19 k a19))) (setq k (+ k 1)))))))))))) (if rest (progn (progn (let* ((k35 (y-length args1)) (v35 (quote &rest))) (setq args1 (y-put args1 k35 v35)) v35) nil) (progn (let* ((k36 (y-length args1)) (v36 rest)) (setq args1 (y-put args1 k36 v36)) v36) nil))) (if ks (progn (setq bs (y-join bs (list ks rest))))) (let* ((l (list args1))) (progn (let* ((x (y-hd body))) (progn (if (stringp x) (progn (progn (let* ((k37 (y-length l)) (v37 x)) (setq l (y-put l k37 v37)) v37) nil) (setq body (y-tl body)))))) (if (y-is-p bs) (progn (let* ((k38 (y-length l)) (v38 (cons (quote let) (cons bs body)))) (setq l (y-put l k38 v38)) v38) nil) (setq l (y-join l body))) l)))))))))))
(y-setenv (quote bind*) :symbol (quote y-bind*))

(defalias (quote y-macroexpand) (function (lambda (form) (let* ((s (y--symbol-expansion form))) (progn (if s (y-macroexpand s) (if (atom form) form (let* ((x (y-macroexpand (y-hd form)))) (progn (if (eql x (quote quote)) form (if (eql x (quote \`)) (y-macroexpand (funcall (quote macroexpand) form)) (if (y--macro-p x) (y-macroexpand (apply (y--macro-function x) (y-tl form))) (cons x (mapcar (quote y-macroexpand) (y-tl form)))))))))))))))
(y-setenv (quote macroexpand) :symbol (quote y-macroexpand))

(defalias (quote y-eval) (function (lambda (form) (let* ((max-lisp-eval-depth 4450) (max-specpdl-size 13350)) (funcall (quote eval) (y-macroexpand form) (quote t))))))
(y-setenv (quote eval) :symbol (quote y-eval))

(y-setenv (quote =) :macro (function (lambda (&rest args) (if (y-find (quote stringp) args) (cons (quote string=) args) (cons (quote eql) args)))))

(defalias (quote y--expand-if) (function (lambda (id11) (let* ((var8 id11)) (let* ((a (y-get var8 (quote 0)))) (let* ((b (y-get var8 (quote 1)))) (let* ((c (y-cut var8 2))) (progn (if (y-some-p c) (list (cons (quote %if) (cons a (cons b (y--expand-if c))))) (if (y-is-p b) (list (list (quote %if) a b)) (if (y-is-p a) (list a))))))))))))
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

(y-setenv (quote let-macro) :macro (function (lambda (definitions &rest body) (progn (setq y-environment (apply (quote vector) (append y-environment (list (make-hash-table :test (quote eq)))))) (let* ((x17 (progn (let* ((x18 definitions)) (let* ((n32 (y-length x18))) (progn (let* ((i34 0)) (progn (while (< i34 n32) (let* ((m (y-get x18 i34))) (progn (y-macroexpand (cons (quote define-macro) m)))) (setq i34 (+ i34 1)))))))) (cons (quote progn) (y-macroexpand body))))) (progn (setq y-environment (apply (quote vector) (y-almost y-environment))) x17))))))
(function-put (quote let-macro) (quote lisp-indent-function) (quote defun))

(y-setenv (quote let-symbol) :macro (function (lambda (expansions &rest body) (if (y-none-p expansions) (cons (quote progn) (y-macroexpand body)) (progn (setq y-environment (apply (quote vector) (append y-environment (list (make-hash-table :test (quote eq)))))) (let* ((x21 (progn (let* ((x22 (y-pair expansions))) (let* ((n34 (y-length x22))) (progn (let* ((i36 0)) (progn (while (< i36 n34) (let* ((x (y-get x22 i36))) (progn (y-macroexpand (cons (quote define-symbol) x)))) (setq i36 (+ i36 1)))))))) (cons (quote progn) (y-macroexpand body))))) (progn (setq y-environment (apply (quote vector) (y-almost y-environment))) x21)))))))
(function-put (quote let-symbol) (quote lisp-indent-function) (quote defun))

(y-setenv (quote when-compiling) :macro (function (lambda (&rest body) (y-eval (cons (quote progn) body)))))
(function-put (quote when-compiling) (quote lisp-indent-function) (quote defun))

(y-setenv (quote let) :macro (function (lambda (bs &rest body) (if (and bs (atom bs)) (cons (quote let) (cons (list bs (y-hd body)) (y-tl body))) (if (y-none-p bs) (cons (quote progn) body) (let* ((var11 bs)) (let* ((lh (y-get var11 (quote 0)))) (let* ((rh (y-get var11 (quote 1)))) (let* ((bs2 (y-cut var11 2))) (let* ((var12 (y-bind lh rh))) (let* ((var (y-get var12 (quote 0)))) (let* ((val (y-get var12 (quote 1)))) (let* ((bs1 (y-cut var12 2))) (progn (let* ((form (cons (quote let) (cons (y-join bs1 bs2) body)))) (progn (list (quote let*) (list (list var val)) (y-macroexpand form))))))))))))))))))
(function-put (quote let) (quote lisp-indent-function) (quote defun))

(y-setenv (quote join!) :macro (function (lambda (a &rest bs) (list (quote set) a (cons (quote join) (cons a bs))))))

(y-setenv (quote inc) :macro (function (lambda (n &optional by) (list (quote set) n (list (quote +) n (or by 1))))))

(y-setenv (quote dec) :macro (function (lambda (n &optional by) (list (quote set) n (list (quote -) n (or by 1))))))

(y-setenv (quote for) :macro (function (lambda (i to &rest body) (list (quote let) i 0 (cons (quote while) (cons (list (quote <) i to) (append body (list (list (quote inc) i)))))))))
(function-put (quote for) (quote lisp-indent-function) (quote defun))

(y-setenv (quote step) :macro (function (lambda (v l &rest body) (let* ((x (y-unique (quote x))) (n (y-unique (quote n))) (i (y-unique (quote i)))) (list (quote let) (list x l n (list (quote \#) x)) (list (quote for) i n (cons (quote let) (cons (list v (list (quote at) x i)) body))))))))
(function-put (quote step) (quote lisp-indent-function) (quote defun))

(y-setenv (quote each) :macro (function (lambda (x l &rest body) (let* ((o (y-unique (quote o))) (n (y-unique (quote n))) (f (y-unique (quote f))) (a (y-unique (quote a)))) (let* ((var14 (if (y-atom-p x) (list (y-unique (quote i)) x) (if (> (y-length x) 1) x (list (y-unique (quote i)) (y-hd x)))))) (let* ((k (y-get var14 (quote 0)))) (let* ((v (y-get var14 (quote 1)))) (progn (list (quote let) (list o l f (cons (quote fn) (cons (list k v) body))) (list (quote if) (list (quote hash-table-p) o) (list (quote maphash) f o) (list (quote listp) o) (list (quote y-%for) o k a (list (quote funcall) f k a)) (list (quote let) n (list (quote \#) o) (list (quote for) k n (list (quote let) a (list (quote at) o k) (list (quote funcall) f k a))))))))))))))
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

(defalias (quote y-compile-toplevel) (function (lambda (form) (let* ((var15 (if (y-atom-p form) (list) form))) (let* ((x (y-get var15 (quote 0)))) (let* ((y--macro-p (y-get var15 (quote macro)))) (progn (if (eql x (quote defalias)) (terpri) (if (eql x (quote y-setenv)) (if y--macro-p (terpri)))) (if (eql x (quote progn)) (y-map (function y-compile-toplevel) (y-tl form)) (if (eql x (quote prog1)) (y-map (function y-compile-toplevel) (y-tl form)) (prog1 (prin1 form) (terpri)))))))))))
(y-setenv (quote compile-toplevel) :symbol (quote y-compile-toplevel))

(defalias (quote y-compile-file) (function (lambda (path &optional y--module-name) (let* ((name (or y--module-name (file-name-base path))) (y-module name) (forms (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert-file-contents-literally path) (car (read-from-string (concat "(" (buffer-string) ")")))) (and (buffer-name temp-buffer) (kill-buffer temp-buffer)))))) (exprs (funcall (quote macroexpand-all) (cons (quote progn) forms)))) (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert ";;; -*- lexical-binding: t -*-
") (insert (let ((standard-output (get-buffer-create (generate-new-buffer-name " *string-output*")))) (unwind-protect (progn (let ((standard-output standard-output)) (y-compile-toplevel exprs)) (save-current-buffer (set-buffer standard-output) (buffer-string))) (kill-buffer standard-output)))) (untabify (point-min) (point-max)) (buffer-string)) (and (buffer-name temp-buffer) (kill-buffer temp-buffer)))))))))
(y-setenv (quote compile-file) :symbol (quote y-compile-file))

(defalias (quote y-write-file) (function (lambda (path data) (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert (replace-regexp-in-string "" "" data)) (write-region (point-min) (point-max) path nil t)) (and (buffer-name temp-buffer) (kill-buffer temp-buffer))))))))
(y-setenv (quote write-file) :symbol (quote y-write-file))

(defalias (quote y--run-file) (function (lambda (path) (funcall (quote load-file) path))))
(y-setenv (quote run-file) :symbol (quote y--run-file))

(defalias (quote y--usage) (function (lambda nil (y-print "usage: y [options] <object files>") (y-print "options:") (y-print "  -c <input>      Compile input file") (y-print "  -o <output>    Output file") (y-print "  -e <expr>     Expression to evaluate"))))
(y-setenv (quote usage) :symbol (quote y--usage))

(defalias (quote y-main) (function (lambda (args) (let* ((arg (y-hd args))) (progn (if (or (string= arg "-h") (string= arg "--help")) (y--usage) (let* ((pre nil)) (let* ((input nil)) (let* ((output nil)) (let* ((target1 nil)) (let* ((expr nil)) (let* ((n (y-length args))) (progn (let* ((i 0)) (progn (while (< i n) (let* ((a (y-get args i))) (progn (if (or (string= a "-c") (string= a "-o") (string= a "-e")) (if (eql i (- n 1)) (y-print (format "missing argument for %S" a)) (progn (setq i (+ i 1)) (let* ((val (y-get args i))) (progn (if (string= a "-c") (setq input val) (if (string= a "-o") (setq output val) (if (string= a "-e") (setq expr val)))))))) (progn (let* ((k39 (y-length pre)) (v39 a)) (setq pre (y-put pre k39 v39)) v39) nil)))) (setq i (+ i 1))))) (let* ((x23 pre)) (let* ((n35 (y-length x23))) (progn (let* ((i37 0)) (progn (while (< i37 n35) (let* ((file (y-get x23 i37))) (progn (y--run-file file))) (setq i37 (+ i37 1)))))))) (if (y-nil-p input) (if expr (y--rep expr) (y--repl)) (let* ((code (y-compile-file input))) (progn (if (or (y-nil-p output) (string= output "-")) (y-print code) (y-write-file output code))))))))))))))))))
(y-setenv (quote main) :symbol (quote y-main))
(if noninteractive (progn (let* ((args command-line-args-left)) (progn (setq command-line-args-left nil) (y-main args)))))
(provide (quote y))
ECHO is on.
