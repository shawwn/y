;;; -*- lexical-binding: t -*-
(progn (quote nil) (set (make-local-variable (quote lexical-binding)) t) (quote cl) (progn (or lexical-binding (signal (quote cl-assertion-failed) (list (quote lexical-binding)))) nil) (let* ((h (make-hash-table :test (quote eq)))) (defalias (quote y-unique) (function (lambda (&optional x) (let* ((s (or x (quote gs))) (n (gethash s h 0)) (s1 (make-symbol (format "%s%d" (symbol-name s) n)))) (puthash s (+ n 1) h) s1))))) (defalias (quote y-let-unique) (cons (quote macro) (function (lambda (vars &rest body) (cons (quote let*) (cons (mapcar (function (lambda (x) (list x (list (quote y-unique) (list (quote quote) x))))) vars) body)))))) (prog1 (defalias (quote y-next) (function (lambda (h) (if (keywordp (car h)) (cddr h) (cdr h))))) (quote byte-compile-inline-expand)) (progn (or lexical-binding (signal (quote cl-assertion-failed) (list (quote lexical-binding)))) nil) (let* ((h (make-hash-table :test (quote eq))) (unset (list nil))) (prog1 (defalias (quote y-%key) (function (lambda (x) (if (keywordp x) (let* ((x1 (gethash x h unset))) (if (eq x1 unset) (progn (setq x1 (intern (substring (symbol-name x) 1))) (puthash x x1 h))) x1) x)))) (quote byte-compile-inline-expand))) (defalias (quote y-%for) (cons (quote macro) (function (lambda (h k v &rest body) (let* ((i (y-unique (quote i)))) (cons (quote let*) (cons (list (cons i (quote (-1)))) (cons (list (quote while) h (cons (quote let*) (cons (list (list k (list (quote if) (list (quote keywordp) (list (quote car) h)) (list (quote y-%key) (list (quote car) h)) (list (quote setq) i (list (quote 1+) i)))) (list v (list (quote if) (list (quote keywordp) (list (quote car) h)) (list (quote cadr) h) (list (quote car) h)))) body)) (list (quote setq) h (list (quote if) (list (quote keywordp) (list (quote car) h)) (list (quote cddr) h) (list (quote cdr) h)))) (quote (nil)))))))))) (defalias (quote y-set) (cons (quote macro) (function (lambda (a &optional b) (list (quote setf) a b))))) (prog1 (defalias (quote y-get) (function (lambda (h k) (if (hash-table-p h) (gethash k h) (if (listp h) (catch (quote y-break) (let* ((i0 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i0 (1+ i0)))) (val (if (keywordp (car h)) (cadr h) (car h)))) (if (eq var k) (progn (throw (quote y-break) val)))) (setq h (if (keywordp (car h)) (cddr h) (cdr h)))) nil)) (elt h k)))))) (quote byte-compile-inline-expand)) (put (quote y-get) (quote gv-expander) (function (lambda (f place idx) (gv-get place (function (lambda (getter _setter) (funcall f (list (quote y-get) getter idx) (function (lambda (val) (let* ((k (y-unique (quote k))) (v (y-unique (quote v)))) (list (quote let*) (list (list k idx) (list v val)) (list (quote y-set) getter (list (quote y-put) getter k v)) v))))))))))) (defalias (quote y-put) (function (lambda (h k &rest args) (let ((v (car args)) (wipe\? (null args))) (if (hash-table-p h) (progn (setq k (y-%key k)) (if wipe\? (if (integerp k) (let ((n (y-length h)) (i k) (unset (list nil))) (if (and (>= i 0) (<= i (1- n))) (progn (while (< i n) (let ((x (gethash (1+ i) h unset))) (if (eq x unset) (remhash i h) (puthash i x h))) (setq i (1+ i))) (remhash i h)) (remhash k h))) (remhash k h)) (puthash k v h)) h) (let* ((l h)) (if (and (symbolp k) (not (keywordp k))) (setq k (intern (concat ":" (symbol-name k))))) (if (listp h) (catch (quote y-break) (if wipe\? (let ((l1 h) (p nil) (head\? t)) (let* ((i1 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i1 (1+ i1)))) (_val (if (keywordp (car h)) (cadr h) (car h)))) (if (eq var k) (if head\? (progn (setq l1 (if (keywordp k) (cddr h) (cdr h))) (setq h l1)) (progn (setcdr (if (keywordp (car p)) (cdr p) p) (if (keywordp k) (cddr h) (cdr h))) (setq h p))) (setq head\? nil)) (setq p h)) (setq h (if (keywordp (car h)) (cddr h) (cdr h)))) nil) (setq l l1)) (if (or (keywordp k) (>= k 0)) (progn (if (null l) (progn (setq l (if (keywordp k) (list k nil) (list nil))) (setq h l))) (let* ((i2 -1)) (while h (let* ((var (if (keywordp (car h)) (y-%key (car h)) (setq i2 (1+ i2)))) (_val (if (keywordp (car h)) (cadr h) (car h)))) (if (eq var (y-%key k)) (progn (if (integerp k) (setcar h v) (setcar (cdr h) v)) (throw (quote y-break) l))) (if (null (y-next h)) (progn (if (keywordp k) (nconc h (list k v)) (nconc h (list nil)))))) (setq h (if (keywordp (car h)) (cddr h) (cdr h)))) nil))))) (if wipe\? (error (format "Can't wipe index %s of %S" k h)) (aset h k v))) l)))))) (defalias (quote y-wipe) (cons (quote macro) (function (lambda (place) (gv-get place (function (lambda (getter _setter) (if (eq (quote y-get) (car-safe getter)) (list (quote y-set) (cadr getter) (cons (quote y-put) (cdr getter))) (error (format "Can't wipe %S" place)))))))))) (defalias (quote y-length) (function (lambda (h &optional upto) (catch (quote y-break) (if (listp h) (let* ((n -1)) (let* ((i3 -1)) (while h (let* ((k (if (keywordp (car h)) (y-%key (car h)) (setq i3 (1+ i3)))) (_v (if (keywordp (car h)) (cadr h) (car h)))) (if (integerp k) (progn (setq n (max n k)) (if (and upto (>= n upto)) (progn (throw (quote y-break) (+ n 1))))))) (setq h (if (keywordp (car h)) (cddr h) (cdr h)))) nil) (+ n 1)) (if (hash-table-p h) (let* ((n -1)) (maphash (function (lambda (k _v) (if (integerp k) (progn (setq n (max n k)) (if (and upto (>= n upto)) (progn (throw (quote y-break) (+ n 1)))))))) h) (+ n 1)) (length h))))))) (defalias (quote y-%if) (cons (quote macro) (function (lambda (&rest args) (cons (quote if) args))))) (defalias (quote y-do) (cons (quote macro) (function (lambda (&rest body) (let* ((max-lisp-eval-depth 4450) (max-specpdl-size 13350) (y-environment (apply (quote vector) (append y-environment nil)))) (macroexp-progn (mapcar (quote y-macroexpand) body))))))) (progn (defvar y-environment (list (make-hash-table :test (quote eq)))) (prog1 (defalias (quote y-setenv) (function (lambda (k &rest ks) (let* ((i4 (if (memq :toplevel ks) 0 (- (y-length y-environment) 1)))) (progn (let* ((frame (y-get y-environment i4))) (let* ((entry (or (y-get frame k) (make-hash-table :test (quote eq))))) (progn (let* ((o0 ks)) (let* ((f0 (function (lambda (k v) (let* ((k2 k) (v1 v)) (setq entry (y-put entry k2 v1)) v1))))) (progn (if (hash-table-p o0) (maphash f0 o0) (if (listp o0) (let* ((i16 -1)) (while o0 (let* ((k (if (keywordp (car o0)) (y-%key (car o0)) (setq i16 (1+ i16)))) (a0 (if (keywordp (car o0)) (cadr o0) (car o0)))) (funcall f0 k a0)) (setq o0 (if (keywordp (car o0)) (cddr o0) (cdr o0)))) nil) (let* ((n0 (y-length o0))) (progn (let* ((k 0)) (progn (while (< k n0) (let* ((a0 (y-get o0 k))) (progn (funcall f0 k a0))) (setq k (+ k 1)))))))))))) (let* ((k3 k) (v2 entry)) (setq frame (y-put frame k3 v2)) v2))))))))) (y-setenv (quote setenv) :symbol (quote y-setenv))) (prog1 (defalias (quote y-getenv) (function (lambda (k &optional p) (let* ((i5 (- (y-length y-environment) 1))) (progn (progn (catch (quote y-break) (while (>= i5 0) (let* ((b (y-get (y-get y-environment i5) k))) (progn (if b (throw (quote y-break) (if p (y-get b p) b)) (setq i5 (- i5 1))))))))))))) (y-setenv (quote getenv) :symbol (quote y-getenv))) (y-setenv (quote unique) :symbol (quote y-unique)) (y-setenv (quote let-unique) :symbol (quote y-let-unique)) (y-setenv (quote at) :symbol (quote get)) (y-setenv (quote set) :macro (function (lambda (&rest args) (cons (quote y-set) args)))) (y-setenv (quote wipe) :macro (function (lambda (&rest args) (cons (quote y-wipe) args)))) (y-setenv (quote get) :symbol (quote y-get)) (y-setenv (quote \#) :symbol (quote y-length)) (y-setenv (quote environment) :symbol (quote y-environment)) (y-setenv (quote %if) :symbol (quote y-%if)) (prog1 (defalias (quote y-nil-p) (function (lambda (x) (eql x nil)))) (y-setenv (quote nil\?) :symbol (quote y-nil-p))) (prog1 (defalias (quote y-is-p) (function (lambda (x) (not (y-nil-p x))))) (y-setenv (quote is\?) :symbol (quote y-is-p))) (prog1 (defalias (quote y-none-p) (function (lambda (x) (eql (y-length x 0) 0)))) (y-setenv (quote none\?) :symbol (quote y-none-p))) (prog1 (defalias (quote y-some-p) (function (lambda (x) (> (y-length x 0) 0)))) (y-setenv (quote some\?) :symbol (quote y-some-p))) (prog1 (defalias (quote y-one-p) (function (lambda (x) (eql (y-length x 1) 1)))) (y-setenv (quote one\?) :symbol (quote y-one-p))) (prog1 (defalias (quote y-two-p) (function (lambda (x) (eql (y-length x 2) 2)))) (y-setenv (quote two\?) :symbol (quote y-two-p))) (prog1 (defalias (quote y-hd) (function (lambda (l) (y-get l 0)))) (y-setenv (quote hd) :symbol (quote y-hd))) (prog1 (defalias (quote y-type) (function (lambda (x) (type-of x)))) (y-setenv (quote type) :symbol (quote y-type))) (prog1 (defalias (quote y-number-p) (function (lambda (x) (numberp x)))) (y-setenv (quote number\?) :symbol (quote y-number-p))) (prog1 (defalias (quote y-obj-p) (function (lambda (x) (hash-table-p x)))) (y-setenv (quote obj\?) :symbol (quote y-obj-p))) (y-setenv (quote obj) :macro (function (lambda nil (quote (make-hash-table :test (quote eq)))))) (prog1 (defalias (quote y-atom-p) (function (lambda (x) (or (y-nil-p x) (symbolp x) (stringp x) (y-number-p x))))) (y-setenv (quote atom\?) :symbol (quote y-atom-p))) (prog1 (defalias (quote y-clip) (function (lambda (s from &optional upto) (let* ((n1 (length s))) (progn (let* ((i (if (or (y-nil-p from) (< from 0)) 0 from))) (let* ((j (if (or (y-nil-p upto) (> upto n1)) n1 (max upto i)))) (progn (substring s i j))))))))) (y-setenv (quote clip) :symbol (quote y-clip))) (prog1 (defalias (quote y--chop-p) (function (lambda (x from upto) (and (consp x) (eql from 1) (null upto) (not (keywordp (car x))))))) (y-setenv (quote chop\?) :symbol (quote y--chop-p))) (prog1 (defalias (quote y-cut) (function (lambda (x &optional from upto) (if (y--chop-p x from upto) (cdr x) (let* ((l0 (if (y-obj-p x) (make-hash-table :test (quote eq))))) (progn (progn (let* ((j 0)) (let* ((i (if (or (y-nil-p from) (< from 0)) 0 from))) (let* ((n (y-length x))) (let* ((upto (if (or (y-nil-p upto) (> upto n)) n upto))) (progn (while (< i upto) (let* ((k4 j) (v3 (y-get x i))) (setq l0 (y-put l0 k4 v3)) v3) (setq i (+ i 1)) (setq j (+ j 1))) (let* ((o1 x)) (let* ((f1 (function (lambda (k v) (if (y-number-p k) nil (let* ((k5 k) (v4 v)) (setq l0 (y-put l0 k5 v4)) v4)))))) (progn (if (hash-table-p o1) (maphash f1 o1) (if (listp o1) (let* ((i17 -1)) (while o1 (let* ((k (if (keywordp (car o1)) (y-%key (car o1)) (setq i17 (1+ i17)))) (a1 (if (keywordp (car o1)) (cadr o1) (car o1)))) (funcall f1 k a1)) (setq o1 (if (keywordp (car o1)) (cddr o1) (cdr o1)))) nil) (let* ((n2 (y-length o1))) (progn (let* ((k 0)) (progn (while (< k n2) (let* ((a1 (y-get o1 k))) (progn (funcall f1 k a1))) (setq k (+ k 1))))))))))))))))) l0))))))) (y-setenv (quote cut) :symbol (quote y-cut))) (prog1 (defalias (quote y-keys) (function (lambda (x) (let* ((l1 (if (y-obj-p x) (make-hash-table :test (quote eq))))) (progn (progn (let* ((o2 x)) (let* ((f2 (function (lambda (k v) (if (y-number-p k) nil (let* ((k6 k) (v5 v)) (setq l1 (y-put l1 k6 v5)) v5)))))) (progn (if (hash-table-p o2) (maphash f2 o2) (if (listp o2) (let* ((i18 -1)) (while o2 (let* ((k (if (keywordp (car o2)) (y-%key (car o2)) (setq i18 (1+ i18)))) (a2 (if (keywordp (car o2)) (cadr o2) (car o2)))) (funcall f2 k a2)) (setq o2 (if (keywordp (car o2)) (cddr o2) (cdr o2)))) nil) (let* ((n3 (y-length o2))) (progn (let* ((k 0)) (progn (while (< k n3) (let* ((a2 (y-get o2 k))) (progn (funcall f2 k a2))) (setq k (+ k 1)))))))))))) l1)))))) (y-setenv (quote keys) :symbol (quote y-keys))) (prog1 (defalias (quote y-edge) (function (lambda (x) (- (y-length x) 1)))) (y-setenv (quote edge) :symbol (quote y-edge))) (prog1 (defalias (quote y-tl) (function (lambda (l) (y-cut l 1)))) (y-setenv (quote tl) :symbol (quote y-tl))) (prog1 (defalias (quote y-last) (function (lambda (l) (y-get l (y-edge l))))) (y-setenv (quote last) :symbol (quote y-last))) (prog1 (defalias (quote y-almost) (function (lambda (l) (y-cut l 0 (y-edge l))))) (y-setenv (quote almost) :symbol (quote y-almost))) (y-setenv (quote add) :macro (function (lambda (l x) (progn (or (y-atom-p l) (signal (quote cl-assertion-failed) (list (quote (y-atom-p l))))) nil) (cons (quote progn) (cons (list (quote set) (list (quote at) l (list (quote \#) l)) x) (quote (nil))))))) (y-setenv (quote drop) :macro (function (lambda (l) (progn (or (y-atom-p l) (signal (quote cl-assertion-failed) (list (quote (y-atom-p l))))) nil) (list (quote prog1) (list (quote at) l (list (quote edge) l)) (list (quote set) l (list (quote almost) l)))))) (prog1 (defalias (quote y-reverse) (function (lambda (l) (let* ((l10 (y-keys l))) (progn (progn (let* ((i (y-edge l))) (progn (while (>= i 0) (progn (let* ((k7 (y-length l10)) (v6 (y-get l i))) (setq l10 (y-put l10 k7 v6)) v6) nil) (setq i (- i 1))))) l10)))))) (y-setenv (quote reverse) :symbol (quote y-reverse))) (prog1 (defalias (quote y-reduce) (function (lambda (f x) (if (y-none-p x) nil (if (y-one-p x) (y-hd x) (funcall f (y-hd x) (y-reduce f (y-tl x)))))))) (y-setenv (quote reduce) :symbol (quote y-reduce))) (prog1 (defalias (quote y-join) (function (lambda (&rest ls) (if (y-two-p ls) (let* ((var00 ls)) (progn (let* ((a (y-get var00 (quote 0)))) (let* ((b (y-get var00 (quote 1)))) (progn (if (and a b) (let* ((c (if (y-obj-p a) (make-hash-table :test (quote eq))))) (let* ((o (y-length a))) (progn (let* ((o3 a)) (let* ((f3 (function (lambda (k v) (let* ((k8 k) (v7 v)) (setq c (y-put c k8 v7)) v7))))) (progn (if (hash-table-p o3) (maphash f3 o3) (if (listp o3) (let* ((i19 -1)) (while o3 (let* ((k (if (keywordp (car o3)) (y-%key (car o3)) (setq i19 (1+ i19)))) (a3 (if (keywordp (car o3)) (cadr o3) (car o3)))) (funcall f3 k a3)) (setq o3 (if (keywordp (car o3)) (cddr o3) (cdr o3)))) nil) (let* ((n4 (y-length o3))) (progn (let* ((k 0)) (progn (while (< k n4) (let* ((a3 (y-get o3 k))) (progn (funcall f3 k a3))) (setq k (+ k 1)))))))))))) (let* ((o4 b)) (let* ((f4 (function (lambda (k v) (if (y-number-p k) (progn (setq k (+ k o)))) (let* ((k9 k) (v8 v)) (setq c (y-put c k9 v8)) v8))))) (progn (if (hash-table-p o4) (maphash f4 o4) (if (listp o4) (let* ((i20 -1)) (while o4 (let* ((k (if (keywordp (car o4)) (y-%key (car o4)) (setq i20 (1+ i20)))) (a4 (if (keywordp (car o4)) (cadr o4) (car o4)))) (funcall f4 k a4)) (setq o4 (if (keywordp (car o4)) (cddr o4) (cdr o4)))) nil) (let* ((n5 (y-length o4))) (progn (let* ((k0 0)) (progn (progn (while (< k0 n5) (let* ((a4 (y-get o4 k0))) (progn (funcall f4 k0 a4))) (setq k0 (+ k0 1))))))))))))) c))) (or a b nil))))))) (or (y-reduce (quote y-join) ls) nil))))) (y-setenv (quote join) :symbol (quote y-join))) (prog1 (defalias (quote y-find) (function (lambda (f l) (catch (quote y-break) (let* ((o50 l)) (progn (let* ((f5 (function (lambda (i6 x) (let* ((y (funcall f x))) (progn (if y (throw (quote y-break) y)))))))) (progn (if (hash-table-p o50) (maphash f5 o50) (if (listp o50) (let* ((i21 -1)) (while o50 (let* ((i6 (if (keywordp (car o50)) (y-%key (car o50)) (setq i21 (1+ i21)))) (a5 (if (keywordp (car o50)) (cadr o50) (car o50)))) (funcall f5 i6 a5)) (setq o50 (if (keywordp (car o50)) (cddr o50) (cdr o50)))) nil) (let* ((n6 (y-length o50))) (progn (let* ((i6 0)) (progn (while (< i6 n6) (let* ((a5 (y-get o50 i6))) (progn (funcall f5 i6 a5))) (setq i6 (+ i6 1))))))))))))))))) (y-setenv (quote find) :symbol (quote y-find))) (prog1 (defalias (quote y-first) (function (lambda (f l) (catch (quote y-break) (let* ((x00 l)) (progn (let* ((n7 (y-length x00))) (progn (let* ((i7 0)) (progn (while (< i7 n7) (let* ((x (y-get x00 i7))) (progn (let* ((y (funcall f x))) (progn (if y (throw (quote y-break) y)))))) (setq i7 (+ i7 1))))))))))))) (y-setenv (quote first) :symbol (quote y-first))) (prog1 (defalias (quote y-in-p) (function (lambda (x l) (y-find (function (lambda (y) (eql x y))) l)))) (y-setenv (quote in\?) :symbol (quote y-in-p))) (prog1 (defalias (quote y-pair) (function (lambda (l) (let* ((l11 (if (y-obj-p l) (make-hash-table :test (quote eq))))) (progn (progn (let* ((n (y-length l))) (progn (let* ((i 0)) (progn (while (< i n) (progn (let* ((k10 (y-length l11)) (v9 (list (y-get l i) (y-get l (+ i 1))))) (setq l11 (y-put l11 k10 v9)) v9) nil) (setq i (+ i 1)) (setq i (+ i 1))))))) l11)))))) (y-setenv (quote pair) :symbol (quote y-pair))) (prog1 (defalias (quote y-map) (function (lambda (f x) (let* ((l2 (if (y-obj-p x) (make-hash-table :test (quote eq))))) (progn (progn (let* ((x1 x)) (let* ((n8 (y-length x1))) (progn (let* ((i8 0)) (progn (while (< i8 n8) (let* ((v (y-get x1 i8))) (progn (let* ((y (funcall f v))) (progn (if (y-is-p y) (progn (let* ((k11 (y-length l2)) (v10 y)) (setq l2 (y-put l2 k11 v10)) v10) nil)))))) (setq i8 (+ i8 1)))))))) (let* ((o6 x)) (let* ((f6 (function (lambda (k v) (if (y-number-p k) nil (let* ((y0 (funcall f v))) (progn (progn (if (y-is-p y0) (progn (let* ((k12 k) (v11 y0)) (setq l2 (y-put l2 k12 v11)) v11))))))))))) (progn (if (hash-table-p o6) (maphash f6 o6) (if (listp o6) (let* ((i22 -1)) (while o6 (let* ((k (if (keywordp (car o6)) (y-%key (car o6)) (setq i22 (1+ i22)))) (a6 (if (keywordp (car o6)) (cadr o6) (car o6)))) (funcall f6 k a6)) (setq o6 (if (keywordp (car o6)) (cddr o6) (cdr o6)))) nil) (let* ((n9 (y-length o6))) (progn (let* ((k 0)) (progn (while (< k n9) (let* ((a6 (y-get o6 k))) (progn (funcall f6 k a6))) (setq k (+ k 1)))))))))))) l2)))))) (y-setenv (quote map) :symbol (quote y-map))) (prog1 (defalias (quote y-keep) (function (lambda (f x) (y-map (function (lambda (v) (if (funcall f v) (progn v)))) x)))) (y-setenv (quote keep) :symbol (quote y-keep))) (prog1 (defalias (quote y-keys-p) (function (lambda (l) (catch (quote y-break) (let* ((o70 l)) (progn (let* ((f7 (function (lambda (k v) (if (y-number-p k) nil (throw (quote y-break) (quote t))))))) (progn (if (hash-table-p o70) (maphash f7 o70) (if (listp o70) (let* ((i23 -1)) (while o70 (let* ((k (if (keywordp (car o70)) (y-%key (car o70)) (setq i23 (1+ i23)))) (a7 (if (keywordp (car o70)) (cadr o70) (car o70)))) (funcall f7 k a7)) (setq o70 (if (keywordp (car o70)) (cddr o70) (cdr o70)))) nil) (let* ((n10 (y-length o70))) (progn (let* ((k 0)) (progn (while (< k n10) (let* ((a7 (y-get o70 k))) (progn (funcall f7 k a7))) (setq k (+ k 1))))))))))))) nil)))) (y-setenv (quote keys\?) :symbol (quote y-keys-p))) (prog1 (defalias (quote y-empty-p) (function (lambda (l) (catch (quote y-break) (let* ((o80 l)) (progn (let* ((f8 (function (lambda (i9 x) (throw (quote y-break) nil))))) (progn (if (hash-table-p o80) (maphash f8 o80) (if (listp o80) (let* ((i24 -1)) (while o80 (let* ((i9 (if (keywordp (car o80)) (y-%key (car o80)) (setq i24 (1+ i24)))) (a8 (if (keywordp (car o80)) (cadr o80) (car o80)))) (funcall f8 i9 a8)) (setq o80 (if (keywordp (car o80)) (cddr o80) (cdr o80)))) nil) (let* ((n11 (y-length o80))) (progn (let* ((i9 0)) (progn (while (< i9 n11) (let* ((a8 (y-get o80 i9))) (progn (funcall f8 i9 a8))) (setq i9 (+ i9 1))))))))))))) (quote t))))) (y-setenv (quote empty\?) :symbol (quote y-empty-p))) (prog1 (defalias (quote y-toplevel-p) (function (lambda nil (y-one-p y-environment)))) (y-setenv (quote toplevel\?) :symbol (quote y-toplevel-p))) (prog1 (defalias (quote y-print) (function (lambda (x) (princ (format "%s
" x)) nil))) (y-setenv (quote print) :symbol (quote y-print))) (prog1 (defalias (quote y--id) (function (lambda (x) (let* ((s0 (append (if (symbolp x) (symbol-name x) x) nil))) (progn (progn (if (eql 63 (y-get s0 (y-edge s0))) (progn (if (memq 45 s0) (progn (let* ((k13 (y-edge s0)) (v12 45)) (setq s0 (y-put s0 k13 v12)) v12) (let* ((k14 (y-length s0)) (v13 112)) (setq s0 (y-put s0 k14 v13)) v13)) (let* ((k15 (y-edge s0)) (v14 112)) (setq s0 (y-put s0 k15 v14)) v14)))) (intern (concat s0)))))))) (y-setenv (quote id) :symbol (quote y--id))) (prog1 (defvar y-module nil) (y-setenv (quote module) :symbol (quote y-module))) (prog1 (defalias (quote y--module-name) (function (lambda nil (or y-module (let* ((file0 (or load-file-name (buffer-file-name)))) (progn (progn (if file0 (file-name-base file0) (buffer-name))))))))) (y-setenv (quote module-name) :symbol (quote y--module-name))) (prog1 (defalias (quote y--global-id) (function (lambda (prefix name) (let* ((s1 (if (stringp name) name (symbol-name name)))) (progn (progn (if (eql 0 (string-match (regexp-quote prefix) s1)) name (y--id (concat prefix s1))))))))) (y-setenv (quote global-id) :symbol (quote y--global-id))) (prog1 (defalias (quote y--macro-function) (function (lambda (k) (y-getenv k (quote macro))))) (y-setenv (quote macro-function) :symbol (quote y--macro-function))) (prog1 (defalias (quote y--macro-p) (function (lambda (k) (y--macro-function k)))) (y-setenv (quote macro\?) :symbol (quote y--macro-p))) (prog1 (defalias (quote y--symbol-expansion) (function (lambda (k) (y-getenv k (quote symbol))))) (y-setenv (quote symbol-expansion) :symbol (quote y--symbol-expansion))) (prog1 (defalias (quote y--symbol-p) (function (lambda (k) (let* ((v0 (y--symbol-expansion k))) (progn (progn (and v0 (not (eql v0 k))))))))) (y-setenv (quote symbol\?) :symbol (quote y--symbol-p))) (prog1 (defalias (quote y--variable-p) (function (lambda (k) (let* ((i10 (y-edge y-environment))) (progn (progn (catch (quote y-break) (while (>= i10 0) (let* ((b (y-get (y-get y-environment i10) k))) (progn (if b (throw (quote y-break) (y-get b (quote variable))) (setq i10 (1- i10))))))))))))) (y-setenv (quote variable\?) :symbol (quote y--variable-p))) (prog1 (defalias (quote y--bound-p) (function (lambda (x) (or (y--macro-p x) (y--symbol-p x) (y--variable-p x))))) (y-setenv (quote bound\?) :symbol (quote y--bound-p))) (prog1 (defalias (quote y-bind) (function (lambda (lh rh) (if (y-atom-p lh) (list lh rh) (let* ((var (y-unique (quote var)))) (let* ((bs0 (list var rh))) (progn (progn (let* ((o9 lh)) (let* ((f9 (function (lambda (k v) (let* ((x (if (eql k (quote rest)) (list (quote cut) var (y-length lh)) (list (quote get) var (list (quote quote) k))))) (progn (if (y-is-p k) (progn (let* ((k1 (if (eql v (quote t)) k v))) (progn (progn (setq bs0 (y-join bs0 (y-bind k1 x)))))))))))))) (progn (if (hash-table-p o9) (maphash f9 o9) (if (listp o9) (let* ((i25 -1)) (while o9 (let* ((k (if (keywordp (car o9)) (y-%key (car o9)) (setq i25 (1+ i25)))) (a9 (if (keywordp (car o9)) (cadr o9) (car o9)))) (funcall f9 k a9)) (setq o9 (if (keywordp (car o9)) (cddr o9) (cdr o9)))) nil) (let* ((n12 (y-length o9))) (progn (let* ((k 0)) (progn (while (< k n12) (let* ((a9 (y-get o9 k))) (progn (funcall f9 k a9))) (setq k (+ k 1)))))))))))) bs0)))))))) (y-setenv (quote bind) :symbol (quote y-bind))) (prog1 (defalias (quote y-bind*) (function (lambda (args body) (if (and args (y-atom-p args)) (y-bind* (list (quote &rest) args) body) (let* ((rest0 nil)) (progn (let* ((args1 nil)) (let* ((bs nil)) (let* ((ks nil)) (progn (let* ((i 0)) (progn (while (< i (y-length args)) (let* ((arg (y-get args i))) (progn (if (eql arg (quote &rest)) (setq rest0 (y-get args (setq i (+ i 1)))) (if (y-atom-p arg) (progn (let* ((k16 (y-length args1)) (v15 arg)) (setq args1 (y-put args1 k16 v15)) v15) nil) (let* ((id1 (y-unique (quote id1)))) (progn (let* ((k17 (y-length args1)) (v16 id1)) (setq args1 (y-put args1 k17 v16)) v16) nil) (setq bs (y-join bs (list arg id1)))))))) (setq i (+ i 1))))) (let* ((y1 (y-get args (quote rest)))) (progn (if y1 (progn (let* ((x y1)) (progn (setq rest0 x))))))) (let* ((o10 args)) (let* ((f10 (function (lambda (k v) (if (or (y-number-p k) (eql k (quote rest))) nil (if (y-nil-p rest0) (progn (setq rest0 (y-unique (quote args))))) (let* ((v1 (if (eql v (quote t)) k v))) (let* ((k1 (intern (format ":%s" k)))) (progn (setq ks (y-join ks (list k1 v1))))))))))) (progn (if (hash-table-p o10) (maphash f10 o10) (if (listp o10) (let* ((i26 -1)) (while o10 (let* ((k (if (keywordp (car o10)) (y-%key (car o10)) (setq i26 (1+ i26)))) (a10 (if (keywordp (car o10)) (cadr o10) (car o10)))) (funcall f10 k a10)) (setq o10 (if (keywordp (car o10)) (cddr o10) (cdr o10)))) nil) (let* ((n13 (y-length o10))) (progn (let* ((k 0)) (progn (while (< k n13) (let* ((a10 (y-get o10 k))) (progn (funcall f10 k a10))) (setq k (+ k 1)))))))))))) (if rest0 (progn (progn (let* ((k18 (y-length args1)) (v17 (quote &rest))) (setq args1 (y-put args1 k18 v17)) v17) nil) (progn (let* ((k19 (y-length args1)) (v18 rest0)) (setq args1 (y-put args1 k19 v18)) v18) nil))) (if ks (progn (setq bs (y-join bs (list ks rest0))))) (let* ((l (list args1))) (progn (let* ((x2 (y-hd body))) (progn (progn (if (stringp x2) (progn (progn (let* ((k20 (y-length l)) (v19 x2)) (setq l (y-put l k20 v19)) v19) nil) (setq body (y-tl body))))))) (if (y-is-p bs) (progn (let* ((k21 (y-length l)) (v20 (cons (quote let) (cons bs body)))) (setq l (y-put l k21 v20)) v20) nil) (setq l (y-join l body))) l)))))))))))) (y-setenv (quote bind*) :symbol (quote y-bind*))) (prog1 (defalias (quote y-macroexpand) (function (lambda (form) (let* ((s2 (y--symbol-expansion form))) (progn (progn (if s2 (y-macroexpand s2) (if (atom form) form (let* ((x (y-macroexpand (y-hd form)))) (progn (if (eql x (quote quote)) form (if (eql x (quote \`)) (y-macroexpand (funcall (quote macroexpand) form)) (if (y--macro-p x) (y-macroexpand (apply (y--macro-function x) (y-tl form))) (cons x (mapcar (quote y-macroexpand) (y-tl form)))))))))))))))) (y-setenv (quote macroexpand) :symbol (quote y-macroexpand))) (prog1 (defalias (quote y-eval) (function (lambda (form) (let* ((max-lisp-eval-depth 4450) (max-specpdl-size 13350)) (funcall (quote eval) (y-macroexpand form) (quote t)))))) (y-setenv (quote eval) :symbol (quote y-eval))) (y-setenv (quote =) :macro (function (lambda (&rest args) (if (y-find (quote stringp) args) (cons (quote string=) args) (cons (quote eql) args))))) (prog1 (defalias (quote y--expand-if) (function (lambda (id10) (let* ((var10 id10)) (progn (let* ((a (y-get var10 (quote 0)))) (let* ((b (y-get var10 (quote 1)))) (let* ((c (y-cut var10 2))) (progn (if (y-some-p c) (list (cons (quote %if) (cons a (cons b (y--expand-if c))))) (if (y-is-p b) (list (list (quote %if) a b)) (if (y-is-p a) (list a))))))))))))) (y-setenv (quote expand-if) :symbol (quote y--expand-if))) (y-setenv (quote if) :macro (function (lambda (&rest branches) (y-hd (y--expand-if branches))))) (progn (y-setenv (quote with) :macro (function (lambda (x v &rest body) (cons (quote let) (cons (list x v) (append body (list x))))))) (function-put (quote with) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote let-when) :macro (function (lambda (x v &rest body) (let* ((y (y-unique (quote y)))) (list (quote let) y v (list (quote when) y (cons (quote let) (cons (list x y) body)))))))) (function-put (quote let-when) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote fn) :macro (function (lambda (args &rest body) (cons (quote lambda) (y-bind* args body))))) (function-put (quote fn) (quote lisp-indent-function) (quote defun))) (prog1 (defalias (quote y--body-macro-p) (function (lambda (args) (eql (quote body) (if (y-atom-p args) args (car (funcall (quote last) args))))))) (y-setenv (quote body-macro\?) :symbol (quote y--body-macro-p))) (prog1 (defalias (quote y--body-indentation) (function (lambda (name args form) (if (y--body-macro-p args) (list (quote prog1) form (cons (quote function-put) (cons (list (quote quote) name) (quote ((quote lisp-indent-function) (quote defun)))))) form)))) (y-setenv (quote body-indentation) :symbol (quote y--body-indentation))) (progn (y-setenv (quote define-macro) :macro (function (lambda (name args &rest body) (let* ((form (list (quote setenv) (list (quote quote) name) (quote :macro) (cons (quote fn) (cons args body)))) (form (if (eql (quote body) (if (y-atom-p args) args (car (funcall (quote last) args)))) (list (quote progn) form (cons (quote function-put) (cons (list (quote quote) name) (quote ((quote lisp-indent-function) (quote defun)))))) form))) (y-eval form) form)))) (function-put (quote define-macro) (quote lisp-indent-function) (quote defun))) (y-setenv (quote define-symbol) :macro (function (lambda (name expansion) (y-setenv name :symbol expansion) (list (quote setenv) (list (quote quote) name) (quote :symbol) (list (quote quote) expansion))))) (prog1 (defalias (quote y--expand-definition) (function (lambda (name x body global\?) (let* ((sep0 (if global\? "-" "--"))) (progn (let* ((name1 (y--global-id (concat (y--module-name) sep0) name))) (let* ((body1 (y-keep (function (lambda (x) (not (stringp x)))) body))) (let* ((fn\? (y-some-p body1))) (let* ((var (if global\? (quote defvar) (quote defconst)))) (progn (if global\? (y-setenv name1 :variable (quote t) :toplevel (quote t)) (y-setenv name1 :variable (quote t))) (y-setenv name :symbol name1) (list (quote prog1) (if fn\? (list (quote defalias) (list (quote quote) name1) (cons (quote fn) (cons x body))) (cons var (cons name1 (cons x body)))) (list (quote setenv) (list (quote quote) name) (quote :symbol) (list (quote quote) name1))))))))))))) (y-setenv (quote expand-definition) :symbol (quote y--expand-definition))) (progn (y-setenv (quote define) :macro (function (lambda (name &optional x &rest body) (y--expand-definition name x body nil)))) (function-put (quote define) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote define-global) :macro (function (lambda (name &optional x &rest body) (y--expand-definition name x body (quote t))))) (function-put (quote define-global) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote with-frame) :macro (function (lambda (&rest body) (let* ((x (y-unique (quote x)))) (list (quote progn) (quote (set environment (apply (quote vector) (append environment (list (obj)))))) (cons (quote with) (cons x (cons (cons (quote progn) body) (quote ((set environment (apply (quote vector) (almost environment))))))))))))) (function-put (quote with-frame) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote let-macro) :macro (function (lambda (definitions &rest body) (progn (setq y-environment (apply (quote vector) (append y-environment (list (make-hash-table :test (quote eq)))))) (let* ((x50 (progn (let* ((x60 definitions)) (progn (let* ((n15 (y-length x60))) (progn (let* ((i12 0)) (progn (while (< i12 n15) (let* ((m (y-get x60 i12))) (progn (y-macroexpand (cons (quote define-macro) m)))) (setq i12 (+ i12 1))))))))) (cons (quote progn) (y-macroexpand body))))) (progn (progn (setq y-environment (apply (quote vector) (y-almost y-environment))) x50))))))) (function-put (quote let-macro) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote let-symbol) :macro (function (lambda (expansions &rest body) (if (y-none-p expansions) (cons (quote progn) (y-macroexpand body)) (progn (setq y-environment (apply (quote vector) (append y-environment (list (make-hash-table :test (quote eq)))))) (let* ((x90 (progn (let* ((x100 (y-pair expansions))) (progn (let* ((n17 (y-length x100))) (progn (let* ((i14 0)) (progn (while (< i14 n17) (let* ((x (y-get x100 i14))) (progn (y-macroexpand (cons (quote define-symbol) x)))) (setq i14 (+ i14 1))))))))) (cons (quote progn) (y-macroexpand body))))) (progn (progn (setq y-environment (apply (quote vector) (y-almost y-environment))) x90)))))))) (function-put (quote let-symbol) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote when-compiling) :macro (function (lambda (&rest body) (y-eval (cons (quote progn) body))))) (function-put (quote when-compiling) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote let) :macro (function (lambda (bs &rest body) (if (and bs (atom bs)) (cons (quote let) (cons (list bs (y-hd body)) (y-tl body))) (if (y-none-p bs) (cons (quote progn) body) (let* ((var40 bs)) (progn (let* ((lh (y-get var40 (quote 0)))) (let* ((rh (y-get var40 (quote 1)))) (let* ((bs2 (y-cut var40 2))) (let* ((var5 (y-bind lh rh))) (let* ((var (y-get var5 (quote 0)))) (let* ((val (y-get var5 (quote 1)))) (let* ((bs1 (y-cut var5 2))) (progn (let* ((renames nil)) (progn (if (or (y--bound-p var) (y-toplevel-p)) (let* ((var1 (y-unique var))) (progn (setq renames (list var var1)) (setq var var1))) (y-setenv var :variable (quote t))) (let* ((form (cons (quote let) (cons (y-join bs1 bs2) body)))) (progn (if (y-none-p renames) nil (setq form (list (quote let-symbol) renames form))) (list (quote let*) (list (list var val)) (y-macroexpand form))))))))))))))))))))) (function-put (quote let) (quote lisp-indent-function) (quote defun))) (y-setenv (quote join!) :macro (function (lambda (a &rest bs) (list (quote set) a (cons (quote join) (cons a bs)))))) (y-setenv (quote inc) :macro (function (lambda (n &optional by) (list (quote set) n (list (quote +) n (or by 1)))))) (y-setenv (quote dec) :macro (function (lambda (n &optional by) (list (quote set) n (list (quote -) n (or by 1)))))) (progn (y-setenv (quote for) :macro (function (lambda (i to &rest body) (list (quote let) i 0 (cons (quote while) (cons (list (quote <) i to) (append body (list (list (quote inc) i))))))))) (function-put (quote for) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote step) :macro (function (lambda (v l &rest body) (let* ((x (y-unique (quote x))) (n (y-unique (quote n))) (i (y-unique (quote i)))) (list (quote let) (list x l n (list (quote \#) x)) (list (quote for) i n (cons (quote let) (cons (list v (list (quote at) x i)) body)))))))) (function-put (quote step) (quote lisp-indent-function) (quote defun))) (progn (y-setenv (quote each) :macro (function (lambda (x l &rest body) (let* ((o (y-unique (quote o))) (n (y-unique (quote n))) (f (y-unique (quote f))) (a (y-unique (quote a)))) (let* ((var70 (if (y-atom-p x) (list (y-unique (quote i)) x) (if (> (y-length x) 1) x (list (y-unique (quote i)) (y-hd x)))))) (progn (let* ((k (y-get var70 (quote 0)))) (let* ((v (y-get var70 (quote 1)))) (progn (list (quote let) (list o l f (cons (quote fn) (cons (list k v) body))) (list (quote if) (list (quote hash-table-p) o) (list (quote maphash) f o) (list (quote listp) o) (list (quote y-%for) o k a (list (quote funcall) f k a)) (list (quote let) n (list (quote \#) o) (list (quote for) k n (list (quote let) a (list (quote at) o k) (list (quote funcall) f k a))))))))))))))) (function-put (quote each) (quote lisp-indent-function) (quote defun))) (prog1 (defalias (quote y--eval-print) (function (lambda (form) (condition-case err (let* ((x11 (y-eval form))) (progn (progn (if (y-is-p x11) (y-print (format "%S" x11)))))) (error (y-print (format "error: %s" (error-message-string err)))))))) (y-setenv (quote eval-print) :symbol (quote y--eval-print))) (prog1 (defalias (quote y--read-string) (function (lambda (s &optional more) (if more (condition-case nil (car (read-from-string s)) (end-of-file more)) (car (read-from-string s)))))) (y-setenv (quote read-string) :symbol (quote y--read-string))) (prog1 (defalias (quote y--rep) (function (lambda (s) (y--eval-print (y--read-string s))))) (y-setenv (quote rep) :symbol (quote y--rep))) (prog1 (defalias (quote y--repl) (function (lambda nil (let* ((buf0 "")) (progn (progn (let* ((rep1 (function (lambda (s) (setq buf0 (concat buf0 s)) (let* ((more (make-hash-table :test (quote eq)))) (let* ((form (y--read-string buf0 more))) (progn (if (eql form more) nil (y--eval-print form) (setq buf0 "") (princ "> "))))))))) (progn (princ "> ") (catch (quote y-break) (while (quote t) (let* ((s (read-from-minibuffer ""))) (progn (if (and s (not (string= s ":a"))) (funcall rep1 (concat s "
")) (throw (quote y-break) nil)))))))))))))) (y-setenv (quote repl) :symbol (quote y--repl))) (prog1 (defalias (quote y-compile-file) (function (lambda (path &optional y--module-name) (let* ((name (or y--module-name (file-name-base path))) (y-module name) (forms (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert-file-contents-literally path) (car (read-from-string (concat "(" (buffer-string) ")")))) (and (buffer-name temp-buffer) (kill-buffer temp-buffer)))))) (exprs (funcall (quote macroexpand-all) (cons (quote progn) forms)))) (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert ";;; -*- lexical-binding: t -*-
") (insert (let ((standard-output (get-buffer-create (generate-new-buffer-name " *string-output*")))) (unwind-protect (progn (let ((standard-output standard-output)) (prin1 exprs)) (save-current-buffer (set-buffer standard-output) (buffer-string))) (kill-buffer standard-output)))) (untabify (point-min) (point-max)) (buffer-string)) (and (buffer-name temp-buffer) (kill-buffer temp-buffer))))))))) (y-setenv (quote compile-file) :symbol (quote y-compile-file))) (prog1 (defalias (quote y-write-file) (function (lambda (path data) (let ((temp-buffer (generate-new-buffer " *temp*"))) (save-current-buffer (set-buffer temp-buffer) (unwind-protect (progn (insert data) (write-region (point-min) (point-max) path nil t)) (and (buffer-name temp-buffer) (kill-buffer temp-buffer)))))))) (y-setenv (quote write-file) :symbol (quote y-write-file))) (prog1 (defalias (quote y--run-file) (function (lambda (path) (funcall (quote load-file) path)))) (y-setenv (quote run-file) :symbol (quote y--run-file))) (prog1 (defalias (quote y--usage) (function (lambda nil (y-print "usage: y [options] <object files>") (y-print "options:") (y-print "  -c <input>  Compile input file") (y-print "  -o <output>    Output file") (y-print "  -e <expr>     Expression to evaluate")))) (y-setenv (quote usage) :symbol (quote y--usage))) (prog1 (defalias (quote y-main) (function (lambda (args) (let* ((arg0 (y-hd args))) (progn (progn (if (or (string= arg0 "-h") (string= arg0 "--help")) (y--usage) (let* ((pre nil)) (let* ((input nil)) (let* ((output nil)) (let* ((target1 nil)) (let* ((expr nil)) (let* ((n (y-length args))) (progn (let* ((i 0)) (progn (while (< i n) (let* ((a (y-get args i))) (progn (if (or (string= a "-c") (string= a "-o") (string= a "-e")) (if (eql i (- n 1)) (y-print (format "missing argument for %S" a)) (progn (setq i (+ i 1)) (let* ((val (y-get args i))) (progn (if (string= a "-c") (setq input val) (if (string= a "-o") (setq output val) (if (string= a "-e") (setq expr val)))))))) (progn (let* ((k22 (y-length pre)) (v21 a)) (setq pre (y-put pre k22 v21)) v21) nil)))) (setq i (+ i 1))))) (let* ((x12 pre)) (let* ((n18 (y-length x12))) (progn (let* ((i15 0)) (progn (while (< i15 n18) (let* ((file (y-get x12 i15))) (progn (y--run-file file))) (setq i15 (+ i15 1)))))))) (if (y-nil-p input) (if expr (y--rep expr) (y--repl)) (let* ((code (y-compile-file input))) (progn (if (or (y-nil-p output) (string= output "-")) (y-print code) (y-write-file output code))))))))))))))))))) (y-setenv (quote main) :symbol (quote y-main))) (if noninteractive (progn (let* ((args0 command-line-args-left)) (progn (progn (setq command-line-args-left nil) (y-main args0))))))) (provide (quote y)))