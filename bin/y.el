;;; -*- lexical-binding: t -*-
(progn 'nil
       (set
        (make-local-variable 'lexical-binding)
        t)
       'cl
       (progn
         (or lexical-binding
             (signal 'cl-assertion-failed
                     (list 'lexical-binding)))
         nil)
       (let*
           ((h
             (make-hash-table :test 'eq)))
         (defalias 'y-unique
           #'(lambda
               (&optional x)
               (let*
                   ((s
                     (or x 'gs))
                    (n
                     (gethash s h 0))
                    (s1
                     (make-symbol
                      (format "%s%d"
                              (symbol-name s)
                              n))))
                 (puthash s
                          (+ n 1)
                          h)
                 s1))))
       (defalias 'y-let-unique
         (cons 'macro
               #'(lambda
                   (vars &rest body)
                   (cons 'let*
                         (cons
                          (mapcar
                           #'(lambda
                               (x)
                               (list x
                                     (list 'y-unique
                                           (list 'quote x))))
                           vars)
                          body)))))
       (prog1
           (defalias 'y-next
             #'(lambda
                 (h)
                 (if
                     (keywordp
                      (car h))
                     (cddr h)
                   (cdr h))))
         'byte-compile-inline-expand)
       (progn
         (or lexical-binding
             (signal 'cl-assertion-failed
                     (list 'lexical-binding)))
         nil)
       (let*
           ((h
             (make-hash-table :test 'eq))
            (unset
             (list nil)))
         (prog1
             (defalias 'y-%key
               #'(lambda
                   (x)
                   (if
                       (keywordp x)
                       (let*
                           ((x1
                             (gethash x h unset)))
                         (if
                             (eq x1 unset)
                             (progn
                               (setq x1
                                     (intern
                                      (substring
                                       (symbol-name x)
                                       1)))
                               (puthash x x1 h)))
                         x1)
                     x)))
           'byte-compile-inline-expand))
       (defalias 'y-%for
         (cons 'macro
               #'(lambda
                   (h k v &rest body)
                   (let*
                       ((i
                         (y-unique 'i)))
                     (cons 'let*
                           (cons
                            (list
                             (cons i
                                   '(-1)))
                            (cons
                             (list 'while h
                                   (cons 'let*
                                         (cons
                                          (list
                                           (list k
                                                 (list 'if
                                                       (list 'keywordp
                                                             (list 'car h))
                                                       (list 'y-%key
                                                             (list 'car h))
                                                       (list 'setq i
                                                             (list '1+ i))))
                                           (list v
                                                 (list 'if
                                                       (list 'keywordp
                                                             (list 'car h))
                                                       (list 'cadr h)
                                                       (list 'car h))))
                                          body))
                                   (list 'setq h
                                         (list 'if
                                               (list 'keywordp
                                                     (list 'car h))
                                               (list 'cddr h)
                                               (list 'cdr h))))
                             '(nil))))))))
       (defalias 'y-set
         (cons 'macro
               #'(lambda
                   (a &optional b)
                   (list 'setf a b))))
       (prog1
           (defalias 'y-get
             #'(lambda
                 (h k)
                 (if
                     (hash-table-p h)
                     (gethash k h)
                   (if
                       (listp h)
                       (catch 'y-break
                         (let*
                             ((i0 -1))
                           (while h
                             (let*
                                 ((var
                                   (if
                                       (keywordp
                                        (car h))
                                       (y-%key
                                        (car h))
                                     (setq i0
                                           (1+ i0))))
                                  (val
                                   (if
                                       (keywordp
                                        (car h))
                                       (cadr h)
                                     (car h))))
                               (if
                                   (eq var k)
                                   (progn
                                     (throw 'y-break val))))
                             (setq h
                                   (if
                                       (keywordp
                                        (car h))
                                       (cddr h)
                                     (cdr h))))
                           nil))
                     (elt h k)))))
         'byte-compile-inline-expand)
       (put 'y-get 'gv-expander
            #'(lambda
                (f place idx)
                (gv-get place
                        #'(lambda
                            (getter _setter)
                            (funcall f
                                     (list 'y-get getter idx)
                                     #'(lambda
                                         (val)
                                         (let*
                                             ((k
                                               (y-unique 'k))
                                              (v
                                               (y-unique 'v)))
                                           (list 'let*
                                                 (list
                                                  (list k idx)
                                                  (list v val))
                                                 (list 'y-set getter
                                                       (list 'y-put getter k v))
                                                 v))))))))
       (defalias 'y-put
         #'(lambda
             (h k &rest args)
             (let
                 ((v
                   (car args))
                  (wipe\?
                   (null args)))
               (if
                   (hash-table-p h)
                   (progn
                     (setq k
                           (y-%key k))
                     (if wipe\?
                         (if
                             (integerp k)
                             (let
                                 ((n
                                   (y-length h))
                                  (i k)
                                  (unset
                                   (list nil)))
                               (if
                                   (and
                                    (>= i 0)
                                    (<= i
                                        (1- n)))
                                   (progn
                                     (while
                                         (< i n)
                                       (let
                                           ((x
                                             (gethash
                                              (1+ i)
                                              h unset)))
                                         (if
                                             (eq x unset)
                                             (remhash i h)
                                           (puthash i x h)))
                                       (setq i
                                             (1+ i)))
                                     (remhash i h))
                                 (remhash k h)))
                           (remhash k h))
                       (puthash k v h))
                     h)
                 (let*
                     ((l h))
                   (if
                       (and
                        (symbolp k)
                        (not
                         (keywordp k)))
                       (setq k
                             (intern
                              (concat ":"
                                      (symbol-name k)))))
                   (if
                       (listp h)
                       (catch 'y-break
                         (if wipe\?
                             (let
                                 ((l1 h)
                                  (p nil)
                                  (head\? t))
                               (let*
                                   ((i1 -1))
                                 (while h
                                   (let*
                                       ((var
                                         (if
                                             (keywordp
                                              (car h))
                                             (y-%key
                                              (car h))
                                           (setq i1
                                                 (1+ i1))))
                                        (_val
                                         (if
                                             (keywordp
                                              (car h))
                                             (cadr h)
                                           (car h))))
                                     (if
                                         (eq var k)
                                         (if head\?
                                             (progn
                                               (setq l1
                                                     (if
                                                         (keywordp k)
                                                         (cddr h)
                                                       (cdr h)))
                                               (setq h l1))
                                           (progn
                                             (setcdr
                                              (if
                                                  (keywordp
                                                   (car p))
                                                  (cdr p)
                                                p)
                                              (if
                                                  (keywordp k)
                                                  (cddr h)
                                                (cdr h)))
                                             (setq h p)))
                                       (setq head\? nil))
                                     (setq p h))
                                   (setq h
                                         (if
                                             (keywordp
                                              (car h))
                                             (cddr h)
                                           (cdr h))))
                                 nil)
                               (setq l l1))
                           (if
                               (or
                                (keywordp k)
                                (>= k 0))
                               (progn
                                 (if
                                     (null l)
                                     (progn
                                       (setq l
                                             (if
                                                 (keywordp k)
                                                 (list k nil)
                                               (list nil)))
                                       (setq h l)))
                                 (let*
                                     ((i2 -1))
                                   (while h
                                     (let*
                                         ((var
                                           (if
                                               (keywordp
                                                (car h))
                                               (y-%key
                                                (car h))
                                             (setq i2
                                                   (1+ i2))))
                                          (_val
                                           (if
                                               (keywordp
                                                (car h))
                                               (cadr h)
                                             (car h))))
                                       (if
                                           (eq var
                                               (y-%key k))
                                           (progn
                                             (if
                                                 (integerp k)
                                                 (setcar h v)
                                               (setcar
                                                (cdr h)
                                                v))
                                             (throw 'y-break l)))
                                       (if
                                           (null
                                            (y-next h))
                                           (progn
                                             (if
                                                 (keywordp k)
                                                 (nconc h
                                                        (list k v))
                                               (nconc h
                                                      (list nil))))))
                                     (setq h
                                           (if
                                               (keywordp
                                                (car h))
                                               (cddr h)
                                             (cdr h))))
                                   nil)))))
                     (if wipe\?
                         (error
                          (format "Can't wipe index %s of %S" k h))
                       (aset h k v)))
                   l)))))
       (defalias 'y-wipe
         (cons 'macro
               #'(lambda
                   (place)
                   (gv-get place
                           #'(lambda
                               (getter _setter)
                               (if
                                   (eq 'y-get
                                       (car-safe getter))
                                   (list 'y-set
                                         (cadr getter)
                                         (cons 'y-put
                                               (cdr getter)))
                                 (error
                                  (format "Can't wipe %S" place))))))))
       (defalias 'y-length
         #'(lambda
             (h &optional upto)
             (catch 'y-break
               (if
                   (listp h)
                   (let*
                       ((n -1))
                     (let*
                         ((i3 -1))
                       (while h
                         (let*
                             ((k
                               (if
                                   (keywordp
                                    (car h))
                                   (y-%key
                                    (car h))
                                 (setq i3
                                       (1+ i3))))
                              (_v
                               (if
                                   (keywordp
                                    (car h))
                                   (cadr h)
                                 (car h))))
                           (if
                               (integerp k)
                               (progn
                                 (setq n
                                       (max n k))
                                 (if
                                     (and upto
                                          (>= n upto))
                                     (progn
                                       (throw 'y-break
                                              (+ n 1)))))))
                         (setq h
                               (if
                                   (keywordp
                                    (car h))
                                   (cddr h)
                                 (cdr h))))
                       nil)
                     (+ n 1))
                 (if
                     (hash-table-p h)
                     (let*
                         ((n -1))
                       (maphash
                        #'(lambda
                            (k _v)
                            (if
                                (integerp k)
                                (progn
                                  (setq n
                                        (max n k))
                                  (if
                                      (and upto
                                           (>= n upto))
                                      (progn
                                        (throw 'y-break
                                               (+ n 1)))))))
                        h)
                       (+ n 1))
                   (length h))))))
       (defalias 'y-%if
         (cons 'macro
               #'(lambda
                   (&rest args)
                   (cons 'if args))))
       (defalias 'y-do
         (cons 'macro
               #'(lambda
                   (&rest body)
                   (let*
                       ((max-lisp-eval-depth 4450)
                        (y-environment
                         (apply 'vector
                                (append y-environment nil))))
                     (macroexp-progn
                      (mapcar 'y-macroexpand body))))))
       (progn
         (defvar y-environment
           (list
            (make-hash-table :test 'eq)))
         (progn
           (defalias 'y-setenv
             #'(lambda
                 (k &rest ks)
                 (progn
                   (let*
                       ((i4
                         (if
                             (memq :toplevel ks)
                             0
                           (-
                            (y-length y-environment)
                            1))))
                     (progn
                       (let*
                           ((frame
                             (y-get y-environment i4)))
                         (let*
                             ((entry
                               (or
                                (y-get frame k)
                                (make-hash-table :test 'eq))))
                           (progn
                             (let*
                                 ((o0 ks))
                               (let*
                                   ((f0
                                     #'(lambda
                                         (k v)
                                         (progn
                                           (let*
                                               ((k2 k)
                                                (v1 v))
                                             (setq entry
                                                   (y-put entry k2 v1))
                                             v1)))))
                                 (progn
                                   (if
                                       (hash-table-p o0)
                                       (maphash f0 o0)
                                     (if
                                         (listp o0)
                                         (let*
                                             ((i16 -1))
                                           (while o0
                                             (let*
                                                 ((k
                                                   (if
                                                       (keywordp
                                                        (car o0))
                                                       (y-%key
                                                        (car o0))
                                                     (setq i16
                                                           (1+ i16))))
                                                  (a0
                                                   (if
                                                       (keywordp
                                                        (car o0))
                                                       (cadr o0)
                                                     (car o0))))
                                               (funcall f0 k a0))
                                             (setq o0
                                                   (if
                                                       (keywordp
                                                        (car o0))
                                                       (cddr o0)
                                                     (cdr o0))))
                                           nil)
                                       (let*
                                           ((n0
                                             (y-length o0)))
                                         (progn
                                           (let*
                                               ((k 0))
                                             (progn
                                               (while
                                                   (< k n0)
                                                 (let*
                                                     ((a0
                                                       (y-get o0 k)))
                                                   (progn
                                                     (funcall f0 k a0)))
                                                 (setq k
                                                       (+ k 1))))))))))))
                             (let*
                                 ((k3 k)
                                  (v2 entry))
                               (setq frame
                                     (y-put frame k3 v2))
                               v2)))))))))
           (y-setenv 'setenv :symbol 'y-setenv))
         (progn
           (defalias 'y-getenv
             #'(lambda
                 (k &optional p)
                 (progn
                   (let*
                       ((i5
                         (-
                          (y-length y-environment)
                          1)))
                     (progn
                       (progn
                         (catch 'y-break
                           (while
                               (>= i5 0)
                             (let*
                                 ((b
                                   (y-get
                                    (y-get y-environment i5)
                                    k)))
                               (progn
                                 (if b
                                     (throw 'y-break
                                            (if p
                                                (y-get b p)
                                              b))
                                   (setq i5
                                         (- i5 1)))))))))))))
           (y-setenv 'getenv :symbol 'y-getenv))
         (y-setenv 'unique :symbol 'y-unique)
         (y-setenv 'let-unique :symbol 'y-let-unique)
         (y-setenv 'at :symbol 'get)
         (y-setenv 'set :macro
                   #'(lambda
                       (&rest args)
                       (progn
                         (cons 'y-set args))))
         (y-setenv 'wipe :macro
                   #'(lambda
                       (&rest args)
                       (progn
                         (cons 'y-wipe args))))
         (y-setenv 'get :symbol 'y-get)
         (y-setenv '\# :symbol 'y-length)
         (y-setenv '= :symbol 'eql)
         (y-setenv 'environment :symbol 'y-environment)
         (y-setenv '%if :symbol 'y-%if)
         (progn
           (defalias 'y-nil-p
             #'(lambda
                 (x)
                 (progn
                   (eql x nil))))
           (y-setenv 'nil\? :symbol 'y-nil-p))
         (progn
           (defalias 'y-is-p
             #'(lambda
                 (x)
                 (progn
                   (not
                    (y-nil-p x)))))
           (y-setenv 'is\? :symbol 'y-is-p))
         (progn
           (defalias 'y-none-p
             #'(lambda
                 (x)
                 (progn
                   (eql
                    (y-length x 0)
                    0))))
           (y-setenv 'none\? :symbol 'y-none-p))
         (progn
           (defalias 'y-some-p
             #'(lambda
                 (x)
                 (progn
                   (>
                    (y-length x 0)
                    0))))
           (y-setenv 'some\? :symbol 'y-some-p))
         (progn
           (defalias 'y-one-p
             #'(lambda
                 (x)
                 (progn
                   (eql
                    (y-length x 1)
                    1))))
           (y-setenv 'one\? :symbol 'y-one-p))
         (progn
           (defalias 'y-two-p
             #'(lambda
                 (x)
                 (progn
                   (eql
                    (y-length x 2)
                    2))))
           (y-setenv 'two\? :symbol 'y-two-p))
         (progn
           (defalias 'y-hd
             #'(lambda
                 (l)
                 (progn
                   (y-get l 0))))
           (y-setenv 'hd :symbol 'y-hd))
         (progn
           (defalias 'y-type
             #'(lambda
                 (x)
                 (progn
                   (type-of x))))
           (y-setenv 'type :symbol 'y-type))
         (progn
           (defalias 'y-number-p
             #'(lambda
                 (x)
                 (progn
                   (or
                    (integerp x)
                    (numberp x)))))
           (y-setenv 'number\? :symbol 'y-number-p))
         (progn
           (defalias 'y-obj-p
             #'(lambda
                 (x)
                 (progn
                   (hash-table-p x))))
           (y-setenv 'obj\? :symbol 'y-obj-p))
         (y-setenv 'obj :macro
                   #'(lambda nil
                       (progn
                         '(make-hash-table :test 'eq))))
         (progn
           (defalias 'y-atom-p
             #'(lambda
                 (x)
                 (progn
                   (or
                    (y-nil-p x)
                    (symbolp x)
                    (stringp x)
                    (y-number-p x)))))
           (y-setenv 'atom\? :symbol 'y-atom-p))
         (progn
           (defalias 'y-clip
             #'(lambda
                 (s from &optional upto)
                 (progn
                   (let*
                       ((n1
                         (length s)))
                     (progn
                       (let*
                           ((i
                             (if
                                 (or
                                  (y-nil-p from)
                                  (< from 0))
                                 0 from)))
                         (let*
                             ((j
                               (if
                                   (or
                                    (y-nil-p upto)
                                    (> upto n1))
                                   n1
                                 (max upto i))))
                           (progn
                             (substring s i j)))))))))
           (y-setenv 'clip :symbol 'y-clip))
         (progn
           (defalias 'y--chop-p
             #'(lambda
                 (x from upto)
                 (progn
                   (and
                    (consp x)
                    (eql from 1)
                    (null upto)
                    (not
                     (keywordp
                      (car x)))))))
           (y-setenv 'chop\? :symbol 'y--chop-p))
         (progn
           (defalias 'y-cut
             #'(lambda
                 (x &optional from upto)
                 (progn
                   (if
                       (y--chop-p x from upto)
                       (cdr x)
                     (let*
                         ((l0
                           (if
                               (y-obj-p x)
                               (make-hash-table :test 'eq))))
                       (progn
                         (progn
                           (let*
                               ((j 0))
                             (let*
                                 ((i
                                   (if
                                       (or
                                        (y-nil-p from)
                                        (< from 0))
                                       0 from)))
                               (let*
                                   ((n
                                     (y-length x)))
                                 (let*
                                     ((upto
                                       (if
                                           (or
                                            (y-nil-p upto)
                                            (> upto n))
                                           n upto)))
                                   (progn
                                     (while
                                         (< i upto)
                                       (let*
                                           ((k4 j)
                                            (v3
                                             (y-get x i)))
                                         (setq l0
                                               (y-put l0 k4 v3))
                                         v3)
                                       (setq i
                                             (+ i 1))
                                       (setq j
                                             (+ j 1)))
                                     (let*
                                         ((o1 x))
                                       (let*
                                           ((f1
                                             #'(lambda
                                                 (k v)
                                                 (progn
                                                   (if
                                                       (y-number-p k)
                                                       nil
                                                     (let*
                                                         ((k5 k)
                                                          (v4 v))
                                                       (setq l0
                                                             (y-put l0 k5 v4))
                                                       v4))))))
                                         (progn
                                           (if
                                               (hash-table-p o1)
                                               (maphash f1 o1)
                                             (if
                                                 (listp o1)
                                                 (let*
                                                     ((i17 -1))
                                                   (while o1
                                                     (let*
                                                         ((k
                                                           (if
                                                               (keywordp
                                                                (car o1))
                                                               (y-%key
                                                                (car o1))
                                                             (setq i17
                                                                   (1+ i17))))
                                                          (a1
                                                           (if
                                                               (keywordp
                                                                (car o1))
                                                               (cadr o1)
                                                             (car o1))))
                                                       (funcall f1 k a1))
                                                     (setq o1
                                                           (if
                                                               (keywordp
                                                                (car o1))
                                                               (cddr o1)
                                                             (cdr o1))))
                                                   nil)
                                               (let*
                                                   ((n2
                                                     (y-length o1)))
                                                 (progn
                                                   (let*
                                                       ((k 0))
                                                     (progn
                                                       (while
                                                           (< k n2)
                                                         (let*
                                                             ((a1
                                                               (y-get o1 k)))
                                                           (progn
                                                             (funcall f1 k a1)))
                                                         (setq k
                                                               (+ k 1)))))))))))))))))
                           l0)))))))
           (y-setenv 'cut :symbol 'y-cut))
         (progn
           (defalias 'y-keys
             #'(lambda
                 (x)
                 (progn
                   (let*
                       ((l1
                         (if
                             (y-obj-p x)
                             (make-hash-table :test 'eq))))
                     (progn
                       (progn
                         (let*
                             ((o2 x))
                           (let*
                               ((f2
                                 #'(lambda
                                     (k v)
                                     (progn
                                       (if
                                           (y-number-p k)
                                           nil
                                         (let*
                                             ((k6 k)
                                              (v5 v))
                                           (setq l1
                                                 (y-put l1 k6 v5))
                                           v5))))))
                             (progn
                               (if
                                   (hash-table-p o2)
                                   (maphash f2 o2)
                                 (if
                                     (listp o2)
                                     (let*
                                         ((i18 -1))
                                       (while o2
                                         (let*
                                             ((k
                                               (if
                                                   (keywordp
                                                    (car o2))
                                                   (y-%key
                                                    (car o2))
                                                 (setq i18
                                                       (1+ i18))))
                                              (a2
                                               (if
                                                   (keywordp
                                                    (car o2))
                                                   (cadr o2)
                                                 (car o2))))
                                           (funcall f2 k a2))
                                         (setq o2
                                               (if
                                                   (keywordp
                                                    (car o2))
                                                   (cddr o2)
                                                 (cdr o2))))
                                       nil)
                                   (let*
                                       ((n3
                                         (y-length o2)))
                                     (progn
                                       (let*
                                           ((k 0))
                                         (progn
                                           (while
                                               (< k n3)
                                             (let*
                                                 ((a2
                                                   (y-get o2 k)))
                                               (progn
                                                 (funcall f2 k a2)))
                                             (setq k
                                                   (+ k 1))))))))))))
                         l1))))))
           (y-setenv 'keys :symbol 'y-keys))
         (progn
           (defalias 'y-edge
             #'(lambda
                 (x)
                 (progn
                   (-
                    (y-length x)
                    1))))
           (y-setenv 'edge :symbol 'y-edge))
         (progn
           (defalias 'y-tl
             #'(lambda
                 (l)
                 (progn
                   (y-cut l 1))))
           (y-setenv 'tl :symbol 'y-tl))
         (progn
           (defalias 'y-last
             #'(lambda
                 (l)
                 (progn
                   (y-get l
                          (y-edge l)))))
           (y-setenv 'last :symbol 'y-last))
         (progn
           (defalias 'y-almost
             #'(lambda
                 (l)
                 (progn
                   (y-cut l 0
                          (y-edge l)))))
           (y-setenv 'almost :symbol 'y-almost))
         (y-setenv 'add :macro
                   #'(lambda
                       (l x)
                       (progn
                         (progn
                           (or
                            (y-atom-p l)
                            (signal 'cl-assertion-failed
                                    (list
                                     '(y-atom-p l))))
                           nil)
                         (cons 'progn
                               (cons
                                (list 'set
                                      (list 'at l
                                            (list '\# l))
                                      x)
                                '(nil))))))
         (y-setenv 'drop :macro
                   #'(lambda
                       (l)
                       (progn
                         (progn
                           (or
                            (y-atom-p l)
                            (signal 'cl-assertion-failed
                                    (list
                                     '(y-atom-p l))))
                           nil)
                         (list 'prog1
                               (list 'at l
                                     (list 'edge l))
                               (list 'set l
                                     (list 'almost l))))))
         (progn
           (defalias 'y-reverse
             #'(lambda
                 (l)
                 (progn
                   (let*
                       ((l10
                         (y-keys l)))
                     (progn
                       (progn
                         (let*
                             ((i
                               (y-edge l)))
                           (progn
                             (while
                                 (>= i 0)
                               (progn
                                 (let*
                                     ((k7
                                       (y-length l10))
                                      (v6
                                       (y-get l i)))
                                   (setq l10
                                         (y-put l10 k7 v6))
                                   v6)
                                 nil)
                               (setq i
                                     (- i 1)))))
                         l10))))))
           (y-setenv 'reverse :symbol 'y-reverse))
         (progn
           (defalias 'y-reduce
             #'(lambda
                 (f x)
                 (progn
                   (if
                       (y-none-p x)
                       nil
                     (if
                         (y-one-p x)
                         (y-hd x)
                       (funcall f
                                (y-hd x)
                                (y-reduce f
                                          (y-tl x))))))))
           (y-setenv 'reduce :symbol 'y-reduce))
         (progn
           (defalias 'y-join
             #'(lambda
                 (&rest ls)
                 (progn
                   (if
                       (y-two-p ls)
                       (let*
                           ((var00 ls))
                         (progn
                           (let*
                               ((a
                                 (y-get var00 '0)))
                             (let*
                                 ((b
                                   (y-get var00 '1)))
                               (progn
                                 (if
                                     (and a b)
                                     (let*
                                         ((c
                                           (if
                                               (y-obj-p a)
                                               (make-hash-table :test 'eq))))
                                       (let*
                                           ((o
                                             (y-length a)))
                                         (progn
                                           (let*
                                               ((o3 a))
                                             (let*
                                                 ((f3
                                                   #'(lambda
                                                       (k v)
                                                       (progn
                                                         (let*
                                                             ((k8 k)
                                                              (v7 v))
                                                           (setq c
                                                                 (y-put c k8 v7))
                                                           v7)))))
                                               (progn
                                                 (if
                                                     (hash-table-p o3)
                                                     (maphash f3 o3)
                                                   (if
                                                       (listp o3)
                                                       (let*
                                                           ((i19 -1))
                                                         (while o3
                                                           (let*
                                                               ((k
                                                                 (if
                                                                     (keywordp
                                                                      (car o3))
                                                                     (y-%key
                                                                      (car o3))
                                                                   (setq i19
                                                                         (1+ i19))))
                                                                (a3
                                                                 (if
                                                                     (keywordp
                                                                      (car o3))
                                                                     (cadr o3)
                                                                   (car o3))))
                                                             (funcall f3 k a3))
                                                           (setq o3
                                                                 (if
                                                                     (keywordp
                                                                      (car o3))
                                                                     (cddr o3)
                                                                   (cdr o3))))
                                                         nil)
                                                     (let*
                                                         ((n4
                                                           (y-length o3)))
                                                       (progn
                                                         (let*
                                                             ((k 0))
                                                           (progn
                                                             (while
                                                                 (< k n4)
                                                               (let*
                                                                   ((a3
                                                                     (y-get o3 k)))
                                                                 (progn
                                                                   (funcall f3 k a3)))
                                                               (setq k
                                                                     (+ k 1))))))))))))
                                           (let*
                                               ((o4 b))
                                             (let*
                                                 ((f4
                                                   #'(lambda
                                                       (k v)
                                                       (progn
                                                         (if
                                                             (y-number-p k)
                                                             (progn
                                                               (setq k
                                                                     (+ k o))))
                                                         (let*
                                                             ((k9 k)
                                                              (v8 v))
                                                           (setq c
                                                                 (y-put c k9 v8))
                                                           v8)))))
                                               (progn
                                                 (if
                                                     (hash-table-p o4)
                                                     (maphash f4 o4)
                                                   (if
                                                       (listp o4)
                                                       (let*
                                                           ((i20 -1))
                                                         (while o4
                                                           (let*
                                                               ((k
                                                                 (if
                                                                     (keywordp
                                                                      (car o4))
                                                                     (y-%key
                                                                      (car o4))
                                                                   (setq i20
                                                                         (1+ i20))))
                                                                (a4
                                                                 (if
                                                                     (keywordp
                                                                      (car o4))
                                                                     (cadr o4)
                                                                   (car o4))))
                                                             (funcall f4 k a4))
                                                           (setq o4
                                                                 (if
                                                                     (keywordp
                                                                      (car o4))
                                                                     (cddr o4)
                                                                   (cdr o4))))
                                                         nil)
                                                     (let*
                                                         ((n5
                                                           (y-length o4)))
                                                       (progn
                                                         (let*
                                                             ((k0 0))
                                                           (progn
                                                             (progn
                                                               (while
                                                                   (< k0 n5)
                                                                 (let*
                                                                     ((a4
                                                                       (y-get o4 k0)))
                                                                   (progn
                                                                     (funcall f4 k0 a4)))
                                                                 (setq k0
                                                                       (+ k0 1)))))))))))))
                                           c)))
                                   (or a b nil)))))))
                     (or
                      (y-reduce 'y-join ls)
                      nil)))))
           (y-setenv 'join :symbol 'y-join))
         (progn
           (defalias 'y-find
             #'(lambda
                 (f l)
                 (progn
                   (catch 'y-break
                     (let*
                         ((o50 l))
                       (progn
                         (let*
                             ((f5
                               #'(lambda
                                   (i6 x)
                                   (progn
                                     (let*
                                         ((y
                                           (funcall f x)))
                                       (progn
                                         (if y
                                             (throw 'y-break y))))))))
                           (progn
                             (if
                                 (hash-table-p o50)
                                 (maphash f5 o50)
                               (if
                                   (listp o50)
                                   (let*
                                       ((i21 -1))
                                     (while o50
                                       (let*
                                           ((i6
                                             (if
                                                 (keywordp
                                                  (car o50))
                                                 (y-%key
                                                  (car o50))
                                               (setq i21
                                                     (1+ i21))))
                                            (a5
                                             (if
                                                 (keywordp
                                                  (car o50))
                                                 (cadr o50)
                                               (car o50))))
                                         (funcall f5 i6 a5))
                                       (setq o50
                                             (if
                                                 (keywordp
                                                  (car o50))
                                                 (cddr o50)
                                               (cdr o50))))
                                     nil)
                                 (let*
                                     ((n6
                                       (y-length o50)))
                                   (progn
                                     (let*
                                         ((i6 0))
                                       (progn
                                         (while
                                             (< i6 n6)
                                           (let*
                                               ((a5
                                                 (y-get o50 i6)))
                                             (progn
                                               (funcall f5 i6 a5)))
                                           (setq i6
                                                 (+ i6 1)))))))))))))))))
           (y-setenv 'find :symbol 'y-find))
         (progn
           (defalias 'y-first
             #'(lambda
                 (f l)
                 (progn
                   (catch 'y-break
                     (let*
                         ((x00 l))
                       (progn
                         (let*
                             ((n7
                               (y-length x00)))
                           (progn
                             (let*
                                 ((i7 0))
                               (progn
                                 (while
                                     (< i7 n7)
                                   (let*
                                       ((x
                                         (y-get x00 i7)))
                                     (progn
                                       (let*
                                           ((y
                                             (funcall f x)))
                                         (progn
                                           (if y
                                               (throw 'y-break y))))))
                                   (setq i7
                                         (+ i7 1)))))))))))))
           (y-setenv 'first :symbol 'y-first))
         (progn
           (defalias 'y-in-p
             #'(lambda
                 (x l)
                 (progn
                   (y-find
                    #'(lambda
                        (y)
                        (progn
                          (eql x y)))
                    l))))
           (y-setenv 'in\? :symbol 'y-in-p))
         (progn
           (defalias 'y-pair
             #'(lambda
                 (l)
                 (progn
                   (let*
                       ((l11
                         (if
                             (y-obj-p l)
                             (make-hash-table :test 'eq))))
                     (progn
                       (progn
                         (let*
                             ((n
                               (y-length l)))
                           (progn
                             (let*
                                 ((i 0))
                               (progn
                                 (while
                                     (< i n)
                                   (progn
                                     (let*
                                         ((k10
                                           (y-length l11))
                                          (v9
                                           (list
                                            (y-get l i)
                                            (y-get l
                                                   (+ i 1)))))
                                       (setq l11
                                             (y-put l11 k10 v9))
                                       v9)
                                     nil)
                                   (setq i
                                         (+ i 1))
                                   (setq i
                                         (+ i 1)))))))
                         l11))))))
           (y-setenv 'pair :symbol 'y-pair))
         (progn
           (defalias 'y-map
             #'(lambda
                 (f x)
                 (progn
                   (let*
                       ((l2
                         (if
                             (y-obj-p x)
                             (make-hash-table :test 'eq))))
                     (progn
                       (progn
                         (let*
                             ((x1 x))
                           (let*
                               ((n8
                                 (y-length x1)))
                             (progn
                               (let*
                                   ((i8 0))
                                 (progn
                                   (while
                                       (< i8 n8)
                                     (let*
                                         ((v
                                           (y-get x1 i8)))
                                       (progn
                                         (let*
                                             ((y
                                               (funcall f v)))
                                           (progn
                                             (if
                                                 (y-is-p y)
                                                 (progn
                                                   (let*
                                                       ((k11
                                                         (y-length l2))
                                                        (v10 y))
                                                     (setq l2
                                                           (y-put l2 k11 v10))
                                                     v10)
                                                   nil))))))
                                     (setq i8
                                           (+ i8 1))))))))
                         (let*
                             ((o6 x))
                           (let*
                               ((f6
                                 #'(lambda
                                     (k v)
                                     (progn
                                       (if
                                           (y-number-p k)
                                           nil
                                         (let*
                                             ((y0
                                               (funcall f v)))
                                           (progn
                                             (progn
                                               (if
                                                   (y-is-p y0)
                                                   (progn
                                                     (let*
                                                         ((k12 k)
                                                          (v11 y0))
                                                       (setq l2
                                                             (y-put l2 k12 v11))
                                                       v11)))))))))))
                             (progn
                               (if
                                   (hash-table-p o6)
                                   (maphash f6 o6)
                                 (if
                                     (listp o6)
                                     (let*
                                         ((i22 -1))
                                       (while o6
                                         (let*
                                             ((k
                                               (if
                                                   (keywordp
                                                    (car o6))
                                                   (y-%key
                                                    (car o6))
                                                 (setq i22
                                                       (1+ i22))))
                                              (a6
                                               (if
                                                   (keywordp
                                                    (car o6))
                                                   (cadr o6)
                                                 (car o6))))
                                           (funcall f6 k a6))
                                         (setq o6
                                               (if
                                                   (keywordp
                                                    (car o6))
                                                   (cddr o6)
                                                 (cdr o6))))
                                       nil)
                                   (let*
                                       ((n9
                                         (y-length o6)))
                                     (progn
                                       (let*
                                           ((k 0))
                                         (progn
                                           (while
                                               (< k n9)
                                             (let*
                                                 ((a6
                                                   (y-get o6 k)))
                                               (progn
                                                 (funcall f6 k a6)))
                                             (setq k
                                                   (+ k 1))))))))))))
                         l2))))))
           (y-setenv 'map :symbol 'y-map))
         (progn
           (defalias 'y-keep
             #'(lambda
                 (f x)
                 (progn
                   (y-map
                    #'(lambda
                        (v)
                        (progn
                          (if
                              (funcall f v)
                              (progn v))))
                    x))))
           (y-setenv 'keep :symbol 'y-keep))
         (progn
           (defalias 'y-keys-p
             #'(lambda
                 (l)
                 (progn
                   (catch 'y-break
                     (let*
                         ((o70 l))
                       (progn
                         (let*
                             ((f7
                               #'(lambda
                                   (k v)
                                   (progn
                                     (setq v v)
                                     (if
                                         (y-number-p k)
                                         nil
                                       (throw 'y-break t))))))
                           (progn
                             (if
                                 (hash-table-p o70)
                                 (maphash f7 o70)
                               (if
                                   (listp o70)
                                   (let*
                                       ((i23 -1))
                                     (while o70
                                       (let*
                                           ((k
                                             (if
                                                 (keywordp
                                                  (car o70))
                                                 (y-%key
                                                  (car o70))
                                               (setq i23
                                                     (1+ i23))))
                                            (a7
                                             (if
                                                 (keywordp
                                                  (car o70))
                                                 (cadr o70)
                                               (car o70))))
                                         (funcall f7 k a7))
                                       (setq o70
                                             (if
                                                 (keywordp
                                                  (car o70))
                                                 (cddr o70)
                                               (cdr o70))))
                                     nil)
                                 (let*
                                     ((n10
                                       (y-length o70)))
                                   (progn
                                     (let*
                                         ((k 0))
                                       (progn
                                         (while
                                             (< k n10)
                                           (let*
                                               ((a7
                                                 (y-get o70 k)))
                                             (progn
                                               (funcall f7 k a7)))
                                           (setq k
                                                 (+ k 1)))))))))))))
                     nil))))
           (y-setenv 'keys\? :symbol 'y-keys-p))
         (progn
           (defalias 'y-empty-p
             #'(lambda
                 (l)
                 (progn
                   (catch 'y-break
                     (let*
                         ((o80 l))
                       (progn
                         (let*
                             ((f8
                               #'(lambda
                                   (i9 x)
                                   (progn
                                     (setq x x)
                                     (throw 'y-break nil)))))
                           (progn
                             (if
                                 (hash-table-p o80)
                                 (maphash f8 o80)
                               (if
                                   (listp o80)
                                   (let*
                                       ((i24 -1))
                                     (while o80
                                       (let*
                                           ((i9
                                             (if
                                                 (keywordp
                                                  (car o80))
                                                 (y-%key
                                                  (car o80))
                                               (setq i24
                                                     (1+ i24))))
                                            (a8
                                             (if
                                                 (keywordp
                                                  (car o80))
                                                 (cadr o80)
                                               (car o80))))
                                         (funcall f8 i9 a8))
                                       (setq o80
                                             (if
                                                 (keywordp
                                                  (car o80))
                                                 (cddr o80)
                                               (cdr o80))))
                                     nil)
                                 (let*
                                     ((n11
                                       (y-length o80)))
                                   (progn
                                     (let*
                                         ((i9 0))
                                       (progn
                                         (while
                                             (< i9 n11)
                                           (let*
                                               ((a8
                                                 (y-get o80 i9)))
                                             (progn
                                               (funcall f8 i9 a8)))
                                           (setq i9
                                                 (+ i9 1)))))))))))))
                     t))))
           (y-setenv 'empty\? :symbol 'y-empty-p))
         (progn
           (defalias 'y-toplevel-p
             #'(lambda nil
                 (progn
                   (y-one-p y-environment))))
           (y-setenv 'toplevel\? :symbol 'y-toplevel-p))
         (progn
           (defalias 'y-print
             #'(lambda
                 (x)
                 (progn
                   (princ
                    (format "%s\n" x))
                   nil)))
           (y-setenv 'print :symbol 'y-print))
         (progn
           (defalias 'y--id
             #'(lambda
                 (x)
                 (progn
                   (let*
                       ((s0
                         (append
                          (if
                              (symbolp x)
                              (symbol-name x)
                            x)
                          nil)))
                     (progn
                       (progn
                         (if
                             (eq 63
                                 (y-get s0
                                        (y-edge s0)))
                             (progn
                               (if
                                   (memq 45 s0)
                                   (progn
                                     (let*
                                         ((k13
                                           (y-edge s0))
                                          (v12 45))
                                       (setq s0
                                             (y-put s0 k13 v12))
                                       v12)
                                     (let*
                                         ((k14
                                           (y-length s0))
                                          (v13 112))
                                       (setq s0
                                             (y-put s0 k14 v13))
                                       v13))
                                 (let*
                                     ((k15
                                       (y-edge s0))
                                      (v14 112))
                                   (setq s0
                                         (y-put s0 k15 v14))
                                   v14))))
                         (intern
                          (concat s0))))))))
           (y-setenv 'id :symbol 'y--id))
         (defvar y-module nil)
         (progn
           (defalias 'y--module-name
             #'(lambda nil
                 (progn
                   (or y-module
                       (let*
                           ((file0
                             (or load-file-name
                                 (buffer-file-name))))
                         (progn
                           (progn
                             (if file0
                                 (file-name-base file0)
                               (buffer-name)))))))))
           (y-setenv 'module-name :symbol 'y--module-name))
         (progn
           (defalias 'y--global-id
             #'(lambda
                 (prefix name)
                 (progn
                   (let*
                       ((s1
                         (if
                             (stringp name)
                             name
                           (symbol-name name))))
                     (progn
                       (progn
                         (if
                             (eq 0
                                 (string-match
                                  (regexp-quote prefix)
                                  s1))
                             name
                           (y--id
                            (concat prefix s1)))))))))
           (y-setenv 'global-id :symbol 'y--global-id))
         (progn
           (defalias 'y--macro-function
             #'(lambda
                 (k)
                 (progn
                   (y-getenv k 'macro))))
           (y-setenv 'macro-function :symbol 'y--macro-function))
         (progn
           (defalias 'y--macro-p
             #'(lambda
                 (k)
                 (progn
                   (y--macro-function k))))
           (y-setenv 'macro\? :symbol 'y--macro-p))
         (progn
           (defalias 'y--symbol-expansion
             #'(lambda
                 (k)
                 (progn
                   (y-getenv k 'symbol))))
           (y-setenv 'symbol-expansion :symbol 'y--symbol-expansion))
         (progn
           (defalias 'y--symbol-p
             #'(lambda
                 (k)
                 (progn
                   (let*
                       ((v0
                         (y--symbol-expansion k)))
                     (progn
                       (progn
                         (and v0
                              (not
                               (eq v0 k)))))))))
           (y-setenv 'symbol\? :symbol 'y--symbol-p))
         (progn
           (defalias 'y--variable-p
             #'(lambda
                 (k)
                 (progn
                   (let*
                       ((i10
                         (y-edge y-environment)))
                     (progn
                       (progn
                         (catch 'y-break
                           (while
                               (>= i10 0)
                             (let*
                                 ((b
                                   (y-get
                                    (y-get y-environment i10)
                                    k)))
                               (progn
                                 (if b
                                     (throw 'y-break
                                            (y-get b 'variable))
                                   (setq i10
                                         (1- i10)))))))))))))
           (y-setenv 'variable\? :symbol 'y--variable-p))
         (progn
           (defalias 'y--bound-p
             #'(lambda
                 (x)
                 (progn
                   (or
                    (y--macro-p x)
                    (y--symbol-p x)
                    (y--variable-p x)))))
           (y-setenv 'bound\? :symbol 'y--bound-p))
         (progn
           (defalias 'y--bind
             #'(lambda
                 (lh rh)
                 (progn
                   (if
                       (y-atom-p lh)
                       (list lh rh)
                     (let*
                         ((var
                           (y-unique 'var)))
                       (let*
                           ((bs0
                             (list var rh)))
                         (progn
                           (progn
                             (let*
                                 ((o9 lh))
                               (let*
                                   ((f9
                                     #'(lambda
                                         (k v)
                                         (progn
                                           (let*
                                               ((x
                                                 (if
                                                     (eql k 'rest)
                                                     (list 'cut var
                                                           (y-length lh))
                                                   (list 'get var
                                                         (list 'quote k)))))
                                             (progn
                                               (if
                                                   (y-is-p k)
                                                   (progn
                                                     (let*
                                                         ((k1
                                                           (if
                                                               (eql v t)
                                                               k v)))
                                                       (progn
                                                         (progn
                                                           (setq bs0
                                                                 (y-join bs0
                                                                         (y--bind k1 x))))))))))))))
                                 (progn
                                   (if
                                       (hash-table-p o9)
                                       (maphash f9 o9)
                                     (if
                                         (listp o9)
                                         (let*
                                             ((i25 -1))
                                           (while o9
                                             (let*
                                                 ((k
                                                   (if
                                                       (keywordp
                                                        (car o9))
                                                       (y-%key
                                                        (car o9))
                                                     (setq i25
                                                           (1+ i25))))
                                                  (a9
                                                   (if
                                                       (keywordp
                                                        (car o9))
                                                       (cadr o9)
                                                     (car o9))))
                                               (funcall f9 k a9))
                                             (setq o9
                                                   (if
                                                       (keywordp
                                                        (car o9))
                                                       (cddr o9)
                                                     (cdr o9))))
                                           nil)
                                       (let*
                                           ((n12
                                             (y-length o9)))
                                         (progn
                                           (let*
                                               ((k 0))
                                             (progn
                                               (while
                                                   (< k n12)
                                                 (let*
                                                     ((a9
                                                       (y-get o9 k)))
                                                   (progn
                                                     (funcall f9 k a9)))
                                                 (setq k
                                                       (+ k 1))))))))))))
                             bs0))))))))
           (y-setenv 'bind :symbol 'y--bind))
         (progn
           (defalias 'y--bind*
             #'(lambda
                 (args body)
                 (progn
                   (if
                       (and args
                            (y-atom-p args))
                       (y--bind*
                        (list '&rest args)
                        body)
                     (let*
                         ((args10 nil))
                       (progn
                         (let*
                             ((bs nil))
                           (progn
                             (mapc
                              #'(lambda
                                  (x)
                                  (progn
                                    (if
                                        (atom x)
                                        (setq args10
                                              (nconc args10
                                                     (list x)))
                                      (let*
                                          ((id1
                                            (y-unique 'id1)))
                                        (setq args10
                                              (nconc args10
                                                     (list id1)))
                                        (setq bs
                                              (y-join bs
                                                      (list x id1)))))))
                              args)
                             (list args10
                                   (if
                                       (null bs)
                                       (cons 'progn body)
                                     (cons 'let
                                           (cons bs body))))))))))))
           (y-setenv 'bind* :symbol 'y--bind*))
         (progn
           (defalias 'y-macroexpand
             #'(lambda
                 (form)
                 (progn
                   (let*
                       ((s2
                         (y--symbol-expansion form)))
                     (progn
                       (progn
                         (if s2
                             (y-macroexpand s2)
                           (if
                               (atom form)
                               form
                             (let*
                                 ((x
                                   (y-macroexpand
                                    (y-hd form))))
                               (progn
                                 (if
                                     (eq x 'quote)
                                     form
                                   (if
                                       (eq x '\`)
                                       (y-macroexpand
                                        (funcall 'macroexpand form))
                                     (if
                                         (y--macro-p x)
                                         (y-macroexpand
                                          (apply
                                           (y--macro-function x)
                                           (y-tl form)))
                                       (cons x
                                             (mapcar 'y-macroexpand
                                                     (y-tl form))))))))))))))))
           (y-setenv 'macroexpand :symbol 'y-macroexpand))
         (progn
           (defalias 'y-eval
             #'(lambda
                 (form)
                 (progn
                   (funcall 'eval
                            (y-macroexpand form)
                            t))))
           (y-setenv 'eval :symbol 'y-eval))
         (progn
           (defalias 'y--expand-if
             #'(lambda
                 (id10)
                 (let*
                     ((var10 id10))
                   (progn
                     (let*
                         ((a
                           (y-get var10 '0)))
                       (let*
                           ((b
                             (y-get var10 '1)))
                         (let*
                             ((c
                               (y-cut var10 2)))
                           (progn
                             (if
                                 (y-some-p c)
                                 (list
                                  (cons '%if
                                        (cons a
                                              (cons b
                                                    (y--expand-if c)))))
                               (if
                                   (y-is-p b)
                                   (list
                                    (list '%if a b))
                                 (if
                                     (y-is-p a)
                                     (list a))))))))))))
           (y-setenv 'expand-if :symbol 'y--expand-if))
         (y-setenv 'if :macro
                   #'(lambda
                       (&rest branches)
                       (progn
                         (y-hd
                          (y--expand-if branches)))))
         (y-setenv 'with :macro
                   #'(lambda
                       (x v &rest body)
                       (progn
                         (cons 'let
                               (cons
                                (list x v)
                                (append body
                                        (list x)))))))
         (y-setenv 'let-when :macro
                   #'(lambda
                       (x v &rest body)
                       (progn
                         (let*
                             ((y
                               (y-unique 'y)))
                           (list 'let y v
                                 (list 'when y
                                       (cons 'let
                                             (cons
                                              (list x y)
                                              body))))))))
         (y-setenv 'fn :macro
                   #'(lambda
                       (args &rest body)
                       (progn
                         (cons 'lambda
                               (y--bind* args body)))))
         (y-setenv 'define-macro :macro
                   #'(lambda
                       (name args &rest body)
                       (progn
                         (let*
                             ((form
                               (list 'setenv
                                     (list 'quote name)
                                     ':macro
                                     (cons 'fn
                                           (cons args body)))))
                           (y-eval form)
                           form))))
         (y-setenv 'define-symbol :macro
                   #'(lambda
                       (name expansion)
                       (progn
                         (y-setenv name :symbol expansion)
                         (list 'setenv
                               (list 'quote name)
                               ':symbol
                               (list 'quote expansion)))))
         (y-setenv 'define :macro
                   #'(lambda
                       (name x &rest body)
                       (progn
                         (let*
                             ((var3
                               (y--global-id
                                (concat
                                 (y--module-name)
                                 "--")
                                name)))
                           (progn
                             (progn
                               (y-setenv name :symbol var3)
                               (y-setenv var3 :variable t)
                               (list 'progn
                                     (list 'defalias
                                           (list 'quote var3)
                                           (cons 'fn
                                                 (cons x body)))
                                     (list 'setenv
                                           (list 'quote name)
                                           ':symbol
                                           (list 'quote var3)))))))))
         (y-setenv 'define-global :macro
                   #'(lambda
                       (name x &rest body)
                       (progn
                         (let*
                             ((var5
                               (y--global-id
                                (concat
                                 (y--module-name)
                                 "-")
                                name)))
                           (progn
                             (progn
                               (y-setenv name :symbol var5)
                               (y-setenv var5 :variable t :toplevel t)
                               (list 'progn
                                     (list 'defalias
                                           (list 'quote var5)
                                           (cons 'fn
                                                 (cons x body)))
                                     (list 'setenv
                                           (list 'quote name)
                                           ':symbol
                                           (list 'quote var5)))))))))
         (y-setenv 'with-frame :macro
                   #'(lambda
                       (&rest body)
                       (progn
                         (let*
                             ((x
                               (y-unique 'x)))
                           (list 'progn
                                 '(set environment
                                       (apply 'vector
                                              (append environment
                                                      (list
                                                       (obj)))))
                                 (cons 'with
                                       (cons x
                                             (cons
                                              (cons 'progn body)
                                              '((set environment
                                                     (apply 'vector
                                                            (almost environment))))))))))))
         (y-setenv 'let-macro :macro
                   #'(lambda
                       (definitions &rest body)
                       (progn
                         (progn
                           (setq y-environment
                                 (apply 'vector
                                        (append y-environment
                                                (list
                                                 (make-hash-table :test 'eq)))))
                           (let*
                               ((x40
                                 (progn
                                   (let*
                                       ((x50 definitions))
                                     (progn
                                       (let*
                                           ((n14
                                             (y-length x50)))
                                         (progn
                                           (let*
                                               ((i12 0))
                                             (progn
                                               (while
                                                   (< i12 n14)
                                                 (let*
                                                     ((m
                                                       (y-get x50 i12)))
                                                   (progn
                                                     (y-macroexpand
                                                      (cons 'define-macro m))))
                                                 (setq i12
                                                       (+ i12 1)))))))))
                                   (cons 'progn
                                         (y-macroexpand body)))))
                             (progn
                               (progn
                                 (setq y-environment
                                       (apply 'vector
                                              (y-almost y-environment)))
                                 x40)))))))
         (y-setenv 'let-symbol :macro
                   #'(lambda
                       (expansions &rest body)
                       (progn
                         (if
                             (y-none-p expansions)
                             (cons 'progn
                                   (y-macroexpand body))
                           (progn
                             (setq y-environment
                                   (apply 'vector
                                          (append y-environment
                                                  (list
                                                   (make-hash-table :test 'eq)))))
                             (let*
                                 ((x80
                                   (progn
                                     (let*
                                         ((x90
                                           (y-pair expansions)))
                                       (progn
                                         (let*
                                             ((n16
                                               (y-length x90)))
                                           (progn
                                             (let*
                                                 ((i14 0))
                                               (progn
                                                 (while
                                                     (< i14 n16)
                                                   (let*
                                                       ((x
                                                         (y-get x90 i14)))
                                                     (progn
                                                       (y-macroexpand
                                                        (cons 'define-symbol x))))
                                                   (setq i14
                                                         (+ i14 1)))))))))
                                     (cons 'progn
                                           (y-macroexpand body)))))
                               (progn
                                 (progn
                                   (setq y-environment
                                         (apply 'vector
                                                (y-almost y-environment)))
                                   x80))))))))
         (y-setenv 'when-compiling :macro
                   #'(lambda
                       (&rest body)
                       (progn
                         (y-eval
                          (cons 'progn body)))))
         (y-setenv 'let :macro
                   #'(lambda
                       (bs &rest body)
                       (progn
                         (if
                             (and bs
                                  (atom bs))
                             (cons 'let
                                   (cons
                                    (list bs
                                          (y-hd body))
                                    (y-tl body)))
                           (if
                               (y-none-p bs)
                               (cons 'progn body)
                             (let*
                                 ((var80 bs))
                               (progn
                                 (let*
                                     ((lh
                                       (y-get var80 '0)))
                                   (let*
                                       ((rh
                                         (y-get var80 '1)))
                                     (let*
                                         ((bs2
                                           (y-cut var80 2)))
                                       (let*
                                           ((var9
                                             (y--bind lh rh)))
                                         (let*
                                             ((var
                                               (y-get var9 '0)))
                                           (let*
                                               ((val
                                                 (y-get var9 '1)))
                                             (let*
                                                 ((bs1
                                                   (y-cut var9 2)))
                                               (progn
                                                 (let*
                                                     ((renames nil))
                                                   (progn
                                                     (if
                                                         (or
                                                          (y--bound-p var)
                                                          (y-toplevel-p))
                                                         (let*
                                                             ((var1
                                                               (y-unique var)))
                                                           (progn
                                                             (setq renames
                                                                   (list var var1))
                                                             (setq var var1)))
                                                       (y-setenv var :variable t))
                                                     (let*
                                                         ((form
                                                           (cons 'let
                                                                 (cons
                                                                  (y-join bs1 bs2)
                                                                  body))))
                                                       (progn
                                                         (if
                                                             (y-none-p renames)
                                                             nil
                                                           (setq form
                                                                 (list 'let-symbol renames form)))
                                                         (list 'let*
                                                               (list
                                                                (list var val))
                                                               (y-macroexpand form)))))))))))))))))))))
         (y-setenv 'join! :macro
                   #'(lambda
                       (a &rest bs)
                       (progn
                         (list 'set a
                               (cons 'join
                                     (cons a bs))))))
         (y-setenv 'inc :macro
                   #'(lambda
                       (n &optional by)
                       (progn
                         (list 'set n
                               (list '+ n
                                     (or by 1))))))
         (y-setenv 'dec :macro
                   #'(lambda
                       (n &optional by)
                       (progn
                         (list 'set n
                               (list '- n
                                     (or by 1))))))
         (y-setenv 'for :macro
                   #'(lambda
                       (i to &rest body)
                       (progn
                         (list 'let i 0
                               (cons 'while
                                     (cons
                                      (list '< i to)
                                      (append body
                                              (list
                                               (list 'inc i)))))))))
         (y-setenv 'step :macro
                   #'(lambda
                       (v l &rest body)
                       (progn
                         (let*
                             ((x
                               (y-unique 'x))
                              (n
                               (y-unique 'n))
                              (i
                               (y-unique 'i)))
                           (list 'let
                                 (list x l n
                                       (list '\# x))
                                 (list 'for i n
                                       (cons 'let
                                             (cons
                                              (list v
                                                    (list 'at x i))
                                              body))))))))
         (y-setenv 'each :macro
                   #'(lambda
                       (x l &rest body)
                       (progn
                         (let*
                             ((o
                               (y-unique 'o))
                              (n
                               (y-unique 'n))
                              (f
                               (y-unique 'f))
                              (a
                               (y-unique 'a)))
                           (let*
                               ((var110
                                 (if
                                     (y-atom-p x)
                                     (list
                                      (y-unique 'i)
                                      x)
                                   (if
                                       (>
                                        (y-length x)
                                        1)
                                       x
                                     (list
                                      (y-unique 'i)
                                      (y-hd x))))))
                             (progn
                               (let*
                                   ((k
                                     (y-get var110 '0)))
                                 (let*
                                     ((v
                                       (y-get var110 '1)))
                                   (progn
                                     (list 'let
                                           (list o l f
                                                 (cons 'fn
                                                       (cons
                                                        (list k v)
                                                        body)))
                                           (list 'if
                                                 (list 'hash-table-p o)
                                                 (list 'maphash f o)
                                                 (list 'listp o)
                                                 (list 'y-%for o k a
                                                       (list 'funcall f k a))
                                                 (list 'let n
                                                       (list '\# o)
                                                       (list 'for k n
                                                             (list 'let a
                                                                   (list 'at o k)
                                                                   (list 'funcall f k a)))))))))))))))
         (progn
           (defalias 'y--eval-print
             #'(lambda
                 (form)
                 (progn
                   (condition-case err
                       (let*
                           ((x10
                             (y-eval form)))
                         (progn
                           (progn
                             (if
                                 (y-is-p x10)
                                 (y-print
                                  (format "%S" x10))))))
                     (error
                      (y-print
                       (format "error: %s"
                               (error-message-string err))))))))
           (y-setenv 'eval-print :symbol 'y--eval-print))
         (progn
           (defalias 'y--read-string
             #'(lambda
                 (s &optional more)
                 (progn
                   (if more
                       (condition-case nil
                           (car
                            (read-from-string s))
                         (end-of-file more))
                     (car
                      (read-from-string s))))))
           (y-setenv 'read-string :symbol 'y--read-string))
         (progn
           (defalias 'y--rep
             #'(lambda
                 (s)
                 (progn
                   (y--eval-print
                    (y--read-string s)))))
           (y-setenv 'rep :symbol 'y--rep))
         (progn
           (defalias 'y--repl
             #'(lambda nil
                 (progn
                   (let*
                       ((buf0 ""))
                     (progn
                       (progn
                         (let*
                             ((rep1
                               #'(lambda
                                   (s)
                                   (progn
                                     (setq buf0
                                           (concat buf0 s))
                                     (let*
                                         ((more
                                           (make-hash-table :test 'eq)))
                                       (let*
                                           ((form
                                             (y--read-string buf0 more)))
                                         (progn
                                           (if
                                               (eql form more)
                                               nil
                                             (y--eval-print form)
                                             (setq buf0 "")
                                             (princ "> ")))))))))
                           (progn
                             (princ "> ")
                             (catch 'y-break
                               (while t
                                 (let*
                                     ((s
                                       (read-from-minibuffer "")))
                                   (progn
                                     (if
                                         (and s
                                              (not
                                               (string= s ":a")))
                                         (funcall rep1
                                                  (concat s "\n"))
                                       (throw 'y-break nil))))))))))))))
           (y-setenv 'repl :symbol 'y--repl))
         (progn
           (defalias 'y-compile-file
             #'(lambda
                 (path &optional y--module-name)
                 (progn
                   (let*
                       ((name
                         (or y--module-name
                             (file-name-base path)))
                        (y-module name)
                        (forms
                         (let
                             ((temp-buffer
                               (generate-new-buffer " *temp*")))
                           (save-current-buffer
                             (set-buffer temp-buffer)
                             (unwind-protect
                                 (progn
                                   (insert-file-contents-literally path)
                                   (car
                                    (read-from-string
                                     (concat "("
                                             (buffer-string)
                                             ")"))))
                               (and
                                (buffer-name temp-buffer)
                                (kill-buffer temp-buffer))))))
                        (exprs
                         (funcall 'macroexpand-all
                                  (cons 'progn forms))))
                     (let
                         ((temp-buffer
                           (generate-new-buffer " *temp*")))
                       (save-current-buffer
                         (set-buffer temp-buffer)
                         (unwind-protect
                             (progn
                               (insert ";;; -*- lexical-binding: t -*-\n")
                               (insert
                                (let
                                    ((standard-output
                                      (get-buffer-create
                                       (generate-new-buffer-name " *string-output*"))))
                                  (unwind-protect
                                      (progn
                                        (let
                                            ((standard-output standard-output))
                                          (pp exprs))
                                        (save-current-buffer
                                          (set-buffer standard-output)
                                          (buffer-string)))
                                    (kill-buffer standard-output))))
                               (untabify
                                (point-min)
                                (point-max))
                               (buffer-string))
                           (and
                            (buffer-name temp-buffer)
                            (kill-buffer temp-buffer)))))))))
           (y-setenv 'compile-file :symbol 'y-compile-file))
         (progn
           (defalias 'y-write-file
             #'(lambda
                 (path data)
                 (progn
                   (let
                       ((temp-buffer
                         (generate-new-buffer " *temp*")))
                     (save-current-buffer
                       (set-buffer temp-buffer)
                       (unwind-protect
                           (progn
                             (insert data)
                             (write-region
                              (point-min)
                              (point-max)
                              path nil t))
                         (and
                          (buffer-name temp-buffer)
                          (kill-buffer temp-buffer))))))))
           (y-setenv 'write-file :symbol 'y-write-file))
         (progn
           (defalias 'y--run-file
             #'(lambda
                 (path)
                 (progn
                   (funcall 'load-file path))))
           (y-setenv 'run-file :symbol 'y--run-file))
         (progn
           (defalias 'y--usage
             #'(lambda nil
                 (progn
                   (y-print "usage: y [options] <object files>")
                   (y-print "options:")
                   (y-print "  -c <input>       Compile input file")
                   (y-print "  -o <output>      Output file")
                   (y-print "  -e <expr>        Expression to evaluate"))))
           (y-setenv 'usage :symbol 'y--usage))
         (progn
           (defalias 'y-main
             #'(lambda
                 (args)
                 (progn
                   (let*
                       ((arg0
                         (y-hd args)))
                     (progn
                       (progn
                         (if
                             (or
                              (string= arg0 "-h")
                              (string= arg0 "--help"))
                             (y--usage)
                           (let*
                               ((pre nil))
                             (let*
                                 ((input nil))
                               (let*
                                   ((output nil))
                                 (let*
                                     ((target1 nil))
                                   (let*
                                       ((expr nil))
                                     (let*
                                         ((n
                                           (y-length args)))
                                       (progn
                                         (let*
                                             ((i 0))
                                           (progn
                                             (while
                                                 (< i n)
                                               (let*
                                                   ((a
                                                     (y-get args i)))
                                                 (progn
                                                   (if
                                                       (or
                                                        (string= a "-c")
                                                        (string= a "-o")
                                                        (string= a "-e"))
                                                       (if
                                                           (eql i
                                                                (- n 1))
                                                           (y-print
                                                            (format "missing argument for %S" a))
                                                         (progn
                                                           (setq i
                                                                 (+ i 1))
                                                           (let*
                                                               ((val
                                                                 (y-get args i)))
                                                             (progn
                                                               (if
                                                                   (string= a "-c")
                                                                   (setq input val)
                                                                 (if
                                                                     (string= a "-o")
                                                                     (setq output val)
                                                                   (if
                                                                       (string= a "-e")
                                                                       (setq expr val))))))))
                                                     (progn
                                                       (let*
                                                           ((k16
                                                             (y-length pre))
                                                            (v15 a))
                                                         (setq pre
                                                               (y-put pre k16 v15))
                                                         v15)
                                                       nil))))
                                               (setq i
                                                     (+ i 1)))))
                                         (let*
                                             ((x11 pre))
                                           (let*
                                               ((n17
                                                 (y-length x11)))
                                             (progn
                                               (let*
                                                   ((i15 0))
                                                 (progn
                                                   (while
                                                       (< i15 n17)
                                                     (let*
                                                         ((file
                                                           (y-get x11 i15)))
                                                       (progn
                                                         (y--run-file file)))
                                                     (setq i15
                                                           (+ i15 1))))))))
                                         (if
                                             (y-nil-p input)
                                             (if expr
                                                 (y--rep expr)
                                               (y--repl))
                                           (let*
                                               ((code
                                                 (y-compile-file input)))
                                             (progn
                                               (if
                                                   (or
                                                    (y-nil-p output)
                                                    (string= output "-"))
                                                   (y-print code)
                                                 (y-write-file output code)))))))))))))))))))
           (y-setenv 'main :symbol 'y-main))
         (if noninteractive
             (progn
               (let*
                   ((args0 command-line-args-left))
                 (progn
                   (progn
                     (setq command-line-args-left nil)
                     (y-main args0)))))))
       (provide 'y))
