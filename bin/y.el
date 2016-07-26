;;; -*- lexical-binding: t -*-
(progn
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
                                                   (list 'car h)
                                                   (list 'incf i)))
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
              ((i4 -1))
              (while h
                (let*
                  ((var
                    (if
                      (keywordp
                       (car h))
                      (car h)
                      (setq i4
                            (1+ i4))))
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
                       (getter setter)
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
  (prog1
(defalias 'y-put
  #'(lambda
      (h k &optional v)
      (if
        (hash-table-p h)
        (progn
          (puthash k v h)
          h)
        (let*
          ((l h))
          (if
            (listp h)
            (catch 'y-break
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
                    ((i5 -1))
                    (while h
                      (let*
                        ((var
                          (if
                            (keywordp
                             (car h))
                            (car h)
                            (setq i5
                                  (1+ i5))))
                         (_val
                          (if
                            (keywordp
                             (car h))
                            (cadr h)
                            (car h))))
                        (if
                          (eq var k)
                          (progn
                            (if
                              (keywordp k)
                              (setcar
                               (cdr h)
                               v)
                              (setq h
                                    (setcar h v)))
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
                    nil))))
            (aset h k v))
          l))))
'byte-compile-inline-expand)
  (prog1
(defalias 'y-length
  #'(lambda
      (h &optional upto)
      (catch 'y-break
        (if
          (listp h)
          (let*
            ((n -1))
            (let*
              ((i6 -1))
              (while h
                (let*
                  ((k
                    (if
                      (keywordp
                       (car h))
                      (car h)
                      (setq i6
                            (1+ i6))))
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
'byte-compile-inline-expand)
  (defalias 'y-do
    (cons 'macro
          #'(lambda
              (&rest body)
              (let*
                ((y-environment
                  (y-apply 'vector
                           (append y-environment nil))))
                (cons 'progn
                      (mapcar 'y-macroexpand body))))))
  (progn
(defvar y-environment
  (list
    (make-hash-table :test 'eq)))
(progn
(defalias 'y-setenv
  #'(lambda
      (k &rest ks)
      (let*
        ((i
          (if
            (memq :toplevel ks)
            0
            (-
             (y-length y-environment)
             1)))
         (frame
          (y-get y-environment i))
         (entry
          (or
           (y-get frame k)
           (make-hash-table :test 'eq))))
        (let*
          ((o0 ks)
           (f0
            #'(lambda
                (k v)
                (let*
                  ((k0 k)
                   (v0 v))
                  (setq entry
                        (y-put entry k0 v0))
                  v0))))
          (if
            (hash-table-p o0)
            (maphash f0 o0)
            (if
              (listp o0)
              (let*
                ((i7 -1))
                (while o0
                  (let*
                    ((k
                      (if
                        (keywordp
                         (car o0))
                        (car o0)
                        (setq i7
                              (1+ i7))))
                     (v
                      (if
                        (keywordp
                         (car o0))
                        (cadr o0)
                        (car o0))))
                    (funcall f0 k v))
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
                (let*
                  ((k 0))
                  (while
                    (< k n0)
                    (let*
                      ((v
                        (y-get o0 k)))
                      (funcall f0 k v))
                    (setq k
                          (+ k 1))))))))
        (let*
          ((k1 k)
           (v1 entry))
          (setq frame
                (y-put frame k1 v1))
          v1))))
(y-setenv 'setenv :symbol 'y-setenv))
(progn
(defalias 'y-getenv
  #'(lambda
      (k &optional p)
      (let*
        ((i
          (-
           (y-length y-environment)
           1)))
        (catch 'y-break
          (while
            (>= i 0)
            (let*
              ((b
                (y-get
                 (y-get y-environment i)
                 k)))
              (if b
                  (throw 'y-break
                         (if p
                             (y-get b p)
                             b))
                  (setq i
                        (- i 1)))))))))
(y-setenv 'getenv :symbol 'y-getenv))
(y-setenv 'unique :symbol 'y-unique)
(y-setenv 'let-unique :symbol 'y-let-unique)
(y-setenv 'at :symbol 'get)
(y-setenv 'set :macro
          #'(lambda
              (&rest args)
              (cons 'y-set args)))
(y-setenv 'get :symbol 'y-get)
(y-setenv '\# :symbol 'y-length)
(y-setenv '= :symbol 'eql)
(y-setenv 'environment :symbol 'y-environment)
(progn
(defalias 'y-nil-p
  #'(lambda
      (x)
      (eql x nil)))
(y-setenv 'nil\? :symbol 'y-nil-p))
(progn
(defalias 'y-is-p
  #'(lambda
      (x)
      (not
       (y-nil-p x))))
(y-setenv 'is\? :symbol 'y-is-p))
(progn
(defalias 'y-none-p
  #'(lambda
      (x)
      (eql
       (y-length x 0)
       0)))
(y-setenv 'none\? :symbol 'y-none-p))
(progn
(defalias 'y-some-p
  #'(lambda
      (x)
      (>
       (y-length x 0)
       0)))
(y-setenv 'some\? :symbol 'y-some-p))
(progn
(defalias 'y-one-p
  #'(lambda
      (x)
      (eql
       (y-length x 1)
       1)))
(y-setenv 'one\? :symbol 'y-one-p))
(progn
(defalias 'y-two-p
  #'(lambda
      (x)
      (eql
       (y-length x 2)
       2)))
(y-setenv 'two\? :symbol 'y-two-p))
(progn
(defalias 'y-hd
  #'(lambda
      (l)
      (y-get l 0)))
(y-setenv 'hd :symbol 'y-hd))
(progn
(defalias 'y-type
  #'(lambda
      (x)
      (type-of x)))
(y-setenv 'type :symbol 'y-type))
(progn
(defalias 'y-string-p
  #'(lambda
      (x)
      (stringp x)))
(y-setenv 'string\? :symbol 'y-string-p))
(progn
(defalias 'y-number-p
  #'(lambda
      (x)
      (or
       (integerp x)
       (numberp x))))
(y-setenv 'number\? :symbol 'y-number-p))
(progn
(defalias 'y-function-p
  #'(lambda
      (x)
      (and
       (not
        (symbolp x))
       (functionp x))))
(y-setenv 'function\? :symbol 'y-function-p))
(progn
(defalias 'y-obj-p
  #'(lambda
      (x)
      (hash-table-p x)))
(y-setenv 'obj\? :symbol 'y-obj-p))
(y-setenv 'obj :macro
          #'(lambda nil
              '(make-hash-table :test 'eq)))
(progn
(defalias 'y-atom-p
  #'(lambda
      (x)
      (or
       (y-nil-p x)
       (symbolp x)
       (y-string-p x)
       (y-number-p x))))
(y-setenv 'atom\? :symbol 'y-atom-p))
(progn
(defalias 'y-clip
  #'(lambda
      (s from &optional upto)
      (let*
        ((n
          (length s))
         (i
          (if
            (or
             (y-nil-p from)
             (< from 0))
            0 from))
         (j
          (if
            (or
             (y-nil-p upto)
             (> upto n))
            n
            (max upto i))))
        (substring s i j))))
(y-setenv 'clip :symbol 'y-clip))
(progn
(defalias 'y--chop-p
  #'(lambda
      (x from upto)
      (and
       (consp x)
       (eql from 1)
       (null upto)
       (not
        (keywordp
         (car x))))))
(y-setenv 'chop\? :symbol 'y--chop-p))
(progn
(defalias 'y-cut
  #'(lambda
      (x &optional from upto)
      (if
        (y--chop-p x from upto)
        (cdr x)
        (let*
          ((l
            (if
              (y-obj-p x)
              (make-hash-table :test 'eq)
              nil)))
          (let*
            ((j 0)
             (i
              (if
                (or
                 (y-nil-p from)
                 (< from 0))
                0 from))
             (n
              (y-length x))
             (upto
              (if
                (or
                 (y-nil-p upto)
                 (> upto n))
                n upto)))
            (while
              (< i upto)
              (let*
                ((k2 j)
                 (v2
                  (y-get x i)))
                (setq l
                      (y-put l k2 v2))
                v2)
              (setq i
                    (+ i 1))
              (setq j
                    (+ j 1)))
            (let*
              ((o1 x)
               (f1
                #'(lambda
                    (k v)
                    (if
                      (y-number-p k)
                      nil
                      (let*
                        ((k3 k)
                         (v3 v))
                        (setq l
                              (y-put l k3 v3))
                        v3)))))
              (if
                (hash-table-p o1)
                (maphash f1 o1)
                (if
                  (listp o1)
                  (let*
                    ((i8 -1))
                    (while o1
                      (let*
                        ((k
                          (if
                            (keywordp
                             (car o1))
                            (car o1)
                            (setq i8
                                  (1+ i8))))
                         (v
                          (if
                            (keywordp
                             (car o1))
                            (cadr o1)
                            (car o1))))
                        (funcall f1 k v))
                      (setq o1
                            (if
                              (keywordp
                               (car o1))
                              (cddr o1)
                              (cdr o1))))
                    nil)
                  (let*
                    ((n1
                      (y-length o1)))
                    (let*
                      ((k 0))
                      (while
                        (< k n1)
                        (let*
                          ((v
                            (y-get o1 k)))
                          (funcall f1 k v))
                        (setq k
                              (+ k 1)))))))))
          l))))
(y-setenv 'cut :symbol 'y-cut))
(progn
(defalias 'y-keys
  #'(lambda
      (x)
      (let*
        ((l
          (if
            (y-obj-p x)
            (make-hash-table :test 'eq)
            nil)))
        (let*
          ((o2 x)
           (f2
            #'(lambda
                (k v)
                (if
                  (y-number-p k)
                  nil
                  (let*
                    ((k4 k)
                     (v4 v))
                    (setq l
                          (y-put l k4 v4))
                    v4)))))
          (if
            (hash-table-p o2)
            (maphash f2 o2)
            (if
              (listp o2)
              (let*
                ((i9 -1))
                (while o2
                  (let*
                    ((k
                      (if
                        (keywordp
                         (car o2))
                        (car o2)
                        (setq i9
                              (1+ i9))))
                     (v
                      (if
                        (keywordp
                         (car o2))
                        (cadr o2)
                        (car o2))))
                    (funcall f2 k v))
                  (setq o2
                        (if
                          (keywordp
                           (car o2))
                          (cddr o2)
                          (cdr o2))))
                nil)
              (let*
                ((n2
                  (y-length o2)))
                (let*
                  ((k 0))
                  (while
                    (< k n2)
                    (let*
                      ((v
                        (y-get o2 k)))
                      (funcall f2 k v))
                    (setq k
                          (+ k 1))))))))
        l)))
(y-setenv 'keys :symbol 'y-keys))
(progn
(defalias 'y-edge
  #'(lambda
      (x)
      (-
       (y-length x)
       1)))
(y-setenv 'edge :symbol 'y-edge))
(progn
(defalias 'y-tl
  #'(lambda
      (l)
      (y-cut l 1)))
(y-setenv 'tl :symbol 'y-tl))
(progn
(defalias 'y-last
  #'(lambda
      (l)
      (y-get l
             (y-edge l))))
(y-setenv 'last :symbol 'y-last))
(progn
(defalias 'y-almost
  #'(lambda
      (l)
      (y-cut l 0
             (y-edge l))))
(y-setenv 'almost :symbol 'y-almost))
(y-setenv 'add :macro
          #'(lambda
              (l x)
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
                     '(nil)))))
(y-setenv 'drop :macro
          #'(lambda
              (l)
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
                          (list 'almost l)))))
(progn
(defalias 'y-reverse
  #'(lambda
      (l)
      (let*
        ((l1
          (y-keys l)))
        (let*
          ((i
            (y-edge l)))
          (while
            (>= i 0)
            (progn
              (let*
                ((k5
                  (y-length l1))
                 (v5
                  (y-get l i)))
                (setq l1
                      (y-put l1 k5 v5))
                v5)
              nil)
            (setq i
                  (- i 1))))
        l1)))
(y-setenv 'reverse :symbol 'y-reverse))
(progn
(defalias 'y-reduce
  #'(lambda
      (f x)
      (if
        (y-none-p x)
        nil
        (y-one-p x)
        (y-hd x)
        (funcall f
                 (y-hd x)
                 (y-reduce f
                           (y-tl x))))))
(y-setenv 'reduce :symbol 'y-reduce))
(progn
(defalias 'y-join
  #'(lambda
      (&rest ls)
      (if
        (y-two-p ls)
        (let*
          ((a
            (y-get ls 0))
           (b
            (y-get ls 1)))
          (if
            (and a b)
            (let*
              ((c
                (if
                  (y-obj-p a)
                  (make-hash-table :test 'eq)
                  nil))
               (o
                (y-length a)))
              (let*
                ((o3 a)
                 (f3
                  #'(lambda
                      (k v)
                      (let*
                        ((k6 k)
                         (v6 v))
                        (setq c
                              (y-put c k6 v6))
                        v6))))
                (if
                  (hash-table-p o3)
                  (maphash f3 o3)
                  (if
                    (listp o3)
                    (let*
                      ((i10 -1))
                      (while o3
                        (let*
                          ((k
                            (if
                              (keywordp
                               (car o3))
                              (car o3)
                              (setq i10
                                    (1+ i10))))
                           (v
                            (if
                              (keywordp
                               (car o3))
                              (cadr o3)
                              (car o3))))
                          (funcall f3 k v))
                        (setq o3
                              (if
                                (keywordp
                                 (car o3))
                                (cddr o3)
                                (cdr o3))))
                      nil)
                    (let*
                      ((n3
                        (y-length o3)))
                      (let*
                        ((k 0))
                        (while
                          (< k n3)
                          (let*
                            ((v
                              (y-get o3 k)))
                            (funcall f3 k v))
                          (setq k
                                (+ k 1))))))))
              (let*
                ((o4 b)
                 (f4
                  #'(lambda
                      (k v)
                      (if
                        (y-number-p k)
                        (progn
                          (setq k
                                (+ k o))))
                      (let*
                        ((k7 k)
                         (v7 v))
                        (setq c
                              (y-put c k7 v7))
                        v7))))
                (if
                  (hash-table-p o4)
                  (maphash f4 o4)
                  (if
                    (listp o4)
                    (let*
                      ((i11 -1))
                      (while o4
                        (let*
                          ((k
                            (if
                              (keywordp
                               (car o4))
                              (car o4)
                              (setq i11
                                    (1+ i11))))
                           (v
                            (if
                              (keywordp
                               (car o4))
                              (cadr o4)
                              (car o4))))
                          (funcall f4 k v))
                        (setq o4
                              (if
                                (keywordp
                                 (car o4))
                                (cddr o4)
                                (cdr o4))))
                      nil)
                    (let*
                      ((n4
                        (y-length o4)))
                      (let*
                        ((k 0))
                        (while
                          (< k n4)
                          (let*
                            ((v
                              (y-get o4 k)))
                            (funcall f4 k v))
                          (setq k
                                (+ k 1))))))))
              c)
            (or a b nil)))
        (or
         (y-reduce 'y-join ls)
         nil))))
(y-setenv 'join :symbol 'y-join))
(progn
(defalias 'y-find
  #'(lambda
      (f l)
      (catch 'y-break
        (let*
          ((o5 l)
           (f5
            #'(lambda
                (i0 x)
                (let*
                  ((y
                    (funcall f x)))
                  (if y
                      (throw 'y-break y))))))
          (if
            (hash-table-p o5)
            (maphash f5 o5)
            (if
              (listp o5)
              (let*
                ((i12 -1))
                (while o5
                  (let*
                    ((i0
                      (if
                        (keywordp
                         (car o5))
                        (car o5)
                        (setq i12
                              (1+ i12))))
                     (x
                      (if
                        (keywordp
                         (car o5))
                        (cadr o5)
                        (car o5))))
                    (funcall f5 i0 x))
                  (setq o5
                        (if
                          (keywordp
                           (car o5))
                          (cddr o5)
                          (cdr o5))))
                nil)
              (let*
                ((n5
                  (y-length o5)))
                (let*
                  ((i0 0))
                  (while
                    (< i0 n5)
                    (let*
                      ((x
                        (y-get o5 i0)))
                      (funcall f5 i0 x))
                    (setq i0
                          (+ i0 1)))))))))))
(y-setenv 'find :symbol 'y-find))
(progn
(defalias 'y-first
  #'(lambda
      (f l)
      (catch 'y-break
        (let*
          ((x0 l)
           (n6
            (y-length x0)))
          (let*
            ((i1 0))
            (while
              (< i1 n6)
              (let*
                ((x
                  (y-get x0 i1)))
                (let*
                  ((y
                    (funcall f x)))
                  (if y
                      (throw 'y-break y))))
              (setq i1
                    (+ i1 1))))))))
(y-setenv 'first :symbol 'y-first))
(progn
(defalias 'y-in-p
  #'(lambda
      (x l)
      (y-find
       #'(lambda
           (y)
           (eql x y))
       l)))
(y-setenv 'in\? :symbol 'y-in-p))
(progn
(defalias 'y-pair
  #'(lambda
      (l)
      (let*
        ((l1
          (if
            (y-obj-p l)
            (make-hash-table :test 'eq)
            nil)))
        (let*
          ((n
            (y-length l)))
          (let*
            ((i 0))
            (while
              (< i n)
              (progn
                (let*
                  ((k8
                    (y-length l1))
                   (v8
                    (list
                      (y-get l i)
                      (y-get l
                             (+ i 1)))))
                  (setq l1
                        (y-put l1 k8 v8))
                  v8)
                nil)
              (setq i
                    (+ i 1))
              (setq i
                    (+ i 1)))))
        l1)))
(y-setenv 'pair :symbol 'y-pair))
(progn
(defalias 'y-map
  #'(lambda
      (f x)
      (let*
        ((l
          (if
            (y-obj-p x)
            (make-hash-table :test 'eq)
            nil)))
        (let*
          ((x1 x)
           (n7
            (y-length x1)))
          (let*
            ((i2 0))
            (while
              (< i2 n7)
              (let*
                ((v
                  (y-get x1 i2)))
                (let*
                  ((y
                    (funcall f v)))
                  (if
                    (y-is-p y)
                    (progn
                      (let*
                        ((k9
                          (y-length l))
                         (v9 y))
                        (setq l
                              (y-put l k9 v9))
                        v9)
                      nil))))
              (setq i2
                    (+ i2 1)))))
        (let*
          ((o6 x)
           (f6
            #'(lambda
                (k v)
                (if
                  (y-number-p k)
                  nil
                  (let*
                    ((y
                      (funcall f v)))
                    (if
                      (y-is-p y)
                      (progn
                        (let*
                          ((k10 k)
                           (v10 y))
                          (setq l
                                (y-put l k10 v10))
                          v10))))))))
          (if
            (hash-table-p o6)
            (maphash f6 o6)
            (if
              (listp o6)
              (let*
                ((i13 -1))
                (while o6
                  (let*
                    ((k
                      (if
                        (keywordp
                         (car o6))
                        (car o6)
                        (setq i13
                              (1+ i13))))
                     (v
                      (if
                        (keywordp
                         (car o6))
                        (cadr o6)
                        (car o6))))
                    (funcall f6 k v))
                  (setq o6
                        (if
                          (keywordp
                           (car o6))
                          (cddr o6)
                          (cdr o6))))
                nil)
              (let*
                ((n8
                  (y-length o6)))
                (let*
                  ((k 0))
                  (while
                    (< k n8)
                    (let*
                      ((v
                        (y-get o6 k)))
                      (funcall f6 k v))
                    (setq k
                          (+ k 1))))))))
        l)))
(y-setenv 'map :symbol 'y-map))
(progn
(defalias 'y-keep
  #'(lambda
      (f x)
      (y-map
       #'(lambda
           (v)
           (if
             (funcall f v)
             (progn v)))
       x)))
(y-setenv 'keep :symbol 'y-keep))
(progn
(defalias 'y-keys-p
  #'(lambda
      (l)
      (catch 'y-break
        (let*
          ((o7 l)
           (f7
            #'(lambda
                (k v)
                (setq v v)
                (if
                  (y-number-p k)
                  nil
                  (throw 'y-break t)))))
          (if
            (hash-table-p o7)
            (maphash f7 o7)
            (if
              (listp o7)
              (let*
                ((i14 -1))
                (while o7
                  (let*
                    ((k
                      (if
                        (keywordp
                         (car o7))
                        (car o7)
                        (setq i14
                              (1+ i14))))
                     (v
                      (if
                        (keywordp
                         (car o7))
                        (cadr o7)
                        (car o7))))
                    (funcall f7 k v))
                  (setq o7
                        (if
                          (keywordp
                           (car o7))
                          (cddr o7)
                          (cdr o7))))
                nil)
              (let*
                ((n9
                  (y-length o7)))
                (let*
                  ((k 0))
                  (while
                    (< k n9)
                    (let*
                      ((v
                        (y-get o7 k)))
                      (funcall f7 k v))
                    (setq k
                          (+ k 1))))))))
        nil)))
(y-setenv 'keys\? :symbol 'y-keys-p))
(progn
(defalias 'y-empty-p
  #'(lambda
      (l)
      (catch 'y-break
        (let*
          ((o8 l)
           (f8
            #'(lambda
                (i3 x)
                (setq x x)
                (throw 'y-break nil))))
          (if
            (hash-table-p o8)
            (maphash f8 o8)
            (if
              (listp o8)
              (let*
                ((i15 -1))
                (while o8
                  (let*
                    ((i3
                      (if
                        (keywordp
                         (car o8))
                        (car o8)
                        (setq i15
                              (1+ i15))))
                     (x
                      (if
                        (keywordp
                         (car o8))
                        (cadr o8)
                        (car o8))))
                    (funcall f8 i3 x))
                  (setq o8
                        (if
                          (keywordp
                           (car o8))
                          (cddr o8)
                          (cdr o8))))
                nil)
              (let*
                ((n10
                  (y-length o8)))
                (let*
                  ((i3 0))
                  (while
                    (< i3 n10)
                    (let*
                      ((x
                        (y-get o8 i3)))
                      (funcall f8 i3 x))
                    (setq i3
                          (+ i3 1))))))))
        t)))
(y-setenv 'empty\? :symbol 'y-empty-p))
(progn
(defalias 'y-stash
  #'(lambda
      (args)
      (let*
        ((l nil))
        (let*
          ((o9 args)
           (f9
            #'(lambda
                (k v)
                (if
                  (y-number-p k)
                  (progn
                    (progn
                      (let*
                        ((k11
                          (y-length l))
                         (v11 v))
                        (setq l
                              (y-put l k11 v11))
                        v11)
                      nil))))))
          (if
            (hash-table-p o9)
            (maphash f9 o9)
            (if
              (listp o9)
              (let*
                ((i16 -1))
                (while o9
                  (let*
                    ((k
                      (if
                        (keywordp
                         (car o9))
                        (car o9)
                        (setq i16
                              (1+ i16))))
                     (v
                      (if
                        (keywordp
                         (car o9))
                        (cadr o9)
                        (car o9))))
                    (funcall f9 k v))
                  (setq o9
                        (if
                          (keywordp
                           (car o9))
                          (cddr o9)
                          (cdr o9))))
                nil)
              (let*
                ((n11
                  (y-length o9)))
                (let*
                  ((k 0))
                  (while
                    (< k n11)
                    (let*
                      ((v
                        (y-get o9 k)))
                      (funcall f9 k v))
                    (setq k
                          (+ k 1))))))))
        (if
          (y-keys-p args)
          (progn
            (let*
              ((p
                (if
                  (y-obj-p args)
                  (make-hash-table :test 'eq)
                  nil)))
              (let*
                ((o10 args)
                 (f10
                  #'(lambda
                      (k v)
                      (if
                        (y-number-p k)
                        nil
                        (let*
                          ((k12 k)
                           (v12 v))
                          (setq p
                                (y-put p k12 v12))
                          v12)))))
                (if
                  (hash-table-p o10)
                  (maphash f10 o10)
                  (if
                    (listp o10)
                    (let*
                      ((i17 -1))
                      (while o10
                        (let*
                          ((k
                            (if
                              (keywordp
                               (car o10))
                              (car o10)
                              (setq i17
                                    (1+ i17))))
                           (v
                            (if
                              (keywordp
                               (car o10))
                              (cadr o10)
                              (car o10))))
                          (funcall f10 k v))
                        (setq o10
                              (if
                                (keywordp
                                 (car o10))
                                (cddr o10)
                                (cdr o10))))
                      nil)
                    (let*
                      ((n12
                        (y-length o10)))
                      (let*
                        ((k 0))
                        (while
                          (< k n12)
                          (let*
                            ((v
                              (y-get o10 k)))
                            (funcall f10 k v))
                          (setq k
                                (+ k 1))))))))
              (let*
                ((k13 :_stash)
                 (v13 t))
                (setq p
                      (y-put p k13 v13))
                v13)
              (progn
                (let*
                  ((k14
                    (y-length l))
                   (v14 p))
                  (setq l
                        (y-put l k14 v14))
                  v14)
                nil))))
        l)))
(y-setenv 'stash :symbol 'y-stash))
(progn
(defalias 'y-unstash
  #'(lambda
      (args)
      (if
        (y-none-p args)
        nil
        (let*
          ((l
            (y-last args)))
          (if
            (y-get l :_stash)
            (let*
              ((args1
                (y-almost args)))
              (let*
                ((o11 l)
                 (f11
                  #'(lambda
                      (k v)
                      (if
                        (eql k :_stash)
                        nil
                        (let*
                          ((k15 k)
                           (v15 v))
                          (setq args1
                                (y-put args1 k15 v15))
                          v15)))))
                (if
                  (hash-table-p o11)
                  (maphash f11 o11)
                  (if
                    (listp o11)
                    (let*
                      ((i18 -1))
                      (while o11
                        (let*
                          ((k
                            (if
                              (keywordp
                               (car o11))
                              (car o11)
                              (setq i18
                                    (1+ i18))))
                           (v
                            (if
                              (keywordp
                               (car o11))
                              (cadr o11)
                              (car o11))))
                          (funcall f11 k v))
                        (setq o11
                              (if
                                (keywordp
                                 (car o11))
                                (cddr o11)
                                (cdr o11))))
                      nil)
                    (let*
                      ((n13
                        (y-length o11)))
                      (let*
                        ((k 0))
                        (while
                          (< k n13)
                          (let*
                            ((v
                              (y-get o11 k)))
                            (funcall f11 k v))
                          (setq k
                                (+ k 1))))))))
              args1)
            args)))))
(y-setenv 'unstash :symbol 'y-unstash))
(progn
(defalias 'y-destash!
  #'(lambda
      (l args1)
      (if
        (and
         (or
          (listp l)
          (y-obj-p l))
         (y-get l :_stash))
        (let*
          ((o12 l)
           (f12
            #'(lambda
                (k v)
                (if
                  (eql k :_stash)
                  nil
                  (let*
                    ((k16 k)
                     (v16 v))
                    (setq args1
                          (y-put args1 k16 v16))
                    v16)))))
          (if
            (hash-table-p o12)
            (maphash f12 o12)
            (if
              (listp o12)
              (let*
                ((i19 -1))
                (while o12
                  (let*
                    ((k
                      (if
                        (keywordp
                         (car o12))
                        (car o12)
                        (setq i19
                              (1+ i19))))
                     (v
                      (if
                        (keywordp
                         (car o12))
                        (cadr o12)
                        (car o12))))
                    (funcall f12 k v))
                  (setq o12
                        (if
                          (keywordp
                           (car o12))
                          (cddr o12)
                          (cdr o12))))
                nil)
              (let*
                ((n14
                  (y-length o12)))
                (let*
                  ((k 0))
                  (while
                    (< k n14)
                    (let*
                      ((v
                        (y-get o12 k)))
                      (funcall f12 k v))
                    (setq k
                          (+ k 1))))))))
        l)))
(y-setenv 'destash! :symbol 'y-destash!))
(progn
(defalias 'y-apply
  #'(lambda
      (f args)
      (let*
        ((args1
          (y-stash args)))
        (funcall 'apply f args1))))
(y-setenv 'apply :symbol 'y-apply))
(progn
(defalias 'y-toplevel-p
  #'(lambda nil
      (y-one-p y-environment)))
(y-setenv 'toplevel\? :symbol 'y-toplevel-p))
(progn
(defalias 'y--id
  #'(lambda
      (x)
      (let*
        ((s
          (append
           (if
             (symbolp x)
             (symbol-name x)
             x)
           nil)))
        (if
          (eq 63
              (y-get s
                     (y-edge s)))
          (progn
            (if
              (memq 45 s)
              (progn
                (let*
                  ((k17
                    (y-edge s))
                   (v17 45))
                  (setq s
                        (y-put s k17 v17))
                  v17)
                (let*
                  ((k18
                    (y-length s))
                   (v18 112))
                  (setq s
                        (y-put s k18 v18))
                  v18))
              (let*
                ((k19
                  (y-edge s))
                 (v19 112))
                (setq s
                      (y-put s k19 v19))
                v19))))
        (intern
         (concat s)))))
(y-setenv 'id :symbol 'y--id))
(defvar y-module nil)
(progn
(defalias 'y--module-name
  #'(lambda nil
      (or y-module
          (let*
            ((file
              (or load-file-name
                  (buffer-file-name))))
            (if file
                (file-name-base file)
                (buffer-name))))))
(y-setenv 'module-name :symbol 'y--module-name))
(progn
(defalias 'y--global-id
  #'(lambda
      (prefix name)
      (let*
        ((s
          (if
            (stringp name)
            name
            (symbol-name name))))
        (if
          (eq 0
              (string-match
               (regexp-quote prefix)
               s))
          name
          (y--id
           (concat prefix s))))))
(y-setenv 'global-id :symbol 'y--global-id))
(progn
(defalias 'y--macro-function
  #'(lambda
      (k)
      (y-getenv k :macro)))
(y-setenv 'macro-function :symbol 'y--macro-function))
(progn
(defalias 'y--macro-p
  #'(lambda
      (k)
      (y--macro-function k)))
(y-setenv 'macro\? :symbol 'y--macro-p))
(progn
(defalias 'y--symbol-expansion
  #'(lambda
      (k)
      (y-getenv k :symbol)))
(y-setenv 'symbol-expansion :symbol 'y--symbol-expansion))
(progn
(defalias 'y--symbol-p
  #'(lambda
      (k)
      (let*
        ((v
          (y--symbol-expansion k)))
        (and v
             (not
              (eq v k))))))
(y-setenv 'symbol\? :symbol 'y--symbol-p))
(progn
(defalias 'y--variable-p
  #'(lambda
      (k)
      (let*
        ((i
          (-
           (y-length y-environment)
           1)))
        (catch 'y-break
          (while
            (>= i 0)
            (let*
              ((b
                (y-get
                 (y-get y-environment i)
                 k)))
              (if b
                  (throw 'y-break
                         (and b
                              (y-get b :variable)))
                  (setq i
                        (1- i)))))))))
(y-setenv 'variable\? :symbol 'y--variable-p))
(progn
(defalias 'y--bound-p
  #'(lambda
      (x)
      (or
       (y--macro-p x)
       (y--symbol-p x)
       (y--variable-p x))))
(y-setenv 'bound\? :symbol 'y--bound-p))
(progn
(defalias 'y--unkeywordify
  #'(lambda
      (k)
      (if
        (keywordp k)
        (intern
         (y-clip
          (symbol-name k)
          1))
        k)))
(y-setenv 'unkeywordify :symbol 'y--unkeywordify))
(progn
(defalias 'y--bind
  #'(lambda
      (lh rh)
      (if
        (y-atom-p lh)
        (list lh rh)
        (let*
          ((var
            (y-unique 'var)))
          (let*
            ((bs
              (list var rh)))
            (let*
              ((o13 lh)
               (f13
                #'(lambda
                    (k v)
                    (let*
                      ((x
                        (if
                          (eql k :rest)
                          (list 'cut var
                                (y-length lh))
                          (list 'get var
                                (list 'quote k)))))
                      (if
                        (y-is-p k)
                        (progn
                          (let*
                            ((k
                              (if
                                (eql v t)
                                (y--unkeywordify k)
                                v)))
                            (setq bs
                                  (y-join bs
                                          (y--bind k x))))))))))
              (if
                (hash-table-p o13)
                (maphash f13 o13)
                (if
                  (listp o13)
                  (let*
                    ((i20 -1))
                    (while o13
                      (let*
                        ((k
                          (if
                            (keywordp
                             (car o13))
                            (car o13)
                            (setq i20
                                  (1+ i20))))
                         (v
                          (if
                            (keywordp
                             (car o13))
                            (cadr o13)
                            (car o13))))
                        (funcall f13 k v))
                      (setq o13
                            (if
                              (keywordp
                               (car o13))
                              (cddr o13)
                              (cdr o13))))
                    nil)
                  (let*
                    ((n15
                      (y-length o13)))
                    (let*
                      ((k 0))
                      (while
                        (< k n15)
                        (let*
                          ((v
                            (y-get o13 k)))
                          (funcall f13 k v))
                        (setq k
                              (+ k 1))))))))
            bs)))))
(y-setenv 'bind :symbol 'y--bind))
(progn
(defalias 'y-macroexpand
  #'(lambda
      (form)
      (let*
        ((s
          (y--symbol-expansion form)))
        (if s
            (y-macroexpand s)
            (if
              (atom form)
              form
              (let*
                ((x
                  (y-macroexpand
                   (y-hd form))))
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
                       (funcall 'apply
                                (y--macro-function x)
                                (y-tl form)))
                      (cons x
                            (mapcar 'y-macroexpand
                                    (y-tl form))))))))))))
(y-setenv 'macroexpand :symbol 'y-macroexpand))
(progn
(defalias 'y-expand
  #'(lambda
      (form)
      (macroexpand-all
       (y-macroexpand form))))
(y-setenv 'expand :symbol 'y-expand))
(progn
(defalias 'y-eval
  #'(lambda
      (form)
      (funcall 'eval
               (y-macroexpand form)
               t)))
(y-setenv 'eval :symbol 'y-eval))
(y-setenv 'with :macro
          #'(lambda
              (x v &rest body)
              (cons 'let*
                    (cons
                     (list
                       (list x v))
                     (append body
                             (list x))))))
(y-setenv 'fn :macro
          #'(lambda
              (args &rest body)
              (cons 'lambda
                    (cons
                     (if
                       (not
                        (listp args))
                       (list '&rest args)
                       args)
                     body))))
(y-setenv 'define-macro :macro
          #'(lambda
              (name args &rest body)
              (let*
                ((form
                   (list 'setenv
                         (list 'quote name)
                         ':macro
                         (cons 'fn
                               (cons args body)))))
                (y-eval form)
                form)))
(y-setenv 'define-symbol :macro
          #'(lambda
              (name expansion)
              (y-setenv name :symbol expansion)
              (list 'setenv
                    (list 'quote name)
                    ':symbol
                    (list 'quote expansion))))
(y-setenv 'define :macro
          #'(lambda
              (name x &rest body)
              (let*
                ((var
                  (y--global-id
                   (concat
                    (y--module-name)
                    "--")
                   name)))
                (y-setenv name :symbol var)
                (y-setenv var :variable t)
                (list 'progn
                      (list 'defalias
                            (list 'quote var)
                            (cons 'fn
                                  (cons x body)))
                      (list 'setenv
                            (list 'quote name)
                            ':symbol
                            (list 'quote var))))))
(y-setenv 'define-global :macro
          #'(lambda
              (name x &rest body)
              (let*
                ((var
                  (y--global-id
                   (concat
                    (y--module-name)
                    "-")
                   name)))
                (y-setenv name :symbol var)
                (y-setenv var :variable t :toplevel t)
                (list 'progn
                      (list 'defalias
                            (list 'quote var)
                            (cons 'fn
                                  (cons x body)))
                      (list 'setenv
                            (list 'quote name)
                            ':symbol
                            (list 'quote var))))))
(y-setenv 'with-frame :macro
          #'(lambda
              (&rest body)
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
                                                 (almost environment)))))))))))
(y-setenv 'let-macro :macro
          #'(lambda
(definitions &rest body)
(progn
  (setq y-environment
        (y-apply 'vector
                 (append y-environment
                         (list
                           (make-hash-table :test 'eq)))))
  (let*
    ((x3
      (progn
        (y-map
         #'(lambda
             (m)
             (y-macroexpand
              (cons 'define-macro m)))
         definitions)
        (cons 'progn
              (y-macroexpand body)))))
    (setq y-environment
          (y-apply 'vector
                   (y-almost y-environment)))
    x3))))
(y-setenv 'let-symbol :macro
          #'(lambda
              (expansions &rest body)
              (if
                (y-none-p expansions)
                (cons 'progn
                      (y-macroexpand body))
                (progn
                  (setq y-environment
                        (y-apply 'vector
                                 (append y-environment
                                         (list
                                           (make-hash-table :test 'eq)))))
                  (let*
                    ((x5
                      (progn
                        (mapc
                         #'(lambda
                             (x)
                             (y-macroexpand
                              (cons 'define-symbol x)))
                         (y-pair expansions))
                        (cons 'progn
                              (y-macroexpand body)))))
                    (setq y-environment
                          (y-apply 'vector
                                   (y-almost y-environment)))
                    x5)))))
(y-setenv 'when-compiling :macro
          #'(lambda
              (&rest body)
              (y-eval
               (cons 'progn body))))
(y-setenv 'let :macro
          #'(lambda
              (bs &rest body)
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
                    ((var20 bs))
                    (progn
                      (let*
                        ((lh
                          (y-get var20 '0)))
                        (let*
                          ((rh
                            (y-get var20 '1)))
                          (let*
                            ((bs2
                              (y-cut var20 2)))
                            (let*
                              ((var3
                                (y--bind lh rh)))
                              (let*
                                ((var
                                  (y-get var3 '0)))
                                (let*
                                  ((val
                                    (y-get var3 '1)))
                                  (let*
                                    ((bs1
                                      (y-cut var3 2)))
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
                                                    (y-macroexpand form))))))))))))))))))))
(y-setenv 'join! :macro
          #'(lambda
              (a &rest bs)
              (list 'set a
                    (cons 'join
                          (cons a bs)))))
(y-setenv 'inc :macro
          #'(lambda
              (n &optional by)
              (list 'set n
                    (list '+ n
                          (or by 1)))))
(y-setenv 'dec :macro
          #'(lambda
              (n &optional by)
              (list 'set n
                    (list '- n
                          (or by 1)))))
(y-setenv 'for :macro
          #'(lambda
              (i to &rest body)
              (list 'let*
                    (list
                      (cons i
                            '(0)))
                    (cons 'while
                          (cons
                           (list '< i to)
                           (append body
                                   (list
                                     (list 'inc i))))))))
(y-setenv 'step :macro
          #'(lambda
              (v l &rest body)
              (let*
                ((x
                  (y-unique 'x))
                 (n
                  (y-unique 'n))
                 (i
                  (y-unique 'i)))
                (list 'let*
                      (list
                        (list x l)
                        (list n
                              (list '\# x)))
                      (list 'for i n
                            (cons 'let*
                                  (cons
                                   (list
                                     (list v
                                           (list 'at x i)))
                                   body)))))))
(y-setenv 'each :macro
          #'(lambda
              (x l &rest body)
              (let*
                ((o
                  (y-unique 'o))
                 (n
                  (y-unique 'n))
                 (f
                  (y-unique 'f)))
                (progn
                  (let*
                    ((--cl-rest--
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
                            (y-hd x)))))
                     (k
                      (if
                        (=
                         (length --cl-rest--)
                         2)
                        (car-safe
                         (prog1 --cl-rest--
                           (setq --cl-rest--
                                 (cdr --cl-rest--))))
                        (signal 'wrong-number-of-arguments
                                (list nil
                                      (length --cl-rest--)))))
                     (v
                      (car --cl-rest--)))
                    (list 'let*
                          (list
                            (list o l)
                            (list f
                                  (cons 'lambda
                                        (cons
                                         (list k v)
                                         body))))
                          (list 'if
                                (list 'hash-table-p o)
                                (list 'maphash f o)
                                (list 'if
                                      (list 'listp o)
                                      (list 'y-%for o k v
                                            (list 'funcall f k v))
                                      (list 'let*
                                            (list
                                              (list n
                                                    (list '\# o)))
                                            (list 'for k n
                                                  (list 'let*
                                                        (list
                                                          (list v
                                                                (list 'at o k)))
                                                        (list 'funcall f k v)))))))))))))
  (provide 'y))
