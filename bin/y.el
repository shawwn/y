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
                ((i4
                  (y-unique 'i4)))
                (cons 'let*
                      (cons
                       (list
                         (cons i4
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
                                                   (list 'incf i4)))
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
              ((i43 -1))
              (while h
                (let*
                  ((var
                    (if
                      (keywordp
                       (car h))
                      (car h)
                      (setq i43
                            (1+ i43))))
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
                    ((i44 -1))
                    (while h
                      (let*
                        ((var
                          (if
                            (keywordp
                             (car h))
                            (car h)
                            (setq i44
                                  (1+ i44))))
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
              ((i45 -1))
              (while h
                (let*
                  ((k
                    (if
                      (keywordp
                       (car h))
                      (car h)
                      (setq i45
                            (1+ i45))))
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
        ((i0
          (if
            (memq :toplevel ks)
            0
            (-
             (y-length y-environment)
             1))))
        (progn
          (let*
            ((frame
              (y-get y-environment i0)))
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
                          (k a0)
                          (let*
                            ((v0 a0))
                            (progn
                              (progn
                                (let*
                                  ((k15 k)
                                   (v16 v0))
                                  (setq entry
                                        (y-put entry k15 v16))
                                  v16)))))))
                    (progn
                      (if
                        (hash-table-p o0)
                        (maphash f0 o0)
                        (if
                          (listp o0)
                          (let*
                            ((i46 -1))
                            (while o0
                              (let*
                                ((k
                                  (if
                                    (keywordp
                                     (car o0))
                                    (car o0)
                                    (setq i46
                                          (1+ i46))))
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
                                ((k0 0))
                                (progn
                                  (progn
                                    (while
                                      (< k0 n0)
                                      (let*
                                        ((a0
                                          (y-get o0 k0)))
                                        (progn
                                          (funcall f0 k0 a0)))
                                      (setq k0
                                            (+ k0 1)))))))))))))
                (let*
                  ((k16 k)
                   (v17 entry))
                  (setq frame
                        (y-put frame k16 v17))
                  v17))))))))
(y-setenv 'setenv :symbol 'y-setenv))
(progn
(defalias 'y-getenv
  #'(lambda
      (k &optional p)
      (let*
        ((i1
          (-
           (y-length y-environment)
           1)))
        (progn
          (progn
            (catch 'y-break
              (while
                (>= i1 0)
                (let*
                  ((b0
                    (y-get
                     (y-get y-environment i1)
                     k)))
                  (progn
                    (progn
                      (if b0
                          (throw 'y-break
                                 (if p
                                     (y-get b0 p)
                                     b0))
                          (setq i1
                                (- i1 1)))))))))))))
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
        ((n1
          (length s)))
        (progn
          (let*
            ((i2
              (if
                (or
                 (y-nil-p from)
                 (< from 0))
                0 from)))
            (progn
              (let*
                ((j
                  (if
                    (or
                     (y-nil-p upto)
                     (> upto n1))
                    n1
                    (max upto i2))))
                (progn
                  (substring s i2 j)))))))))
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
          ((l0
            (if
              (y-obj-p x)
              (make-hash-table :test 'eq)
              nil)))
          (progn
            (progn
              (let*
                ((j 0))
                (let*
                  ((i3
                    (if
                      (or
                       (y-nil-p from)
                       (< from 0))
                      0 from)))
                  (progn
                    (let*
                      ((n2
                        (y-length x)))
                      (progn
                        (let*
                          ((upto
                            (if
                              (or
                               (y-nil-p upto)
                               (> upto n2))
                              n2 upto)))
                          (progn
                            (while
                              (< i3 upto)
                              (let*
                                ((k17 j)
                                 (v18
                                  (y-get x i3)))
                                (setq l0
                                      (y-put l0 k17 v18))
                                v18)
                              (setq i3
                                    (+ i3 1))
                              (setq j
                                    (+ j 1)))
                            (let*
                              ((o1 x))
                              (let*
                                ((f1
                                  #'(lambda
                                      (k a1)
                                      (let*
                                        ((v1 a1))
                                        (progn
                                          (progn
                                            (if
                                              (y-number-p k)
                                              nil
                                              (let*
                                                ((k18 k)
                                                 (v19 v1))
                                                (setq l0
                                                      (y-put l0 k18 v19))
                                                v19))))))))
                                (progn
                                  (if
                                    (hash-table-p o1)
                                    (maphash f1 o1)
                                    (if
                                      (listp o1)
                                      (let*
                                        ((i47 -1))
                                        (while o1
                                          (let*
                                            ((k
                                              (if
                                                (keywordp
                                                 (car o1))
                                                (car o1)
                                                (setq i47
                                                      (1+ i47))))
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
                                        ((n3
                                          (y-length o1)))
                                        (progn
                                          (let*
                                            ((k1 0))
                                            (progn
                                              (progn
                                                (while
                                                  (< k1 n3)
                                                  (let*
                                                    ((a1
                                                      (y-get o1 k1)))
                                                    (progn
                                                      (funcall f1 k1 a1)))
                                                  (setq k1
                                                        (+ k1 1))))))))))))))))))))
              l0))))))
(y-setenv 'cut :symbol 'y-cut))
(progn
(defalias 'y-keys
  #'(lambda
      (x)
      (let*
        ((l1
          (if
            (y-obj-p x)
            (make-hash-table :test 'eq)
            nil)))
        (progn
          (progn
            (let*
              ((o2 x))
              (let*
                ((f2
                  #'(lambda
                      (k a2)
                      (let*
                        ((v2 a2))
                        (progn
                          (progn
                            (if
                              (y-number-p k)
                              nil
                              (let*
                                ((k19 k)
                                 (v20 v2))
                                (setq l1
                                      (y-put l1 k19 v20))
                                v20))))))))
                (progn
                  (if
                    (hash-table-p o2)
                    (maphash f2 o2)
                    (if
                      (listp o2)
                      (let*
                        ((i48 -1))
                        (while o2
                          (let*
                            ((k
                              (if
                                (keywordp
                                 (car o2))
                                (car o2)
                                (setq i48
                                      (1+ i48))))
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
                        ((n4
                          (y-length o2)))
                        (progn
                          (let*
                            ((k2 0))
                            (progn
                              (progn
                                (while
                                  (< k2 n4)
                                  (let*
                                    ((a2
                                      (y-get o2 k2)))
                                    (progn
                                      (funcall f2 k2 a2)))
                                  (setq k2
                                        (+ k2 1)))))))))))))
            l1)))))
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
        ((l10
          (y-keys l)))
        (progn
          (progn
            (let*
              ((i4
                (y-edge l)))
              (progn
                (progn
                  (while
                    (>= i4 0)
                    (progn
                      (let*
                        ((k20
                          (y-length l10))
                         (v21
                          (y-get l i4)))
                        (setq l10
                              (y-put l10 k20 v21))
                        v21)
                      nil)
                    (setq i4
                          (- i4 1))))))
            l10)))))
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
          ((var0 ls))
          (let*
            ((a3
              (y-get var0 '0)))
            (progn
              (let*
                ((b1
                  (y-get var0 '1)))
                (progn
                  (progn
                    (if
                      (and a3 b1)
                      (let*
                        ((c0
                          (if
                            (y-obj-p a3)
                            (make-hash-table :test 'eq)
                            nil)))
                        (progn
                          (let*
                            ((o3
                              (y-length a3)))
                            (progn
                              (progn
                                (let*
                                  ((o4 a3))
                                  (let*
                                    ((f3
                                      #'(lambda
                                          (k a4)
                                          (let*
                                            ((v3 a4))
                                            (progn
                                              (progn
                                                (let*
                                                  ((k21 k)
                                                   (v22 v3))
                                                  (setq c0
                                                        (y-put c0 k21 v22))
                                                  v22)))))))
                                    (progn
                                      (if
                                        (hash-table-p o4)
                                        (maphash f3 o4)
                                        (if
                                          (listp o4)
                                          (let*
                                            ((i49 -1))
                                            (while o4
                                              (let*
                                                ((k
                                                  (if
                                                    (keywordp
                                                     (car o4))
                                                    (car o4)
                                                    (setq i49
                                                          (1+ i49))))
                                                 (a4
                                                  (if
                                                    (keywordp
                                                     (car o4))
                                                    (cadr o4)
                                                    (car o4))))
                                                (funcall f3 k a4))
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
                                                ((k3 0))
                                                (progn
                                                  (progn
                                                    (while
                                                      (< k3 n5)
                                                      (let*
                                                        ((a4
                                                          (y-get o4 k3)))
                                                        (progn
                                                          (funcall f3 k3 a4)))
                                                      (setq k3
                                                            (+ k3 1)))))))))))))
                                (let*
                                  ((o5 b1))
                                  (let*
                                    ((f4
                                      #'(lambda
                                          (k a5)
                                          (let*
                                            ((v4 a5))
                                            (progn
                                              (progn
                                                (if
                                                  (y-number-p k)
                                                  (progn
                                                    (setq k
                                                          (+ k o3))))
                                                (let*
                                                  ((k22 k)
                                                   (v23 v4))
                                                  (setq c0
                                                        (y-put c0 k22 v23))
                                                  v23)))))))
                                    (progn
                                      (if
                                        (hash-table-p o5)
                                        (maphash f4 o5)
                                        (if
                                          (listp o5)
                                          (let*
                                            ((i410 -1))
                                            (while o5
                                              (let*
                                                ((k
                                                  (if
                                                    (keywordp
                                                     (car o5))
                                                    (car o5)
                                                    (setq i410
                                                          (1+ i410))))
                                                 (a5
                                                  (if
                                                    (keywordp
                                                     (car o5))
                                                    (cadr o5)
                                                    (car o5))))
                                                (funcall f4 k a5))
                                              (setq o5
                                                    (if
                                                      (keywordp
                                                       (car o5))
                                                      (cddr o5)
                                                      (cdr o5))))
                                            nil)
                                          (let*
                                            ((n6
                                              (y-length o5)))
                                            (progn
                                              (let*
                                                ((k4 0))
                                                (progn
                                                  (progn
                                                    (while
                                                      (< k4 n6)
                                                      (let*
                                                        ((a5
                                                          (y-get o5 k4)))
                                                        (progn
                                                          (funcall f4 k4 a5)))
                                                      (setq k4
                                                            (+ k4 1)))))))))))))
                                c0)))))
                      (or a3 b1 nil))))))))
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
          ((o6 l))
          (let*
            ((f5
              #'(lambda
                  (i5 a6)
                  (let*
                    ((x0 a6))
                    (progn
                      (progn
                        (let*
                          ((y0
                            (funcall f x0)))
                          (progn
                            (progn
                              (if y0
                                  (throw 'y-break y0)))))))))))
            (progn
              (if
                (hash-table-p o6)
                (maphash f5 o6)
                (if
                  (listp o6)
                  (let*
                    ((i411 -1))
                    (while o6
                      (let*
                        ((i5
                          (if
                            (keywordp
                             (car o6))
                            (car o6)
                            (setq i411
                                  (1+ i411))))
                         (a6
                          (if
                            (keywordp
                             (car o6))
                            (cadr o6)
                            (car o6))))
                        (funcall f5 i5 a6))
                      (setq o6
                            (if
                              (keywordp
                               (car o6))
                              (cddr o6)
                              (cdr o6))))
                    nil)
                  (let*
                    ((n7
                      (y-length o6)))
                    (progn
                      (let*
                        ((i5 0))
                        (progn
                          (while
                            (< i5 n7)
                            (let*
                              ((a6
                                (y-get o6 i5)))
                              (progn
                                (funcall f5 i5 a6)))
                            (setq i5
                                  (+ i5 1)))))))))))))))
(y-setenv 'find :symbol 'y-find))
(progn
(defalias 'y-first
  #'(lambda
      (f l)
      (catch 'y-break
        (let*
          ((x1 l))
          (let*
            ((n8
              (y-length x1)))
            (progn
              (let*
                ((i40 0))
                (progn
                  (while
                    (< i40 n8)
                    (let*
                      ((x2
                        (y-get x1 i40)))
                      (progn
                        (progn
                          (let*
                            ((y1
                              (funcall f x2)))
                            (progn
                              (progn
                                (if y1
                                    (throw 'y-break y1))))))))
                    (setq i40
                          (+ i40 1)))))))))))
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
        ((l11
          (if
            (y-obj-p l)
            (make-hash-table :test 'eq)
            nil)))
        (progn
          (progn
            (let*
              ((n9
                (y-length l)))
              (progn
                (progn
                  (let*
                    ((i6 0))
                    (progn
                      (progn
                        (while
                          (< i6 n9)
                          (progn
                            (let*
                              ((k23
                                (y-length l11))
                               (v24
                                (list
                                  (y-get l i6)
                                  (y-get l
                                         (+ i6 1)))))
                              (setq l11
                                    (y-put l11 k23 v24))
                              v24)
                            nil)
                          (setq i6
                                (+ i6 1))
                          (setq i6
                                (+ i6 1)))))))))
            l11)))))
(y-setenv 'pair :symbol 'y-pair))
(progn
(defalias 'y-map
  #'(lambda
      (f x)
      (let*
        ((l2
          (if
            (y-obj-p x)
            (make-hash-table :test 'eq)
            nil)))
        (progn
          (progn
            (let*
              ((x3 x))
              (let*
                ((n10
                  (y-length x3)))
                (progn
                  (let*
                    ((i41 0))
                    (progn
                      (while
                        (< i41 n10)
                        (let*
                          ((v5
                            (y-get x3 i41)))
                          (progn
                            (progn
                              (let*
                                ((y2
                                  (funcall f v5)))
                                (progn
                                  (progn
                                    (if
                                      (y-is-p y2)
                                      (progn
                                        (let*
                                          ((k24
                                            (y-length l2))
                                           (v25 y2))
                                          (setq l2
                                                (y-put l2 k24 v25))
                                          v25)
                                        nil))))))))
                        (setq i41
                              (+ i41 1))))))))
            (let*
              ((o7 x))
              (let*
                ((f6
                  #'(lambda
                      (k a7)
                      (let*
                        ((v6 a7))
                        (progn
                          (progn
                            (if
                              (y-number-p k)
                              nil
                              (let*
                                ((y3
                                  (funcall f v6)))
                                (progn
                                  (progn
                                    (if
                                      (y-is-p y3)
                                      (progn
                                        (let*
                                          ((k25 k)
                                           (v26 y3))
                                          (setq l2
                                                (y-put l2 k25 v26))
                                          v26)))))))))))))
                (progn
                  (if
                    (hash-table-p o7)
                    (maphash f6 o7)
                    (if
                      (listp o7)
                      (let*
                        ((i412 -1))
                        (while o7
                          (let*
                            ((k
                              (if
                                (keywordp
                                 (car o7))
                                (car o7)
                                (setq i412
                                      (1+ i412))))
                             (a7
                              (if
                                (keywordp
                                 (car o7))
                                (cadr o7)
                                (car o7))))
                            (funcall f6 k a7))
                          (setq o7
                                (if
                                  (keywordp
                                   (car o7))
                                  (cddr o7)
                                  (cdr o7))))
                        nil)
                      (let*
                        ((n11
                          (y-length o7)))
                        (progn
                          (let*
                            ((k5 0))
                            (progn
                              (progn
                                (while
                                  (< k5 n11)
                                  (let*
                                    ((a7
                                      (y-get o7 k5)))
                                    (progn
                                      (funcall f6 k5 a7)))
                                  (setq k5
                                        (+ k5 1)))))))))))))
            l2)))))
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
          ((o8 l))
          (let*
            ((f7
              #'(lambda
                  (k a8)
                  (let*
                    ((v7 a8))
                    (progn
                      (progn
                        (setq v7 v7)
                        (if
                          (y-number-p k)
                          nil
                          (throw 'y-break t))))))))
            (progn
              (if
                (hash-table-p o8)
                (maphash f7 o8)
                (if
                  (listp o8)
                  (let*
                    ((i413 -1))
                    (while o8
                      (let*
                        ((k
                          (if
                            (keywordp
                             (car o8))
                            (car o8)
                            (setq i413
                                  (1+ i413))))
                         (a8
                          (if
                            (keywordp
                             (car o8))
                            (cadr o8)
                            (car o8))))
                        (funcall f7 k a8))
                      (setq o8
                            (if
                              (keywordp
                               (car o8))
                              (cddr o8)
                              (cdr o8))))
                    nil)
                  (let*
                    ((n12
                      (y-length o8)))
                    (progn
                      (let*
                        ((k6 0))
                        (progn
                          (progn
                            (while
                              (< k6 n12)
                              (let*
                                ((a8
                                  (y-get o8 k6)))
                                (progn
                                  (funcall f7 k6 a8)))
                              (setq k6
                                    (+ k6 1)))))))))))))
        nil)))
(y-setenv 'keys\? :symbol 'y-keys-p))
(progn
(defalias 'y-empty-p
  #'(lambda
      (l)
      (catch 'y-break
        (let*
          ((o9 l))
          (let*
            ((f8
              #'(lambda
                  (i7 a9)
                  (let*
                    ((x4 a9))
                    (progn
                      (progn
                        (setq x4 x4)
                        (throw 'y-break nil)))))))
            (progn
              (if
                (hash-table-p o9)
                (maphash f8 o9)
                (if
                  (listp o9)
                  (let*
                    ((i414 -1))
                    (while o9
                      (let*
                        ((i7
                          (if
                            (keywordp
                             (car o9))
                            (car o9)
                            (setq i414
                                  (1+ i414))))
                         (a9
                          (if
                            (keywordp
                             (car o9))
                            (cadr o9)
                            (car o9))))
                        (funcall f8 i7 a9))
                      (setq o9
                            (if
                              (keywordp
                               (car o9))
                              (cddr o9)
                              (cdr o9))))
                    nil)
                  (let*
                    ((n13
                      (y-length o9)))
                    (progn
                      (let*
                        ((i7 0))
                        (progn
                          (while
                            (< i7 n13)
                            (let*
                              ((a9
                                (y-get o9 i7)))
                              (progn
                                (funcall f8 i7 a9)))
                            (setq i7
                                  (+ i7 1))))))))))))
        t)))
(y-setenv 'empty\? :symbol 'y-empty-p))
(progn
(defalias 'y-stash
  #'(lambda
      (args)
      (let*
        ((l3 nil))
        (progn
          (progn
            (let*
              ((x5 args))
              (let*
                ((n14
                  (y-length x5)))
                (progn
                  (let*
                    ((i42 0))
                    (progn
                      (while
                        (< i42 n14)
                        (let*
                          ((x6
                            (y-get x5 i42)))
                          (progn
                            (progn
                              (progn
                                (let*
                                  ((k26
                                    (y-length l3))
                                   (v27 x6))
                                  (setq l3
                                        (y-put l3 k26 v27))
                                  v27)
                                nil))))
                        (setq i42
                              (+ i42 1))))))))
            (if
              (y-keys-p args)
              (progn
                (let*
                  ((p
                    (if
                      (y-obj-p args)
                      (make-hash-table :test 'eq)
                      nil)))
                  (progn
                    (let*
                      ((o10 args))
                      (let*
                        ((f9
                          #'(lambda
                              (k a10)
                              (let*
                                ((v8 a10))
                                (progn
                                  (progn
                                    (if
                                      (y-number-p k)
                                      nil
                                      (let*
                                        ((k27 k)
                                         (v28 v8))
                                        (setq p
                                              (y-put p k27 v28))
                                        v28))))))))
                        (progn
                          (if
                            (hash-table-p o10)
                            (maphash f9 o10)
                            (if
                              (listp o10)
                              (let*
                                ((i415 -1))
                                (while o10
                                  (let*
                                    ((k
                                      (if
                                        (keywordp
                                         (car o10))
                                        (car o10)
                                        (setq i415
                                              (1+ i415))))
                                     (a10
                                      (if
                                        (keywordp
                                         (car o10))
                                        (cadr o10)
                                        (car o10))))
                                    (funcall f9 k a10))
                                  (setq o10
                                        (if
                                          (keywordp
                                           (car o10))
                                          (cddr o10)
                                          (cdr o10))))
                                nil)
                              (let*
                                ((n15
                                  (y-length o10)))
                                (progn
                                  (let*
                                    ((k7 0))
                                    (progn
                                      (progn
                                        (while
                                          (< k7 n15)
                                          (let*
                                            ((a10
                                              (y-get o10 k7)))
                                            (progn
                                              (funcall f9 k7 a10)))
                                          (setq k7
                                                (+ k7 1)))))))))))))
                    (let*
                      ((k28 :_stash)
                       (v29 t))
                      (setq p
                            (y-put p k28 v29))
                      v29)
                    (progn
                      (let*
                        ((k29
                          (y-length l3))
                         (v30 p))
                        (setq l3
                              (y-put l3 k29 v30))
                        v30)
                      nil)))))
            l3)))))
(y-setenv 'stash :symbol 'y-stash))
(progn
(defalias 'y-unstash
  #'(lambda
      (args)
      (if
        (y-none-p args)
        nil
        (let*
          ((l4
            (y-last args)))
          (progn
            (progn
              (if
                (y-get l4 :_stash)
                (let*
                  ((args10
                    (y-almost args)))
                  (progn
                    (progn
                      (let*
                        ((o11 l4))
                        (let*
                          ((f10
                            #'(lambda
                                (k a11)
                                (let*
                                  ((v9 a11))
                                  (progn
                                    (progn
                                      (if
                                        (eql k :_stash)
                                        nil
                                        (let*
                                          ((k30 k)
                                           (v31 v9))
                                          (setq args10
                                                (y-put args10 k30 v31))
                                          v31))))))))
                          (progn
                            (if
                              (hash-table-p o11)
                              (maphash f10 o11)
                              (if
                                (listp o11)
                                (let*
                                  ((i416 -1))
                                  (while o11
                                    (let*
                                      ((k
                                        (if
                                          (keywordp
                                           (car o11))
                                          (car o11)
                                          (setq i416
                                                (1+ i416))))
                                       (a11
                                        (if
                                          (keywordp
                                           (car o11))
                                          (cadr o11)
                                          (car o11))))
                                      (funcall f10 k a11))
                                    (setq o11
                                          (if
                                            (keywordp
                                             (car o11))
                                            (cddr o11)
                                            (cdr o11))))
                                  nil)
                                (let*
                                  ((n16
                                    (y-length o11)))
                                  (progn
                                    (let*
                                      ((k8 0))
                                      (progn
                                        (progn
                                          (while
                                            (< k8 n16)
                                            (let*
                                              ((a11
                                                (y-get o11 k8)))
                                              (progn
                                                (funcall f10 k8 a11)))
                                            (setq k8
                                                  (+ k8 1)))))))))))))
                      args10)))
                args)))))))
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
          ((o12 l))
          (let*
            ((f11
              #'(lambda
                  (k a12)
                  (let*
                    ((v10 a12))
                    (progn
                      (progn
                        (if
                          (eql k :_stash)
                          nil
                          (let*
                            ((k31 k)
                             (v32 v10))
                            (setq args1
                                  (y-put args1 k31 v32))
                            v32))))))))
            (progn
              (if
                (hash-table-p o12)
                (maphash f11 o12)
                (if
                  (listp o12)
                  (let*
                    ((i417 -1))
                    (while o12
                      (let*
                        ((k
                          (if
                            (keywordp
                             (car o12))
                            (car o12)
                            (setq i417
                                  (1+ i417))))
                         (a12
                          (if
                            (keywordp
                             (car o12))
                            (cadr o12)
                            (car o12))))
                        (funcall f11 k a12))
                      (setq o12
                            (if
                              (keywordp
                               (car o12))
                              (cddr o12)
                              (cdr o12))))
                    nil)
                  (let*
                    ((n17
                      (y-length o12)))
                    (progn
                      (let*
                        ((k9 0))
                        (progn
                          (progn
                            (while
                              (< k9 n17)
                              (let*
                                ((a12
                                  (y-get o12 k9)))
                                (progn
                                  (funcall f11 k9 a12)))
                              (setq k9
                                    (+ k9 1)))))))))))))
        l)))
(y-setenv 'destash! :symbol 'y-destash!))
(progn
(defalias 'y-apply
  #'(lambda
      (f args)
      (let*
        ((args11
          (y-stash args)))
        (progn
          (progn
            (funcall 'apply f args11))))))
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
                      ((k32
                        (y-edge s0))
                       (v33 45))
                      (setq s0
                            (y-put s0 k32 v33))
                      v33)
                    (let*
                      ((k33
                        (y-length s0))
                       (v34 112))
                      (setq s0
                            (y-put s0 k33 v34))
                      v34))
                  (let*
                    ((k34
                      (y-edge s0))
                     (v35 112))
                    (setq s0
                          (y-put s0 k34 v35))
                    v35))))
            (intern
             (concat s0)))))))
(y-setenv 'id :symbol 'y--id))
(defvar y-module nil)
(progn
(defalias 'y--module-name
  #'(lambda nil
      (or y-module
          (let*
            ((file0
              (or load-file-name
                  (buffer-file-name))))
            (progn
              (progn
                (if file0
                    (file-name-base file0)
                    (buffer-name))))))))
(y-setenv 'module-name :symbol 'y--module-name))
(progn
(defalias 'y--global-id
  #'(lambda
      (prefix name)
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
               (concat prefix s1))))))))
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
        ((v11
          (y--symbol-expansion k)))
        (progn
          (progn
            (and v11
                 (not
                  (eq v11 k))))))))
(y-setenv 'symbol\? :symbol 'y--symbol-p))
(progn
(defalias 'y--variable-p
  #'(lambda
      (k)
      (let*
        ((i8
          (-
           (y-length y-environment)
           1)))
        (progn
          (progn
            (catch 'y-break
              (while
                (>= i8 0)
                (let*
                  ((b2
                    (y-get
                     (y-get y-environment i8)
                     k)))
                  (progn
                    (progn
                      (if b2
                          (throw 'y-break
                                 (and b2
                                      (y-get b2 :variable)))
                          (setq i8
                                (1- i8)))))))))))))
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
(defalias 'y--flag-p
  #'(lambda
      (x)
      (and
       (keywordp x)
       (let*
         ((s2
           (symbol-name x)))
         (progn
           (progn
             (eql 58
                  (y-get s2
                         (y-edge s2)))))))))
(y-setenv 'flag\? :symbol 'y--flag-p))
(progn
(defalias 'y--key-p
  #'(lambda
      (x)
      (and
       (not
        (keywordp x))
       (symbolp x)
       (let*
         ((s3
           (symbol-name x)))
         (progn
           (progn
             (eql 58
                  (y-get s3
                         (y-edge s3)))))))))
(y-setenv 'key\? :symbol 'y--key-p))
(progn
(defalias 'y--key
  #'(lambda
      (x)
      (if
        (y--flag-p x)
        (intern
         (let*
           ((s4
             (symbol-name x)))
           (progn
             (progn
               (y-clip s4 0
                       (y-edge s4))))))
        (if
          (y--key-p x)
          (intern
           (let*
             ((s5
               (symbol-name x)))
             (progn
               (progn
                 (concat ":"
                         (y-clip s5 0
                                 (y-edge s5)))))))
          x))))
(y-setenv 'key :symbol 'y--key))
(progn
(defalias 'y--%list
  #'(lambda
      (args)
      (let*
        ((l5 nil))
        (progn
          (progn
            (while args
              (let*
                ((k10
                  (car args)))
                (progn
                  (let*
                    ((v12
                      (cadr args)))
                    (progn
                      (progn
                        (if
                          (or
                           (eql k10 '&rest)
                           (eql k10 :rest))
                          (progn
                            (setq k10 'rest:)))
                        (if
                          (eql k10 '&optional)
                          nil
                          (if
                            (y--key-p k10)
                            (progn
                              (setq l5
                                    (y-join l5
                                            (list
                                              (y--key k10)
                                              v12)))
                              (setq args
                                    (cdr args)))
                            (if
                              (or
                               (y--flag-p k10)
                               (keywordp k10))
                              (setq l5
                                    (y-join l5
                                            (list
                                              (y--key k10)
                                              t)))
                              (progn
                                (let*
                                  ((k35
                                    (y-length l5))
                                   (v36
                                    (car args)))
                                  (setq l5
                                        (y-put l5 k35 v36))
                                  v36)
                                nil))))
                        (setq args
                              (cdr args))))))))
            l5)))))
(y-setenv '%list :symbol 'y--%list))
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
            ((bs0
              (list var rh)))
            (progn
              (progn
                (let*
                  ((l6
                    (y--%list lh)))
                  (progn
                    (progn
                      (let*
                        ((o13 l6))
                        (let*
                          ((f12
                            #'(lambda
                                (k a13)
                                (let*
                                  ((v13 a13))
                                  (progn
                                    (progn
                                      (let*
                                        ((x7
                                          (if
                                            (eql k :rest)
                                            (list 'cut var
                                                  (y-length l6))
                                            (list 'get var
                                                  (list 'quote k)))))
                                        (progn
                                          (progn
                                            (if
                                              (y-is-p k)
                                              (progn
                                                (let*
                                                  ((k12
                                                    (if
                                                      (eql v13 t)
                                                      (y--unkeywordify k)
                                                      v13)))
                                                  (progn
                                                    (progn
                                                      (setq bs0
                                                            (y-join bs0
                                                                    (y--bind k12 x7)))))))))))))))))
                          (progn
                            (if
                              (hash-table-p o13)
                              (maphash f12 o13)
                              (if
                                (listp o13)
                                (let*
                                  ((i418 -1))
                                  (while o13
                                    (let*
                                      ((k
                                        (if
                                          (keywordp
                                           (car o13))
                                          (car o13)
                                          (setq i418
                                                (1+ i418))))
                                       (a13
                                        (if
                                          (keywordp
                                           (car o13))
                                          (cadr o13)
                                          (car o13))))
                                      (funcall f12 k a13))
                                    (setq o13
                                          (if
                                            (keywordp
                                             (car o13))
                                            (cddr o13)
                                            (cdr o13))))
                                  nil)
                                (let*
                                  ((n18
                                    (y-length o13)))
                                  (progn
                                    (let*
                                      ((k11 0))
                                      (progn
                                        (progn
                                          (while
                                            (< k11 n18)
                                            (let*
                                              ((a13
                                                (y-get o13 k11)))
                                              (progn
                                                (funcall f12 k11 a13)))
                                            (setq k11
                                                  (+ k11 1))))))))))))))))
                bs0)))))))
(y-setenv 'bind :symbol 'y--bind))
(progn
(defalias 'y-macroexpand
  #'(lambda
      (form)
      (let*
        ((s6
          (y--symbol-expansion form)))
        (progn
          (progn
            (if s6
                (y-macroexpand s6)
                (if
                  (atom form)
                  form
                  (let*
                    ((x8
                      (y-macroexpand
                       (y-hd form))))
                    (progn
                      (progn
                        (if
                          (eq x8 'quote)
                          form
                          (if
                            (eq x8 '\`)
                            (y-macroexpand
                             (funcall 'macroexpand form))
                            (if
                              (y--macro-p x8)
                              (y-macroexpand
                               (funcall 'apply
                                        (y--macro-function x8)
                                        (y-tl form)))
                              (cons x8
                                    (mapcar 'y-macroexpand
                                            (y-tl form))))))))))))))))
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
              (cons 'let
                    (cons
                     (list x v)
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
                ((form1
                  (list 'setenv
                        (list 'quote name)
                        ':macro
                        (cons 'fn
                              (cons args body)))))
                (progn
                  (progn
                    (y-eval form1)
                    form1)))))
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
                ((var2
                  (y--global-id
                   (concat
                    (y--module-name)
                    "--")
                   name)))
                (progn
                  (progn
                    (y-setenv name :symbol var2)
                    (y-setenv var2 :variable t)
                    (list 'progn
                          (list 'defalias
                                (list 'quote var2)
                                (cons 'fn
                                      (cons x body)))
                          (list 'setenv
                                (list 'quote name)
                                ':symbol
                                (list 'quote var2))))))))
(y-setenv 'define-global :macro
          #'(lambda
              (name x &rest body)
              (let*
                ((var4
                  (y--global-id
                   (concat
                    (y--module-name)
                    "-")
                   name)))
                (progn
                  (progn
                    (y-setenv name :symbol var4)
                    (y-setenv var4 :variable t :toplevel t)
                    (list 'progn
                          (list 'defalias
                                (list 'quote var4)
                                (cons 'fn
                                      (cons x body)))
                          (list 'setenv
                                (list 'quote name)
                                ':symbol
                                (list 'quote var4))))))))
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
    ((x10
      (progn
        (y-map
         #'(lambda
             (m)
             (y-macroexpand
              (cons 'define-macro m)))
         definitions)
        (cons 'progn
              (y-macroexpand body)))))
    (progn
      (setq y-environment
            (y-apply 'vector
                     (y-almost y-environment)))
      x10)))))
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
                    ((x12
                      (progn
                        (mapc
                         #'(lambda
                             (x)
                             (y-macroexpand
                              (cons 'define-symbol x)))
                         (y-pair expansions))
                        (cons 'progn
                              (y-macroexpand body)))))
                    (progn
                      (setq y-environment
                            (y-apply 'vector
                                     (y-almost y-environment)))
                      x12))))))
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
                    ((var8 bs))
                    (let*
                      ((lh1
                        (y-get var8 '0)))
                      (progn
                        (let*
                          ((rh1
                            (y-get var8 '1)))
                          (progn
                            (let*
                              ((bs21
                                (y-cut var8 2)))
                              (progn
                                (let*
                                  ((var9
                                    (y--bind lh1 rh1)))
                                  (let*
                                    ((var10
                                      (y-get var9 '0)))
                                    (progn
                                      (let*
                                        ((val1
                                          (y-get var9 '1)))
                                        (progn
                                          (let*
                                            ((bs11
                                              (y-cut var9 2)))
                                            (progn
                                              (progn
                                                (let*
                                                  ((renames1 nil))
                                                  (progn
                                                    (progn
                                                      (if
                                                        (or
                                                         (y--bound-p var10)
                                                         (y-toplevel-p))
                                                        (let*
                                                          ((var11
                                                            (y-unique var10)))
                                                          (progn
                                                            (progn
                                                              (setq renames1
                                                                    (list var10 var11))
                                                              (setq var10 var11))))
                                                        (y-setenv var10 :variable t))
                                                      (let*
                                                        ((form3
                                                          (cons 'let
                                                                (cons
                                                                 (y-join bs11 bs21)
                                                                 body))))
                                                        (progn
                                                          (progn
                                                            (if
                                                              (y-none-p renames1)
                                                              nil
                                                              (setq form3
                                                                    (list 'let-symbol renames1 form3)))
                                                            (list 'let*
                                                                  (list
                                                                    (list var10 val1))
                                                                  (y-macroexpand form3)))))))))))))))))))))))))))
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
              (i4 to &rest body)
              (list 'let i4 0
                    (cons 'while
                          (cons
                           (list '< i4 to)
                           (append body
                                   (list
                                     (list 'inc i4))))))))
(y-setenv 'step :macro
          #'(lambda
              (v l &rest body)
              (let*
                ((x
                  (y-unique 'x))
                 (n
                  (y-unique 'n))
                 (i4
                  (y-unique 'i4)))
                (list 'let
                      (list x l n
                            (list '\# x))
                      (list 'for i4 n
                            (cons 'let
                                  (cons
                                   (list v
                                         (list 'at x i4))
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
                  (y-unique 'f))
                 (a
                  (y-unique 'a)))
                (let*
                  ((var12
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
                  (let*
                    ((k14
                      (y-get var12 '0)))
                    (progn
                      (let*
                        ((v15
                          (y-get var12 '1)))
                        (progn
                          (progn
                            (list 'let
                                  (list o l f
                                        (list 'lambda
                                              (list k14 a)
                                              (cons 'let
                                                    (cons
                                                     (list v15 a)
                                                     body))))
                                  (list 'if
                                        (list 'hash-table-p o)
                                        (list 'maphash f o)
                                        (list 'if
                                              (list 'listp o)
                                              (list 'y-%for o k14 a
                                                    (list 'funcall f k14 a))
                                              (list 'let n
                                                    (list '\# o)
                                                    (list 'for k14 n
                                                          (list 'let
                                                                (list a
                                                                      (list 'at o k14))
                                                                (list 'funcall f k14 a)))))))))))))))))
  (provide 'y))
