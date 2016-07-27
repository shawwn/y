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
              ((i12 -1))
              (while h
                (let*
                  ((var
                    (if
                      (keywordp
                       (car h))
                      (car h)
                      (setq i12
                            (1+ i12))))
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
                    ((i13 -1))
                    (while h
                      (let*
                        ((var
                          (if
                            (keywordp
                             (car h))
                            (car h)
                            (setq i13
                                  (1+ i13))))
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
              ((i14 -1))
              (while h
                (let*
                  ((k
                    (if
                      (keywordp
                       (car h))
                      (car h)
                      (setq i14
                            (1+ i14))))
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
      (progn
        (let*
          ((i
            (if
              (memq :toplevel ks)
              0
              (-
               (y-length y-environment)
               1))))
          (progn
            (let*
              ((frame
                (y-get y-environment i)))
              (let*
                ((entry
                  (or
                   (y-get frame k)
                   (make-hash-table :test 'eq))))
                (progn
                  (progn
                    (let*
                      ((o0 ks))
                      (let*
                        ((f0
                          #'(lambda
                              (k v)
                              (progn
                                (let*
                                  ((k14 k)
                                   (v4 v))
                                  (setq entry
                                        (y-put entry k14 v4))
                                  v4)))))
                        (progn
                          (if
                            (hash-table-p o0)
                            (maphash f0 o0)
                            (if
                              (listp o0)
                              (let*
                                ((i15 -1))
                                (while o0
                                  (let*
                                    ((k
                                      (if
                                        (keywordp
                                         (car o0))
                                        (car o0)
                                        (setq i15
                                              (1+ i15))))
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
                                      (progn
                                        (while
                                          (< k n0)
                                          (let*
                                            ((a0
                                              (y-get o0 k)))
                                            (progn
                                              (funcall f0 k a0)))
                                          (setq k
                                                (+ k 1)))))))))))))
                    (let*
                      ((k15 k)
                       (v5 entry))
                      (setq frame
                            (y-put frame k15 v5))
                      v5))))))))))
(y-setenv 'setenv :symbol 'y-setenv))
(progn
(defalias 'y-getenv
  #'(lambda
      (k &optional p)
      (progn
        (let*
          ((i
            (-
             (y-length y-environment)
             1)))
          (progn
            (progn
              (catch 'y-break
                (while
                  (>= i 0)
                  (let*
                    ((b
                      (y-get
                       (y-get y-environment i)
                       k)))
                    (progn
                      (progn
                        (if b
                            (throw 'y-break
                                   (if p
                                       (y-get b p)
                                       b))
                            (setq i
                                  (- i 1))))))))))))))
(y-setenv 'getenv :symbol 'y-getenv))
(y-setenv 'unique :symbol 'y-unique)
(y-setenv 'let-unique :symbol 'y-let-unique)
(y-setenv 'at :symbol 'get)
(y-setenv 'set :macro
          #'(lambda
              (&rest args)
              (progn
                (cons 'y-set args))))
(y-setenv 'get :symbol 'y-get)
(y-setenv '\# :symbol 'y-length)
(y-setenv '= :symbol 'eql)
(y-setenv 'environment :symbol 'y-environment)
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
          ((n
            (length s)))
          (progn
            (let*
              ((i
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
                       (> upto n))
                      n
                      (max upto i))))
                  (progn
                    (progn
                      (substring s i j)))))))))))
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
            ((l
              (if
                (y-obj-p x)
                (make-hash-table :test 'eq)
                nil)))
            (progn
              (progn
                (let*
                  ((j 0))
                  (progn
                    (let*
                      ((i
                        (if
                          (or
                           (y-nil-p from)
                           (< from 0))
                          0 from)))
                      (progn
                        (let*
                          ((n
                            (y-length x)))
                          (progn
                            (let*
                              ((upto
                                (if
                                  (or
                                   (y-nil-p upto)
                                   (> upto n))
                                  n upto)))
                              (progn
                                (progn
                                  (while
                                    (< i upto)
                                    (let*
                                      ((k16 j)
                                       (v6
                                        (y-get x i)))
                                      (setq l
                                            (y-put l k16 v6))
                                      v6)
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
                                                  ((k17 k)
                                                   (v7 v))
                                                  (setq l
                                                        (y-put l k17 v7))
                                                  v7))))))
                                      (progn
                                        (if
                                          (hash-table-p o1)
                                          (maphash f1 o1)
                                          (if
                                            (listp o1)
                                            (let*
                                              ((i16 -1))
                                              (while o1
                                                (let*
                                                  ((k
                                                    (if
                                                      (keywordp
                                                       (car o1))
                                                      (car o1)
                                                      (setq i16
                                                            (1+ i16))))
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
                                                  ((k 0))
                                                  (progn
                                                    (progn
                                                      (while
                                                        (< k n3)
                                                        (let*
                                                          ((a1
                                                            (y-get o1 k)))
                                                          (progn
                                                            (funcall f1 k a1)))
                                                        (setq k
                                                              (+ k 1))))))))))))))))))))))
                l)))))))
(y-setenv 'cut :symbol 'y-cut))
(progn
(defalias 'y-keys
  #'(lambda
      (x)
      (progn
        (let*
          ((l
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
                        (k v)
                        (progn
                          (if
                            (y-number-p k)
                            nil
                            (let*
                              ((k18 k)
                               (v8 v))
                              (setq l
                                    (y-put l k18 v8))
                              v8))))))
                  (progn
                    (if
                      (hash-table-p o2)
                      (maphash f2 o2)
                      (if
                        (listp o2)
                        (let*
                          ((i17 -1))
                          (while o2
                            (let*
                              ((k
                                (if
                                  (keywordp
                                   (car o2))
                                  (car o2)
                                  (setq i17
                                        (1+ i17))))
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
                              ((k 0))
                              (progn
                                (progn
                                  (while
                                    (< k n4)
                                    (let*
                                      ((a2
                                        (y-get o2 k)))
                                      (progn
                                        (funcall f2 k a2)))
                                    (setq k
                                          (+ k 1)))))))))))))
              l))))))
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
          ((l1
            (y-keys l)))
          (progn
            (progn
              (let*
                ((i
                  (y-edge l)))
                (progn
                  (progn
                    (while
                      (>= i 0)
                      (progn
                        (let*
                          ((k19
                            (y-length l1))
                           (v9
                            (y-get l i)))
                          (setq l1
                                (y-put l1 k19 v9))
                          v9)
                        nil)
                      (setq i
                            (- i 1))))))
              l1))))))
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
            ((var0 ls))
            (let*
              ((a
                (y-get var0 '0)))
              (progn
                (let*
                  ((b
                    (y-get var0 '1)))
                  (progn
                    (progn
                      (if
                        (and a b)
                        (let*
                          ((c
                            (if
                              (y-obj-p a)
                              (make-hash-table :test 'eq)
                              nil)))
                          (progn
                            (let*
                              ((o
                                (y-length a)))
                              (progn
                                (progn
                                  (let*
                                    ((o4 a))
                                    (let*
                                      ((f3
                                        #'(lambda
                                            (k v)
                                            (progn
                                              (let*
                                                ((k20 k)
                                                 (v10 v))
                                                (setq c
                                                      (y-put c k20 v10))
                                                v10)))))
                                      (progn
                                        (if
                                          (hash-table-p o4)
                                          (maphash f3 o4)
                                          (if
                                            (listp o4)
                                            (let*
                                              ((i18 -1))
                                              (while o4
                                                (let*
                                                  ((k
                                                    (if
                                                      (keywordp
                                                       (car o4))
                                                      (car o4)
                                                      (setq i18
                                                            (1+ i18))))
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
                                                  ((k 0))
                                                  (progn
                                                    (progn
                                                      (while
                                                        (< k n5)
                                                        (let*
                                                          ((a4
                                                            (y-get o4 k)))
                                                          (progn
                                                            (funcall f3 k a4)))
                                                        (setq k
                                                              (+ k 1)))))))))))))
                                  (let*
                                    ((o5 b))
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
                                                ((k21 k)
                                                 (v11 v))
                                                (setq c
                                                      (y-put c k21 v11))
                                                v11)))))
                                      (progn
                                        (if
                                          (hash-table-p o5)
                                          (maphash f4 o5)
                                          (if
                                            (listp o5)
                                            (let*
                                              ((i19 -1))
                                              (while o5
                                                (let*
                                                  ((k
                                                    (if
                                                      (keywordp
                                                       (car o5))
                                                      (car o5)
                                                      (setq i19
                                                            (1+ i19))))
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
                                                  ((k 0))
                                                  (progn
                                                    (progn
                                                      (while
                                                        (< k n6)
                                                        (let*
                                                          ((a5
                                                            (y-get o5 k)))
                                                          (progn
                                                            (funcall f4 k a5)))
                                                        (setq k
                                                              (+ k 1)))))))))))))
                                  c)))))
                        (or a b nil))))))))
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
            ((o6 l))
            (let*
              ((f5
                #'(lambda
                    (i5 x)
                    (progn
                      (let*
                        ((y
                          (funcall f x)))
                        (progn
                          (progn
                            (if y
                                (throw 'y-break y)))))))))
              (progn
                (if
                  (hash-table-p o6)
                  (maphash f5 o6)
                  (if
                    (listp o6)
                    (let*
                      ((i20 -1))
                      (while o6
                        (let*
                          ((i5
                            (if
                              (keywordp
                               (car o6))
                              (car o6)
                              (setq i20
                                    (1+ i20))))
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
                                    (+ i5 1))))))))))))))))
(y-setenv 'find :symbol 'y-find))
(progn
(defalias 'y-first
  #'(lambda
      (f l)
      (progn
        (catch 'y-break
          (let*
            ((x0 l))
            (let*
              ((n8
                (y-length x0)))
              (progn
                (let*
                  ((i6 0))
                  (progn
                    (while
                      (< i6 n8)
                      (let*
                        ((x
                          (y-get x0 i6)))
                        (progn
                          (progn
                            (let*
                              ((y
                                (funcall f x)))
                              (progn
                                (progn
                                  (if y
                                      (throw 'y-break y))))))))
                      (setq i6
                            (+ i6 1))))))))))))
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
          ((l1
            (if
              (y-obj-p l)
              (make-hash-table :test 'eq)
              nil)))
          (progn
            (progn
              (let*
                ((n
                  (y-length l)))
                (progn
                  (progn
                    (let*
                      ((i 0))
                      (progn
                        (progn
                          (while
                            (< i n)
                            (progn
                              (let*
                                ((k22
                                  (y-length l1))
                                 (v12
                                  (list
                                    (y-get l i)
                                    (y-get l
                                           (+ i 1)))))
                                (setq l1
                                      (y-put l1 k22 v12))
                                v12)
                              nil)
                            (setq i
                                  (+ i 1))
                            (setq i
                                  (+ i 1)))))))))
              l1))))))
(y-setenv 'pair :symbol 'y-pair))
(progn
(defalias 'y-map
  #'(lambda
      (f x)
      (progn
        (let*
          ((l
            (if
              (y-obj-p x)
              (make-hash-table :test 'eq)
              nil)))
          (progn
            (progn
              (let*
                ((x2 x))
                (let*
                  ((n10
                    (y-length x2)))
                  (progn
                    (let*
                      ((i8 0))
                      (progn
                        (while
                          (< i8 n10)
                          (let*
                            ((v
                              (y-get x2 i8)))
                            (progn
                              (progn
                                (let*
                                  ((y
                                    (funcall f v)))
                                  (progn
                                    (progn
                                      (if
                                        (y-is-p y)
                                        (progn
                                          (let*
                                            ((k23
                                              (y-length l))
                                             (v13 y))
                                            (setq l
                                                  (y-put l k23 v13))
                                            v13)
                                          nil))))))))
                          (setq i8
                                (+ i8 1))))))))
              (let*
                ((o7 x))
                (let*
                  ((f6
                    #'(lambda
                        (k v)
                        (progn
                          (if
                            (y-number-p k)
                            nil
                            (let*
                              ((y
                                (funcall f v)))
                              (progn
                                (progn
                                  (if
                                    (y-is-p y)
                                    (progn
                                      (let*
                                        ((k24 k)
                                         (v14 y))
                                        (setq l
                                              (y-put l k24 v14))
                                        v14)))))))))))
                  (progn
                    (if
                      (hash-table-p o7)
                      (maphash f6 o7)
                      (if
                        (listp o7)
                        (let*
                          ((i21 -1))
                          (while o7
                            (let*
                              ((k
                                (if
                                  (keywordp
                                   (car o7))
                                  (car o7)
                                  (setq i21
                                        (1+ i21))))
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
                              ((k 0))
                              (progn
                                (progn
                                  (while
                                    (< k n11)
                                    (let*
                                      ((a7
                                        (y-get o7 k)))
                                      (progn
                                        (funcall f6 k a7)))
                                    (setq k
                                          (+ k 1)))))))))))))
              l))))))
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
            ((o8 l))
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
                  (hash-table-p o8)
                  (maphash f7 o8)
                  (if
                    (listp o8)
                    (let*
                      ((i22 -1))
                      (while o8
                        (let*
                          ((k
                            (if
                              (keywordp
                               (car o8))
                              (car o8)
                              (setq i22
                                    (1+ i22))))
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
                          ((k 0))
                          (progn
                            (progn
                              (while
                                (< k n12)
                                (let*
                                  ((a8
                                    (y-get o8 k)))
                                  (progn
                                    (funcall f7 k a8)))
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
            ((o9 l))
            (let*
              ((f8
                #'(lambda
                    (i9 x)
                    (progn
                      (setq x x)
                      (throw 'y-break nil)))))
              (progn
                (if
                  (hash-table-p o9)
                  (maphash f8 o9)
                  (if
                    (listp o9)
                    (let*
                      ((i23 -1))
                      (while o9
                        (let*
                          ((i9
                            (if
                              (keywordp
                               (car o9))
                              (car o9)
                              (setq i23
                                    (1+ i23))))
                           (a9
                            (if
                              (keywordp
                               (car o9))
                              (cadr o9)
                              (car o9))))
                          (funcall f8 i9 a9))
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
                          ((i9 0))
                          (progn
                            (while
                              (< i9 n13)
                              (let*
                                ((a9
                                  (y-get o9 i9)))
                                (progn
                                  (funcall f8 i9 a9)))
                              (setq i9
                                    (+ i9 1))))))))))))
          t))))
(y-setenv 'empty\? :symbol 'y-empty-p))
(progn
(defalias 'y-stash
  #'(lambda
      (args)
      (progn
        (let*
          ((l nil))
          (progn
            (progn
              (let*
                ((x3 args))
                (let*
                  ((n14
                    (y-length x3)))
                  (progn
                    (let*
                      ((i10 0))
                      (progn
                        (while
                          (< i10 n14)
                          (let*
                            ((x
                              (y-get x3 i10)))
                            (progn
                              (progn
                                (progn
                                  (let*
                                    ((k25
                                      (y-length l))
                                     (v15 x))
                                    (setq l
                                          (y-put l k25 v15))
                                    v15)
                                  nil))))
                          (setq i10
                                (+ i10 1))))))))
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
                                (k v)
                                (progn
                                  (if
                                    (y-number-p k)
                                    nil
                                    (let*
                                      ((k26 k)
                                       (v16 v))
                                      (setq p
                                            (y-put p k26 v16))
                                      v16))))))
                          (progn
                            (if
                              (hash-table-p o10)
                              (maphash f9 o10)
                              (if
                                (listp o10)
                                (let*
                                  ((i24 -1))
                                  (while o10
                                    (let*
                                      ((k
                                        (if
                                          (keywordp
                                           (car o10))
                                          (car o10)
                                          (setq i24
                                                (1+ i24))))
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
                                      ((k 0))
                                      (progn
                                        (progn
                                          (while
                                            (< k n15)
                                            (let*
                                              ((a10
                                                (y-get o10 k)))
                                              (progn
                                                (funcall f9 k a10)))
                                            (setq k
                                                  (+ k 1)))))))))))))
                      (let*
                        ((k27 :_stash)
                         (v17 t))
                        (setq p
                              (y-put p k27 v17))
                        v17)
                      (progn
                        (let*
                          ((k28
                            (y-length l))
                           (v18 p))
                          (setq l
                                (y-put l k28 v18))
                          v18)
                        nil)))))
              l))))))
(y-setenv 'stash :symbol 'y-stash))
(progn
(defalias 'y-unstash
  #'(lambda
      (args)
      (progn
        (if
          (y-none-p args)
          (make-hash-table :test 'eq)
          (let*
            ((l
              (y-last args)))
            (progn
              (progn
                (if
                  (and
                   (or
                    (listp l)
                    (y-obj-p l))
                   (y-get l :_stash))
                  (let*
                    ((args1
                      (y-almost args)))
                    (progn
                      (progn
                        (let*
                          ((o11 l))
                          (let*
                            ((f10
                              #'(lambda
                                  (k v)
                                  (progn
                                    (if
                                      (eql k :_stash)
                                      nil
                                      (let*
                                        ((k29 k)
                                         (v19 v))
                                        (setq args1
                                              (y-put args1 k29 v19))
                                        v19))))))
                            (progn
                              (if
                                (hash-table-p o11)
                                (maphash f10 o11)
                                (if
                                  (listp o11)
                                  (let*
                                    ((i25 -1))
                                    (while o11
                                      (let*
                                        ((k
                                          (if
                                            (keywordp
                                             (car o11))
                                            (car o11)
                                            (setq i25
                                                  (1+ i25))))
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
                                        ((k 0))
                                        (progn
                                          (progn
                                            (while
                                              (< k n16)
                                              (let*
                                                ((a11
                                                  (y-get o11 k)))
                                                (progn
                                                  (funcall f10 k a11)))
                                              (setq k
                                                    (+ k 1)))))))))))))
                        args1)))
                  args))))))))
(y-setenv 'unstash :symbol 'y-unstash))
(progn
(defalias 'y-destash!
  #'(lambda
      (l args1)
      (progn
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
                    (k v)
                    (progn
                      (if
                        (eql k :_stash)
                        nil
                        (let*
                          ((k30 k)
                           (v20 v))
                          (setq args1
                                (y-put args1 k30 v20))
                          v20))))))
              (progn
                (if
                  (hash-table-p o12)
                  (maphash f11 o12)
                  (if
                    (listp o12)
                    (let*
                      ((i26 -1))
                      (while o12
                        (let*
                          ((k
                            (if
                              (keywordp
                               (car o12))
                              (car o12)
                              (setq i26
                                    (1+ i26))))
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
                          ((k 0))
                          (progn
                            (progn
                              (while
                                (< k n17)
                                (let*
                                  ((a12
                                    (y-get o12 k)))
                                  (progn
                                    (funcall f11 k a12)))
                                (setq k
                                      (+ k 1)))))))))))))
          l))))
(y-setenv 'destash! :symbol 'y-destash!))
(progn
(defalias 'y-apply
  #'(lambda
      (f args)
      (progn
        (let*
          ((args1
            (y-stash args)))
          (progn
            (progn
              (funcall 'apply f args1)))))))
(y-setenv 'apply :symbol 'y-apply))
(progn
(defalias 'y-toplevel-p
  #'(lambda nil
      (progn
        (y-one-p y-environment))))
(y-setenv 'toplevel\? :symbol 'y-toplevel-p))
(progn
(defalias 'y--id
  #'(lambda
      (x)
      (progn
        (let*
          ((s
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
                    (y-get s
                           (y-edge s)))
                (progn
                  (if
                    (memq 45 s)
                    (progn
                      (let*
                        ((k31
                          (y-edge s))
                         (v21 45))
                        (setq s
                              (y-put s k31 v21))
                        v21)
                      (let*
                        ((k32
                          (y-length s))
                         (v22 112))
                        (setq s
                              (y-put s k32 v22))
                        v22))
                    (let*
                      ((k33
                        (y-edge s))
                       (v23 112))
                      (setq s
                            (y-put s k33 v23))
                      v23))))
              (intern
               (concat s))))))))
(y-setenv 'id :symbol 'y--id))
(defvar y-module nil)
(progn
(defalias 'y--module-name
  #'(lambda nil
      (progn
        (or y-module
            (let*
              ((file
                (or load-file-name
                    (buffer-file-name))))
              (progn
                (progn
                  (if file
                      (file-name-base file)
                      (buffer-name)))))))))
(y-setenv 'module-name :symbol 'y--module-name))
(progn
(defalias 'y--global-id
  #'(lambda
      (prefix name)
      (progn
        (let*
          ((s
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
                     s))
                name
                (y--id
                 (concat prefix s)))))))))
(y-setenv 'global-id :symbol 'y--global-id))
(progn
(defalias 'y--macro-function
  #'(lambda
      (k)
      (progn
        (y-getenv k :macro))))
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
        (y-getenv k :symbol))))
(y-setenv 'symbol-expansion :symbol 'y--symbol-expansion))
(progn
(defalias 'y--symbol-p
  #'(lambda
      (k)
      (progn
        (let*
          ((v
            (y--symbol-expansion k)))
          (progn
            (progn
              (and v
                   (not
                    (eq v k)))))))))
(y-setenv 'symbol\? :symbol 'y--symbol-p))
(progn
(defalias 'y--variable-p
  #'(lambda
      (k)
      (progn
        (let*
          ((i
            (y-edge y-environment)))
          (progn
            (progn
              (catch 'y-break
                (while
                  (>= i 0)
                  (let*
                    ((b
                      (y-get
                       (y-get y-environment i)
                       k)))
                    (progn
                      (progn
                        (if b
                            (throw 'y-break
                                   (y-get b :variable))
                            (setq i
                                  (1- i))))))))))))))
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
(defalias 'y--unkeywordify
  #'(lambda
      (k)
      (progn
        (if
          (keywordp k)
          (intern
           (y-clip
            (symbol-name k)
            1))
          k))))
(y-setenv 'unkeywordify :symbol 'y--unkeywordify))
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
              ((bs
                (list var rh)))
              (progn
                (progn
                  (let*
                    ((o13 lh))
                    (let*
                      ((f12
                        #'(lambda
                            (k v)
                            (progn
                              (let*
                                ((x
                                  (if
                                    (eql k :rest)
                                    (list 'cut var
                                          (y-length lh))
                                    (list 'get var
                                          (list 'quote k)))))
                                (progn
                                  (progn
                                    (if
                                      (y-is-p k)
                                      (progn
                                        (let*
                                          ((k
                                            (if
                                              (eql v t)
                                              (y--unkeywordify k)
                                              v)))
                                          (progn
                                            (progn
                                              (setq bs
                                                    (y-join bs
                                                            (y--bind k x)))))))))))))))
                      (progn
                        (if
                          (hash-table-p o13)
                          (maphash f12 o13)
                          (if
                            (listp o13)
                            (let*
                              ((i27 -1))
                              (while o13
                                (let*
                                  ((k
                                    (if
                                      (keywordp
                                       (car o13))
                                      (car o13)
                                      (setq i27
                                            (1+ i27))))
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
                                  ((k 0))
                                  (progn
                                    (progn
                                      (while
                                        (< k n18)
                                        (let*
                                          ((a13
                                            (y-get o13 k)))
                                          (progn
                                            (funcall f12 k a13)))
                                        (setq k
                                              (+ k 1)))))))))))))
                  bs))))))))
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
            ((args1 nil))
            (progn
              (let*
                ((bs nil))
                (progn
                  (progn
                    (mapc
                     #'(lambda
                         (x)
                         (progn
                           (if
                             (atom x)
                             (setq args1
                                   (nconc args1
                                          (list x)))
                             (let*
                               ((id1
                                 (y-unique 'id1)))
                               (setq args1
                                     (nconc args1
                                            (list id1)))
                               (setq bs
                                     (y-join bs
                                             (list x id1)))))))
                     args)
                    (list args1
                          (if
                            (null bs)
                            (cons 'progn body)
                            (cons 'let
                                  (cons bs body)))))))))))))
(y-setenv 'bind* :symbol 'y--bind*))
(progn
(defalias 'y-macroexpand
  #'(lambda
      (form)
      (progn
        (let*
          ((s
            (y--symbol-expansion form)))
          (progn
            (progn
              (if s
                  (y-macroexpand s)
                  (if
                    (atom form)
                    form
                    (let*
                      ((x
                        (y-macroexpand
                         (y-hd form))))
                      (progn
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
                                 (funcall 'apply
                                          (y--macro-function x)
                                          (y-tl form)))
                                (cons x
                                      (mapcar 'y-macroexpand
                                              (y-tl form)))))))))))))))))
(y-setenv 'macroexpand :symbol 'y-macroexpand))
(progn
(defalias 'y-expand
  #'(lambda
      (form)
      (progn
        (macroexpand-all
         (y-macroexpand form)))))
(y-setenv 'expand :symbol 'y-expand))
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
        ((var1 id10))
        (let*
          ((a
            (y-get var1 '0)))
          (progn
            (let*
              ((b
                (y-get var1 '1)))
              (progn
                (let*
                  ((c
                    (y-cut var1 2)))
                  (progn
                    (progn
                      (if
                        (y-some-p c)
                        (list
                          (cons 'if
                                (cons a
                                      (cons b
                                            (y--expand-if c)))))
                        (if
                          (y-is-p b)
                          (list
                            (list 'if a b))
                          (if
                            (y-is-p a)
                            (list a))))))))))))))
(y-setenv 'expand-if :symbol 'y--expand-if))
(y-setenv 'if\? :macro
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
                  ((var
                    (y--global-id
                     (concat
                      (y--module-name)
                      "--")
                     name)))
                  (progn
                    (progn
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
                                  (list 'quote var)))))))))
(y-setenv 'define-global :macro
          #'(lambda
              (name x &rest body)
              (progn
                (let*
                  ((var
                    (y--global-id
                     (concat
                      (y--module-name)
                      "-")
                     name)))
                  (progn
                    (progn
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
                                  (list 'quote var)))))))))
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
          (y-apply 'vector
                   (append y-environment
                           (list
                             (make-hash-table :test 'eq)))))
    (let*
      ((x8
        (progn
          (y-map
           #'(lambda
               (m)
               (progn
                 (y-macroexpand
                  (cons 'define-macro m))))
           definitions)
          (cons 'progn
                (y-macroexpand body)))))
      (progn
        (setq y-environment
              (y-apply 'vector
                       (y-almost y-environment)))
        x8))))))
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
                          (y-apply 'vector
                                   (append y-environment
                                           (list
                                             (make-hash-table :test 'eq)))))
                    (let*
                      ((x10
                        (progn
                          (mapc
                           #'(lambda
                               (x)
                               (progn
                                 (y-macroexpand
                                  (cons 'define-symbol x))))
                           (y-pair expansions))
                          (cons 'progn
                                (y-macroexpand body)))))
                      (progn
                        (setq y-environment
                              (y-apply 'vector
                                       (y-almost y-environment)))
                        x10)))))))
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
                      ((var9 bs))
                      (let*
                        ((lh1
                          (y-get var9 '0)))
                        (progn
                          (let*
                            ((rh1
                              (y-get var9 '1)))
                            (progn
                              (let*
                                ((bs21
                                  (y-cut var9 2)))
                                (progn
                                  (let*
                                    ((var10
                                      (y--bind lh1 rh1)))
                                    (let*
                                      ((var11
                                        (y-get var10 '0)))
                                      (progn
                                        (let*
                                          ((val1
                                            (y-get var10 '1)))
                                          (progn
                                            (let*
                                              ((bs11
                                                (y-cut var10 2)))
                                              (progn
                                                (progn
                                                  (let*
                                                    ((renames1 nil))
                                                    (progn
                                                      (progn
                                                        (if
                                                          (or
                                                           (y--bound-p var11)
                                                           (y-toplevel-p))
                                                          (let*
                                                            ((var11
                                                              (y-unique var11)))
                                                            (progn
                                                              (progn
                                                                (setq renames1
                                                                      (list var11 var11))
                                                                (setq var11 var11))))
                                                          (y-setenv var11 :variable t))
                                                        (let*
                                                          ((form1
                                                            (cons 'let
                                                                  (cons
                                                                   (y-join bs11 bs21)
                                                                   body))))
                                                          (progn
                                                            (progn
                                                              (if
                                                                (y-none-p renames1)
                                                                nil
                                                                (setq form1
                                                                      (list 'let-symbol renames1 form1)))
                                                              (list 'let*
                                                                    (list
                                                                      (list var11 val1))
                                                                    (y-macroexpand form1))))))))))))))))))))))))))))
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
                    ((var13
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
                      ((k13
                        (y-get var13 '0)))
                      (progn
                        (let*
                          ((v3
                            (y-get var13 '1)))
                          (progn
                            (progn
                              (list 'let
                                    (list o l f
                                          (cons 'fn
                                                (cons
                                                 (list k13 v3)
                                                 body)))
                                    (list 'if\?
                                          (list 'hash-table-p o)
                                          (list 'maphash f o)
                                          (list 'listp o)
                                          (list 'y-%for o k13 a
                                                (list 'funcall f k13 a))
                                          (list 'let n
                                                (list '\# o)
                                                (list 'for k13 n
                                                      (list 'let a
                                                            (list 'at o k13)
                                                            (list 'funcall f k13 a)))))))))))))))))
  (provide 'y))
