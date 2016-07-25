;;; -*- lexical-binding: t -*-
(progn
  (set
   (make-local-variable 'lexical-binding)
   t)
  (require 'cl)
  (progn
    (or lexical-binding
        (signal 'cl-assertion-failed
                (list 'lexical-binding)))
    nil)
  (let
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
              (cons 'let
                    (cons
                     (mapcar
                      #'(lambda
                          (x)
                          (list x
                                (list 'y-unique
                                      (list 'quote x))))
                      vars)
                     body)))))
  (defalias 'y-next
    #'(lambda
        (h)
        (if
          (keywordp
           (car h))
          (cddr h)
          (cdr h))))
  (defalias 'y-%for
    (cons 'macro
          #'(lambda
              (h k v &rest body)
              (let
                ((i
                  (y-unique 'i)))
                (cons 'let
                      (cons
                       (list
                         (cons i
                               '(-1)))
                       (cons
                        (list 'while h
                              (cons 'let
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
            (let
              ((i15 -1))
              (while h
                (let
                  ((var
                    (if
                      (keywordp
                       (car h))
                      (car h)
                      (setq i15
                            (1+ i15))))
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
(progn :autoload-end
       '(lambda
          (f h k)
          (gv-letplace
              (getter setter)
              h
            (macroexp-let2 nil i k
              (funcall f
                       `(y-get ,getter ,i)
                       (lambda
                         (val)
                         (macroexp-let2 nil v val
                           `(progn ,(funcall setter
                                             `(y-put ,getter ,i ,v))
                                   ,v)))))))))
  (defalias 'y-put
    #'(lambda
        (h k &optional v)
        (if
          (hash-table-p h)
          (progn
            (puthash k v h)
            h)
          (let
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
                    (let
                      ((i16 -1))
                      (while h
                        (let
                          ((var
                            (if
                              (keywordp
                               (car h))
                              (car h)
                              (setq i16
                                    (1+ i16))))
                           (val
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
  (defalias 'y-length
    #'(lambda
        (h)
        (if
          (listp h)
          (let
            ((n -1))
            (let
              ((i17 -1))
              (while h
                (let
                  ((k
                    (if
                      (keywordp
                       (car h))
                      (car h)
                      (setq i17
                            (1+ i17))))
                   (v
                    (if
                      (keywordp
                       (car h))
                      (cadr h)
                      (car h))))
                  (if
                    (integerp k)
                    (progn
                      (setq n
                            (max n k)))))
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
            (let
              ((n -1))
              (maphash
               #'(lambda
                   (k v)
                   (if
                     (integerp k)
                     (setq n
                           (max n k))))
               h)
              (+ n 1))
            (length h)))))
  (progn
(defvar y-environment
  (list
    (make-hash-table :test 'equal)))
(progn
(defalias 'y-setenv
  #'(lambda
      (k &rest ks)
      (progn
        (let*
          ((i
            (if
              (y-get ks :toplevel)
              0
              (-
               (y-length y-environment)
               1)))
           (frame
            (y-get y-environment i))
           (entry
            (y-get frame k)))
          (let
            ((o0 ks)
             (f0
              #'(lambda
                  (k v)
                  (let*
                    ((i k)
                     (v v))
                    (progn
                      (setq entry
                            (y-put entry i v))
                      v)))))
            (if
              (hash-table-p o0)
              (maphash f0 o0)
              (if
                (listp o0)
                (let
                  ((i18 -1))
                  (while o0
                    (let
                      ((k
                        (if
                          (keywordp
                           (car o0))
                          (car o0)
                          (setq i18
                                (1+ i18))))
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
                (let
                  ((n0
                    (y-length o0)))
                  (let
                    ((k 0))
                    (while
                      (< k n0)
                      (let
                        ((v
                          (y-get o0 k)))
                        (funcall f0 k v))
                      (setq k
                            (+ k 1))))))))
          (let*
            ((i k)
             (v entry))
            (progn
              (setq frame
                    (y-put frame i v))
              v))))))
(y-setenv 'setenv :symbol 'y-setenv))
(progn
(defalias 'y-getenv
  #'(lambda
      (k &optional p)
      (progn
        (let
          ((i
            (-
             (y-length y-environment)
             1)))
          (catch 'y-break
            (while
              (>= i 0)
              (let
                ((b
                  (y-get
                   (y-get y-environment 0)
                   k)))
                (if b
                    (throw 'y-break
                           (if p
                               (y-get b p)
                               b))
                    (setq i
                          (1- i))))))))))
(y-setenv 'getenv :symbol 'y-getenv))
(y-setenv 'unique :symbol 'y-unique)
(y-setenv 'let-unique :symbol 'y-let-unique)
(y-setenv 'at :symbol 'get)
(y-setenv 'get :symbol 'y-get)
(y-setenv 'set :symbol 'y-set)
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
         (y-length x)
         0))))
(y-setenv 'none\? :symbol 'y-none-p))
(progn
(defalias 'y-some-p
  #'(lambda
      (x)
      (progn
        (>
         (y-length x)
         0))))
(y-setenv 'some\? :symbol 'y-some-p))
(progn
(defalias 'y-one-p
  #'(lambda
      (x)
      (progn
        (eql
         (y-length x)
         1))))
(y-setenv 'one\? :symbol 'y-one-p))
(progn
(defalias 'y-two-p
  #'(lambda
      (x)
      (progn
        (eql
         (y-length x)
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
(defalias 'y-string-p
  #'(lambda
      (x)
      (progn
        (stringp x))))
(y-setenv 'string\? :symbol 'y-string-p))
(progn
(defalias 'y-symbol-p
  #'(lambda
      (x)
      (progn
        (symbolp x))))
(y-setenv 'symbol\? :symbol 'y-symbol-p))
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
(defalias 'y-function-p
  #'(lambda
      (x)
      (progn
        (and
         (not
          (y-symbol-p x))
         (functionp x)))))
(y-setenv 'function\? :symbol 'y-function-p))
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
                '(make-hash-table :test 'equal))))
(progn
(defalias 'y-atom-p
  #'(lambda
      (x)
      (progn
        (or
         (y-nil-p x)
         (y-symbol-p x)
         (y-string-p x)
         (y-number-p x)))))
(y-setenv 'atom\? :symbol 'y-atom-p))
(progn
(defalias 'y-clamp
  #'(lambda
      (n a b)
      (progn
        (if
          (< n a)
          a
          (if
            (> n b)
            b n)))))
(y-setenv 'clamp :symbol 'y-clamp))
(progn
(defalias 'y-clip
  #'(lambda
      (s from &optional upto)
      (progn
        (let*
          ((n
            (length s))
           (i
            (y-clamp from 0 n))
           (j
            (if upto
                (y-clamp upto i n))))
          (substring s i j)))))
(y-setenv 'clip :symbol 'y-clip))
(progn
(defalias 'y-cut
  #'(lambda
      (x &optional from upto)
      (progn
        (let
          ((l
            (if
              (y-obj-p x)
              (make-hash-table :test 'equal)
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
                ((i j)
                 (v
                  (y-get x i)))
                (progn
                  (setq l
                        (y-put l i v))
                  v))
              (setq i
                    (+ i 1))
              (setq j
                    (+ j 1)))
            (let
              ((o1 x)
               (f1
                #'(lambda
                    (k v)
                    (if
                      (y-number-p k)
                      nil
                      (let*
                        ((i k)
                         (v v))
                        (progn
                          (setq l
                                (y-put l i v))
                          v))))))
              (if
                (hash-table-p o1)
                (maphash f1 o1)
                (if
                  (listp o1)
                  (let
                    ((i19 -1))
                    (while o1
                      (let
                        ((k
                          (if
                            (keywordp
                             (car o1))
                            (car o1)
                            (setq i19
                                  (1+ i19))))
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
                  (let
                    ((n1
                      (y-length o1)))
                    (let
                      ((k 0))
                      (while
                        (< k n1)
                        (let
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
      (progn
        (let
          ((l
            (if
              (y-obj-p x)
              (make-hash-table :test 'equal)
              nil)))
          (let
            ((o2 x)
             (f2
              #'(lambda
                  (k v)
                  (if
                    (y-number-p k)
                    nil
                    (let*
                      ((i k)
                       (v v))
                      (progn
                        (setq l
                              (y-put l i v))
                        v))))))
            (if
              (hash-table-p o2)
              (maphash f2 o2)
              (if
                (listp o2)
                (let
                  ((i20 -1))
                  (while o2
                    (let
                      ((k
                        (if
                          (keywordp
                           (car o2))
                          (car o2)
                          (setq i20
                                (1+ i20))))
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
                (let
                  ((n2
                    (y-length o2)))
                  (let
                    ((k 0))
                    (while
                      (< k n2)
                      (let
                        ((v
                          (y-get o2 k)))
                        (funcall f2 k v))
                      (setq k
                            (+ k 1))))))))
          l))))
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
        (let
          ((l1
            (y-keys l)))
          (let
            ((i
              (y-edge l)))
            (while
              (>= i 0)
              (progn
                (let*
                  ((i
                    (y-length l1))
                   (v
                    (y-get l i)))
                  (progn
                    (setq l1
                          (y-put l1 i v))
                    v))
                nil)
              (setq i
                    (- i 1))))
          l1))))
(y-setenv 'reverse :symbol 'y-reverse))
(progn
(defalias 'y-reduce
  #'(lambda
      (f x)
      (progn
        (if
          (y-none-p x)
          nil
          (y-one-p x)
          (y-hd x)
          (funcall f
                   (y-hd x)
                   (y-reduce f
                             (y-tl x)))))))
(y-setenv 'reduce :symbol 'y-reduce))
(progn
(defalias 'y-join
  #'(lambda
      (&rest ls)
      (progn
        (if
          (y-two-p ls)
          (let
            ((a
              (y-get ls 0))
             (b
              (y-get ls 1)))
            (if
              (and a b)
              (let
                ((c
                  (if
                    (y-obj-p a)
                    (make-hash-table :test 'equal)
                    nil))
                 (o
                  (y-length a)))
                (let
                  ((o3 a)
                   (f3
                    #'(lambda
                        (k v)
                        (let*
                          ((i k)
                           (v v))
                          (progn
                            (setq c
                                  (y-put c i v))
                            v)))))
                  (if
                    (hash-table-p o3)
                    (maphash f3 o3)
                    (if
                      (listp o3)
                      (let
                        ((i21 -1))
                        (while o3
                          (let
                            ((k
                              (if
                                (keywordp
                                 (car o3))
                                (car o3)
                                (setq i21
                                      (1+ i21))))
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
                      (let
                        ((n3
                          (y-length o3)))
                        (let
                          ((k 0))
                          (while
                            (< k n3)
                            (let
                              ((v
                                (y-get o3 k)))
                              (funcall f3 k v))
                            (setq k
                                  (+ k 1))))))))
                (let
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
                          ((i k)
                           (v v))
                          (progn
                            (setq c
                                  (y-put c i v))
                            v)))))
                  (if
                    (hash-table-p o4)
                    (maphash f4 o4)
                    (if
                      (listp o4)
                      (let
                        ((i22 -1))
                        (while o4
                          (let
                            ((k
                              (if
                                (keywordp
                                 (car o4))
                                (car o4)
                                (setq i22
                                      (1+ i22))))
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
                      (let
                        ((n4
                          (y-length o4)))
                        (let
                          ((k 0))
                          (while
                            (< k n4)
                            (let
                              ((v
                                (y-get o4 k)))
                              (funcall f4 k v))
                            (setq k
                                  (+ k 1))))))))
                c)
              (or a b nil)))
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
          (let
            ((o5 l)
             (f5
              #'(lambda
                  (i5 x)
                  (let
                    ((y
                      (funcall f x)))
                    (if y
                        (throw 'y-break y))))))
            (if
              (hash-table-p o5)
              (maphash f5 o5)
              (if
                (listp o5)
                (let
                  ((i23 -1))
                  (while o5
                    (let
                      ((i5
                        (if
                          (keywordp
                           (car o5))
                          (car o5)
                          (setq i23
                                (1+ i23))))
                       (x
                        (if
                          (keywordp
                           (car o5))
                          (cadr o5)
                          (car o5))))
                      (funcall f5 i5 x))
                    (setq o5
                          (if
                            (keywordp
                             (car o5))
                            (cddr o5)
                            (cdr o5))))
                  nil)
                (let
                  ((n5
                    (y-length o5)))
                  (let
                    ((i5 0))
                    (while
                      (< i5 n5)
                      (let
                        ((x
                          (y-get o5 i5)))
                        (funcall f5 i5 x))
                      (setq i5
                            (+ i5 1))))))))))))
(y-setenv 'find :symbol 'y-find))
(progn
(defalias 'y-first
  #'(lambda
      (f l)
      (progn
        (catch 'y-break
          (let*
            ((x0 l)
             (n6
              (y-length x0)))
            (let
              ((i6 0))
              (while
                (< i6 n6)
                (let
                  ((x
                    (y-get x0 i6)))
                  (let
                    ((y
                      (funcall f x)))
                    (if y
                        (throw 'y-break y))))
                (setq i6
                      (+ i6 1)))))))))
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
        (let
          ((l1
            (if
              (y-obj-p l)
              (make-hash-table :test 'equal)
              nil)))
          (let
            ((n
              (y-length l)))
            (let
              ((i 0))
              (while
                (< i n)
                (progn
                  (let*
                    ((i
                      (y-length l1))
                     (v
                      (list
                        (y-get l i)
                        (y-get l
                               (+ i 1)))))
                    (progn
                      (setq l1
                            (y-put l1 i v))
                      v))
                  nil)
                (setq i
                      (+ i 1))
                (setq i
                      (+ i 1)))))
          l1))))
(y-setenv 'pair :symbol 'y-pair))
(progn
(defalias 'y-map
  #'(lambda
      (f x)
      (progn
        (let
          ((l
            (if
              (y-obj-p x)
              (make-hash-table :test 'equal)
              nil)))
          (let*
            ((x1 x)
             (n7
              (y-length x1)))
            (let
              ((i7 0))
              (while
                (< i7 n7)
                (let
                  ((v
                    (y-get x1 i7)))
                  (let
                    ((y
                      (funcall f v)))
                    (if
                      (y-is-p y)
                      (progn
                        (let*
                          ((i
                            (y-length l))
                           (v y))
                          (progn
                            (setq l
                                  (y-put l i v))
                            v))
                        nil))))
                (setq i7
                      (+ i7 1)))))
          (let
            ((o6 x)
             (f6
              #'(lambda
                  (k v)
                  (if
                    (y-number-p k)
                    nil
                    (let
                      ((y
                        (funcall f v)))
                      (if
                        (y-is-p y)
                        (progn
                          (let*
                            ((i k)
                             (v y))
                            (progn
                              (setq l
                                    (y-put l i v))
                              v)))))))))
            (if
              (hash-table-p o6)
              (maphash f6 o6)
              (if
                (listp o6)
                (let
                  ((i24 -1))
                  (while o6
                    (let
                      ((k
                        (if
                          (keywordp
                           (car o6))
                          (car o6)
                          (setq i24
                                (1+ i24))))
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
                (let
                  ((n8
                    (y-length o6)))
                  (let
                    ((k 0))
                    (while
                      (< k n8)
                      (let
                        ((v
                          (y-get o6 k)))
                        (funcall f6 k v))
                      (setq k
                            (+ k 1))))))))
          l))))
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
          (let
            ((o7 l)
             (f7
              #'(lambda
                  (k v)
                  (if
                    (y-number-p k)
                    nil
                    (throw 'y-break t)))))
            (if
              (hash-table-p o7)
              (maphash f7 o7)
              (if
                (listp o7)
                (let
                  ((i25 -1))
                  (while o7
                    (let
                      ((k
                        (if
                          (keywordp
                           (car o7))
                          (car o7)
                          (setq i25
                                (1+ i25))))
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
                (let
                  ((n9
                    (y-length o7)))
                  (let
                    ((k 0))
                    (while
                      (< k n9)
                      (let
                        ((v
                          (y-get o7 k)))
                        (funcall f7 k v))
                      (setq k
                            (+ k 1))))))))
          nil))))
(y-setenv 'keys\? :symbol 'y-keys-p))
(progn
(defalias 'y-empty-p
  #'(lambda
      (l)
      (progn
        (catch 'y-break
          (let
            ((o8 l)
             (f8
              #'(lambda
                  (i10 x)
                  (throw 'y-break nil))))
            (if
              (hash-table-p o8)
              (maphash f8 o8)
              (if
                (listp o8)
                (let
                  ((i26 -1))
                  (while o8
                    (let
                      ((i10
                        (if
                          (keywordp
                           (car o8))
                          (car o8)
                          (setq i26
                                (1+ i26))))
                       (x
                        (if
                          (keywordp
                           (car o8))
                          (cadr o8)
                          (car o8))))
                      (funcall f8 i10 x))
                    (setq o8
                          (if
                            (keywordp
                             (car o8))
                            (cddr o8)
                            (cdr o8))))
                  nil)
                (let
                  ((n10
                    (y-length o8)))
                  (let
                    ((i10 0))
                    (while
                      (< i10 n10)
                      (let
                        ((x
                          (y-get o8 i10)))
                        (funcall f8 i10 x))
                      (setq i10
                            (+ i10 1))))))))
          t))))
(y-setenv 'empty\? :symbol 'y-empty-p))
(progn
(defalias 'y-stash
  #'(lambda
      (args)
      (progn
        (let
          ((l nil))
          (let
            ((o9 args)
             (f9
              #'(lambda
                  (k v)
                  (if
                    (y-number-p k)
                    (progn
                      (progn
                        (let*
                          ((i
                            (y-length l))
                           (v v))
                          (progn
                            (setq l
                                  (y-put l i v))
                            v))
                        nil))))))
            (if
              (hash-table-p o9)
              (maphash f9 o9)
              (if
                (listp o9)
                (let
                  ((i27 -1))
                  (while o9
                    (let
                      ((k
                        (if
                          (keywordp
                           (car o9))
                          (car o9)
                          (setq i27
                                (1+ i27))))
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
                (let
                  ((n11
                    (y-length o9)))
                  (let
                    ((k 0))
                    (while
                      (< k n11)
                      (let
                        ((v
                          (y-get o9 k)))
                        (funcall f9 k v))
                      (setq k
                            (+ k 1))))))))
          (if
            (y-keys-p args)
            (progn
              (let
                ((p
                  (if
                    (y-obj-p args)
                    (make-hash-table :test 'equal)
                    nil)))
                (let
                  ((o10 args)
                   (f10
                    #'(lambda
                        (k v)
                        (if
                          (y-number-p k)
                          nil
                          (let*
                            ((i k)
                             (v v))
                            (progn
                              (setq p
                                    (y-put p i v))
                              v))))))
                  (if
                    (hash-table-p o10)
                    (maphash f10 o10)
                    (if
                      (listp o10)
                      (let
                        ((i28 -1))
                        (while o10
                          (let
                            ((k
                              (if
                                (keywordp
                                 (car o10))
                                (car o10)
                                (setq i28
                                      (1+ i28))))
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
                      (let
                        ((n12
                          (y-length o10)))
                        (let
                          ((k 0))
                          (while
                            (< k n12)
                            (let
                              ((v
                                (y-get o10 k)))
                              (funcall f10 k v))
                            (setq k
                                  (+ k 1))))))))
                (progn
                  (setq p
                        (y-put p :_stash t))
                  t)
                (progn
                  (let*
                    ((i
                      (y-length l))
                     (v p))
                    (progn
                      (setq l
                            (y-put l i v))
                      v))
                  nil))))
          l))))
(y-setenv 'stash :symbol 'y-stash))
(progn
(defalias 'y-unstash
  #'(lambda
      (args)
      (progn
        (if
          (y-none-p args)
          nil
          (let
            ((l
              (y-last args)))
            (if
              (y-get l :_stash)
              (let
                ((args1
                  (y-almost args)))
                (let
                  ((o11 l)
                   (f11
                    #'(lambda
                        (k v)
                        (if
                          (eql k :_stash)
                          nil
                          (let*
                            ((i k)
                             (v v))
                            (progn
                              (setq args1
                                    (y-put args1 i v))
                              v))))))
                  (if
                    (hash-table-p o11)
                    (maphash f11 o11)
                    (if
                      (listp o11)
                      (let
                        ((i29 -1))
                        (while o11
                          (let
                            ((k
                              (if
                                (keywordp
                                 (car o11))
                                (car o11)
                                (setq i29
                                      (1+ i29))))
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
                      (let
                        ((n13
                          (y-length o11)))
                        (let
                          ((k 0))
                          (while
                            (< k n13)
                            (let
                              ((v
                                (y-get o11 k)))
                              (funcall f11 k v))
                            (setq k
                                  (+ k 1))))))))
                args1)
              args))))))
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
          (let
            ((o12 l)
             (f12
              #'(lambda
                  (k v)
                  (if
                    (eql k :_stash)
                    nil
                    (let*
                      ((i k)
                       (v v))
                      (progn
                        (setq args1
                              (y-put args1 i v))
                        v))))))
            (if
              (hash-table-p o12)
              (maphash f12 o12)
              (if
                (listp o12)
                (let
                  ((i30 -1))
                  (while o12
                    (let
                      ((k
                        (if
                          (keywordp
                           (car o12))
                          (car o12)
                          (setq i30
                                (1+ i30))))
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
                (let
                  ((n14
                    (y-length o12)))
                  (let
                    ((k 0))
                    (while
                      (< k n14)
                      (let
                        ((v
                          (y-get o12 k)))
                        (funcall f12 k v))
                      (setq k
                            (+ k 1))))))))
          l))))
(y-setenv 'destash! :symbol 'y-destash!))
(progn
(defalias 'y-apply
  #'(lambda
      (f args)
      (progn
        (let
          ((args1
            (y-stash args)))
          (funcall 'apply f args1)))))
(y-setenv 'apply :symbol 'y-apply))
(defalias 'y--id
  #'(lambda
      (x)
      (progn
        (let
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
                    ((i
                      (y-edge s)))
                    (progn
                      (setq s
                            (y-put s i 45))
                      45))
                  (let*
                    ((i
                      (y-length s)))
                    (progn
                      (setq s
                            (y-put s i 112))
                      112)))
                (let*
                  ((i
                    (y-edge s)))
                  (progn
                    (setq s
                          (y-put s i 112))
                    112)))))
          (intern
           (concat s))))))
(defvar y-module nil)
(defalias 'y--module-name
  #'(lambda nil
      (progn
        (or y-module
            (let
              ((file
                (or load-file-name
                    (buffer-file-name))))
              (if file
                  (file-name-base file)
                  (buffer-name)))))))
(defalias 'y--global-id
  #'(lambda
      (prefix name)
      (progn
        (let
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
             (concat prefix s)))))))
(defalias 'y--symbol-expansion
  #'(lambda
      (k)
      (progn
        (y-getenv k :symbol))))
(defalias 'y--symbol-p
  #'(lambda
      (k)
      (progn
        (let
          ((v
            (y--symbol-expansion k)))
          (and v
               (not
                (eq v k)))))))
(defalias 'y--macro-function
  #'(lambda
      (k)
      (progn
        (y-getenv k :macro))))
(defalias 'y--macro-p
  #'(lambda
      (k)
      (progn
        (y--macro-function k))))
(defalias 'y--quasiexpand-1
  #'(lambda
      (x depth)
      (progn
        (cond
          ((eql depth 0)
           (y-macroexpand x))
          ((and
            (consp x)
            (eq
             (car x)
             '\,))
           (list '\,
                 (y--quasiexpand-1
                  (cadr x)
                  (- depth 1))))
          ((and
            (consp x)
            (eq
             (car x)
             '\,@)
            (eql depth 1))
           (list '\,@
                 (y--quasiexpand-1
                  (cadr x)
                  (- depth 1))))
          ((and
            (consp x)
            (eq
             (car x)
             '\`))
           (list '\`
                 (y--quasiexpand-1
                  (cadr x)
                  (+ depth 1))))
          ((consp x)
           (mapcar
            #'(lambda
                (form)
                (y--quasiexpand-1 form depth))
            x))
          (t x)))))
(defalias 'y--quasiexpand
  #'(lambda
      (form)
      (progn
        (list '\`
              (y--quasiexpand-1 form 1)))))
(progn
(defalias 'y-macroexpand
  #'(lambda
      (form)
      (progn
        (if
          (y--symbol-p form)
          (y-macroexpand
           (y--symbol-expansion form))
          (if
            (atom form)
            form
            (let
              ((x
                (y-macroexpand
                 (y-get form 0))))
              (if
                (eq x 'quote)
                form
                (if
                  (eq x '\`)
                  (y--quasiexpand
                   (y-get form 1))
                  (if
                    (y--macro-p x)
                    (y-macroexpand
                     (y-apply
                      (y--macro-function x)
                      (cdr form)))
                    (mapcar 'y-macroexpand form))))))))))
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
                 (y-expand form)
                 t))))
(y-setenv 'eval :symbol 'y-eval))
(y-setenv 'with :macro
          #'(lambda
              (x v &rest body)
              (progn
                (cons 'let
                      (cons
                       (list
                         (list x v))
                       (append body
                               (list x)))))))
(y-setenv 'fn :macro
          #'(lambda
              (args &rest body)
              (progn
                (list 'lambda
                      (if
                        (not
                         (listp args))
                        (list '&rest args)
                        args)
                      (cons 'y-do body)))))
(y-setenv 'define-macro :macro
          #'(lambda
              (name args &rest body)
              (progn
                (let
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
                (let
                  ((var
                    (y--global-id
                     (concat
                      (y--module-name)
                      "--")
                     name)))
                  (y-setenv name :symbol var)
                  (y-setenv var :variable t)
                  (list 'defalias
                        (list 'quote var)
                        (cons 'fn
                              (cons x body)))))))
(y-setenv 'define-global :macro
          #'(lambda
              (name x &rest body)
              (progn
                (let
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
                              (list 'quote var)))))))
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
                (list 'let
                      (list
                        (cons i
                              '(0)))
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
                (let
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
                              (cons 'let
                                    (cons
                                     (list
                                       (list v
                                             (list 'at x i)))
                                     body))))))))
(y-setenv 'each :macro
          #'(lambda
              (x l &rest body)
              (progn
                (let
                  ((o
                    (y-unique 'o))
                   (n
                    (y-unique 'n))
                   (i
                    (y-unique 'i))
                   (f
                    (y-unique 'f)))
                  (progn
                    (let*
                      ((--cl-rest--
                        (if
                          (y-atom-p x)
                          (list i x)
                          (if
                            (>
                             (y-length x)
                             1)
                            x
                            (list i
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
                      (list 'let
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
                                        (list 'let
                                              (list
                                                (list n
                                                      (list '\# o)))
                                              (list 'for k n
                                                    (list 'let
                                                          (list
                                                            (list v
                                                                  (list 'at o k)))
                                                          (list 'funcall f k v))))))))))))))
  (defalias 'y-do
    (cons 'macro
          #'(lambda
              (&rest body)
              (macroexpand-all
               (y-macroexpand
                (cons 'progn body))))))
  (provide 'y))
