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
                            ((i0 -1))
                          (while h
                            (let
                                ((var
                                  (if
                                      (keywordp
                                       (car h))
                                      (car h)
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
        (progn :autoload-end
               '(closure
                 (t)
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
    'byte-compile-inline-expand)
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
                            ((i1 -1))
                          (while h
                            (let
                                ((var
                                  (if
                                      (keywordp
                                       (car h))
                                      (car h)
                                    (setq i1
                                          (1+ i1))))
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
        (let
            ((n -1))
          (let
              ((i2 -1))
            (while h
              (let
                  ((k
                    (if
                        (keywordp
                         (car h))
                        (car h)
                      (setq i2
                            (1+ i2))))
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
          (+ n 1))))
  (defvar y-environment
    (list
     (make-hash-table :test 'equal)))
  (defalias 'y-setenv
    #'(lambda
        (k &rest keys)
        (let*
            ((i
              (if
                  (y-get keys :toplevel)
                  0
                (-
                 (y-length y-environment)
                 1)))
             (frame
              (y-get y-environment i))
             (entry
              (y-get frame k)))
          (let
              ((i3 -1))
            (while keys
              (let
                  ((k
                    (if
                        (keywordp
                         (car keys))
                        (car keys)
                      (setq i3
                            (1+ i3))))
                   (v
                    (if
                        (keywordp
                         (car keys))
                        (cadr keys)
                      (car keys))))
                (let*
                    ((i k)
                     (v v))
                  (progn
                    (setq entry
                          (y-put entry i v))
                    v)))
              (setq keys
                    (if
                        (keywordp
                         (car keys))
                        (cddr keys)
                      (cdr keys))))
            nil)
          (let*
              ((i k)
               (v entry))
            (progn
              (setq frame
                    (y-put frame i v))
              v)))))
  (defalias 'y-getenv
    #'(lambda
        (k &optional p)
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
                        (1- i)))))))))
  (defalias 'y-symbol-expansion
    #'(lambda
        (k)
        (y-getenv k :symbol)))
  (defalias 'y-symbol-p
    #'(lambda
        (k)
        (y-symbol-expansion k)))
  (defalias 'y-macro-function
    #'(lambda
        (k)
        (y-getenv k :macro)))
  (defalias 'y-macro-p
    #'(lambda
        (k)
        (y-macro-function k)))
  (defalias 'y-macroexpand
    #'(lambda
        (form)
        (if
            (y-symbol-p form)
            (y-macroexpand
             (y-symbol-expansion form))
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
                    (y-quasiexpand
                     (y-get form 1))
                  (if
                      (y-macro-p x)
                      (y-macroexpand
                       (apply
                        (y-macro-function x)
                        (cdr form)))
                    (mapcar 'y-macroexpand form)))))))))
  (defalias 'y-expand
    #'(lambda
        (form)
        (macroexpand-all
         (y-macroexpand form))))
  (defalias 'y-eval
    #'(lambda
        (form)
        (eval
         (y-expand form)
         t)))
  (defalias 'y-quasiexpand
    #'(lambda
        (form)
        (list '\`
              (y-quasiexpand-1 form 1))))
  (defalias 'y-quasiexpand-1
    #'(lambda
        (x depth)
        (cond
         ((= depth 0)
          (y-macroexpand x))
         ((and
           (consp x)
           (eq
            (car x)
            '\,))
          (list '\,
                (y-quasiexpand-1
                 (cadr x)
                 (- depth 1))))
         ((and
           (consp x)
           (eq
            (car x)
            '\,@)
           (= depth 1))
          (list '\,@
                (y-quasiexpand-1
                 (cadr x)
                 (- depth 1))))
         ((and
           (consp x)
           (eq
            (car x)
            '\`))
          (list '\`
                (y-quasiexpand-1
                 (cadr x)
                 (+ depth 1))))
         ((consp x)
          (mapcar
           #'(lambda
               (form)
               (y-quasiexpand-1 form depth))
           x))
         (t x))))
  (defalias 'y-do
    (cons 'macro
          #'(lambda
              (&rest body)
              (macroexpand-all
               (y-macroexpand
                (cons 'progn body))))))
  (progn
    (y-setenv 'fn :macro
              #'(lambda
                  (args &rest body)
                  (progn
                    (list 'lambda
                          (if
                              (atom args)
                              (list '&rest args)
                            args)
                          (cons 'y-do body)))))
    (y-setenv 'define-macro :macro
              #'(lambda
                  (name args &rest body)
                  (progn
                    (let
                        ((form
                          (list 'y-setenv
                                (list 'quote name)
                                ':macro
                                (cons 'fn
                                      (cons args body)))))
                      (y-eval form)
                      form)))))
  (provide 'y))
