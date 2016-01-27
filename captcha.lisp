(in-package :cl-user)

(defpackage #:text-captcha
  (:use :cl)
  (:export :generate :load-db))

(in-package :text-captcha)

(defparameter *rand-limit* 10)
(defparameter *default-db-path* (asdf:system-relative-pathname 'text-captcha "captcha.db"))

(defun load-db (&optional (filename *default-db-path*))
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *captcha-db* (read in)))))

(defparameter *captcha-db* (load-db))

(proclaim '(inline rand-num))
(defun rand-num ()
  (random *rand-limit*))

(defmacro %rand-value (type)
  `(alexandria:random-elt (getf *captcha-db* ,type)))

(defmacro define-rand-selectors (&body types)
  `(progn
     ,@(loop for type in types
          as name = (alexandria:symbolicate :rand "-" type)
          collect `(defun ,name () (%rand-value ,type)))))

(define-rand-selectors :chain :object
                       :start-object :modify-object :query-object
                       :start-num :modify-num)

(defclass node ()
  ((type
    :initarg :type
    :initform (error "Must supply a type"))
   (parent
    :initarg :parent)
   (fmt
    :initform "")
   (args
    :initform ())
   (value
    :accessor node-value
    :initform 0)
   (object
    :accessor node-object
    :initform nil)))

(defun randomize (args)
  (loop for arg in args
     collect (if (and (typep arg 'number) (zerop (random 2)))
                 (format nil "~r" arg)
                 arg)))

(defmethod print-object ((node node) stream)
  (with-slots (fmt args) node
    (apply #'format stream fmt (randomize args))))

(defmacro with-num (num &body body)
  `(let ((,num (rand-num)))
     ,@body))

(defun rand-arith-op ()
  (alexandria:random-elt '(("times" . #'*)
                           ("plus" . #'+)
                           ("minus" . #'-))))

(defmacro with-arith-op ((op name) &body body)
  `(destructuring-bind (,op . ,name) (rand-arith-op)
     ,@body))

(defun parse-args (node node-args)
  (with-slots (value args object) node
    (loop for arg in node-args
       do (ecase arg
            (:num
             (with-num num
               (setf value num)
               (push num args)))
            (:num-sub
             (with-num num
               (when (> num value)
                 (setf num (if (zerop value) 0 (random value))))
               (decf value num)
               (push num args)))
            (:num-add
             (with-num num
               (incf value num)
               (push num args)))
            (:num-mul
             (with-num num
               (setf value (* value num))
               (push num args)))
            (:num-reset
             (setf value 0))
            (:num-objects
             (with-num num
               (setf value num)
               (setf object (rand-object))
               (setf args (nconc args (list num object num)))))
            (:arith-op
             (with-num num
               (with-arith-op (op name)
                 (setf value (funcall op value num))
                 (push name args))))
            (:get-object
             (push object args))))))


(defun parse-format (node format-list)
  (with-slots (parent value object fmt args) node
    (when parent
      (setf value (node-value parent)
            object (node-object parent)
            parent node))
    (setf fmt (car format-list))
    (parse-args node (cdr format-list))))

(defmethod initialize-instance :after ((node node) &key)
  (with-slots (type object) node
    (ecase type
      (:start-object
       (parse-format node (rand-start-object)))
      (:modify-object
       (parse-format node (rand-modify-object)))
      (:query-object
       (parse-format node (rand-query-object)))
      ;;
      (:start-num
       (parse-format node (rand-start-num)))
      (:modify-num
       (parse-format node (rand-modify-num))))))

(defun generate-chain ()
  (loop for parent = nil then node
     for type in (rand-chain)
     as node = (make-instance 'node :type type :parent parent)
     collect node))

(defun format-chain (chain)
  (format nil "~{~a ~}?" chain))

(defun chain-answer (chain)
  (node-value (car (last chain))))

(defun generate ()
  (let ((chain (generate-chain)))
    (values (format-chain chain) (chain-answer chain))))
