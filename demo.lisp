(in-package :cl-user)
(defpackage #:text-captcha-demo
  (:use :cl :cl-markup)
  (:export :start :stop :start-production))

(in-package :text-captcha-demo)

(import 'lack.builder:builder)

(defvar *app* (make-instance 'ningle:<app>))

(defvar *handler* nil)

(defparameter *base-url* "")

(defun url (url)
  (concatenate 'string *base-url* url))

;;; html

(defun template(content)
  (html5
   (:head
    (:meta :charset "utf-8")
    (:title "Text captcha demo")
    (:link :rel "stylesheet" :href "static/captcha.css")
    (:body (raw content)
           (:script :src "static/captcha.js" nil)))))

(defun get-param (name params)
  (cdr (assoc name params :test #'string=)))

;;; routes

(setf (ningle:route *app* (url "/"))
      (lambda (params)
        (declare (ignore params))
        (template (markup
                   (:form :id "captcha-form"
                          (:label "Captcha"
                                  (:input :id "captcha" :readonly "true"))
                          (:button :id "refresh" "Refresh")
                          (:br) (:br)
                          (:label "Enter answer"
                                  (:input :id "answer" :name "answer" :autocomplete "off"))
                          (:button "Check")
                          (:div :id "result" ""))))))

;; for demo purposes only one user and request are allowed
(defparameter *answer* nil)

(setf (ningle:route *app* (url "/question"))
      (lambda (params)
        (declare (ignore params))
        (multiple-value-bind (captcha answer) (text-captcha:generate)
          (setf *answer* answer)
          captcha)))


(setf (ningle:route *app* (url "/answer") :method :PUT)
      (lambda (params)
        (handler-case
            (let ((answer (parse-integer (get-param "answer" params))))
              (if (equalp answer *answer*)
                  '(200 () ("OK"))
                  '(400 () ("Wrong answer"))))
          (error (err)
            (format t "Error: ~a ~%" err)
            '(400 () ("Bad request"))))))

(defun start (&rest args)
  (let ((app (lack:builder
              (:static :path "/static/"
                       :root #p"./static/")
              ;; :accesslog
              ;; :backtrace
              *app*)))
    (setf *handler*
          (apply #'clack:clackup app args))))

(defun stop()
  (clack:stop *handler*)
  (setf *handler* nil))

(defun start-production ()
  (start
   :server :woo
   :debug nil
   :num-workers 2
   :use-thread nil
   :use-default-middlewares nil))
