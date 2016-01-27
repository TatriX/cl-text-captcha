;;;; text-captcha.asd

(asdf:defsystem #:text-captcha
  :description "simple api to generate text CAPTCHA"
  :author "TatriX <tatrics@gmail.com>"
  :depends-on (:alexandria)
  :components ((:file "captcha")))

(asdf:defsystem #:text-captcha-demo
  :description "simple RESTfull demo of text-captcha usage"
  :author "TatriX <tatrics@gmail.com>"
  :depends-on (:clack
               :ningle
               :cl-markup
               :text-captcha)
  :components ((:file "demo")))
