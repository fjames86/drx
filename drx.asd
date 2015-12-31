;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :drx
  :name "drx"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "XDR serializer."
  :license "MIT"
  :serial t
  :components
  ((:file "drx"))  
  :depends-on (:nibbles :babel))




