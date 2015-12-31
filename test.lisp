;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:drx.test
  (:use #:cl #:drx))

(in-package #:drx.test)

;; genreates a defstruct form
(defxstruct mystruct ()
  (x :uint32)
  (y :string))

(defun test-mystruct ()
  (let ((m (make-mystruct :x 123 :y "frank"))
        (blk (make-xdr-block)))
    (encode-mystruct blk m)
    (setf (xdr-block-offset blk) 0)
    (decode-mystruct blk)))

(defxstruct mystruct2 ((:mode :list))
  (x :uint32)
  (y :string))

(defun test-mystruct2 ()
  (let ((m (list 123 "frank"))
        (blk (make-xdr-block)))
    (encode-mystruct2 blk m)
    (setf (xdr-block-offset blk) 0)
    (decode-mystruct2 blk)))

(defxstruct mystruct3 ((:mode :plist))
  (x :uint32)
  (y :string))
(defun test-mystruct3 ()
  (let ((m (list 'x 123 'y "frank"))
        (blk (make-xdr-block)))
    (encode-mystruct3 blk m)
    (setf (Xdr-block-offset blk) 0)
    (decode-mystruct3 blk)))

(defun test-opaque* ()
  (let ((blk (make-xdr-block))
        (o (make-array 32 :initial-element 1)))
    (encode-opaque* blk (list o 4 12))
    (setf (xdr-block-offset blk) 0)
    (decode-opaque* blk)))
                     
(defconstant +e1+ 0)
(defconstant +e2+ 1)
(defxenum myenum ()
  (:e1 +e1+)
  (:e2 +e2+))

(defxunion myunion ((:enum myenum))
  (:e1 :uint32)
  (:e2 :string)
  (t :void))
(defun test-myunion ()
  (let ((u (make-xunion :e1 123))
        (blk (make-xdr-block)))
    (encode-myunion blk u)
    (setf (xdr-block-offset blk) 0)
    (decode-myunion blk)))

(defxunion myunion2 ()
  (+e1+ :uint32)
  (+e2+ :string)
  (t :void))
(defun test-myunion2 ()
  (let ((u (make-xunion +e1+ 123))
        (blk (make-xdr-block)))
    (encode-myunion2 blk u)
    (setf (xdr-block-offset blk) 0)
    (decode-myunion2 blk)))



(defxlist mystruct-list ()
  mystruct)

(defun test-mystruct-list ()
  (let ((alist (list (make-mystruct :x 123 :y "frank")
                     (make-mystruct :x 321 :y "james")))
        (blk (make-xdr-block)))
    (encode-mystruct-list blk alist)
    (setf (xdr-block-offset blk) 0)
    (decode-mystruct-list blk)))

(defxoptional mystruct-opt () mystruct)

(defun test-mystruct-opt (&optional yes-no)
  (let ((a (make-mystruct :x 123 :y "frank"))
        (blk (make-xdr-block :count 32)))
    (encode-mystruct-opt blk (if yes-no a nil))
    (setf (xdr-block-offset blk) 0)
    (decode-mystruct-opt blk)))

