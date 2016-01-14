;;;; Copyright (c) Frank James 2016 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage #:drx
  (:use #:cl)
  (:export #:defxstruct
           #:defxunion
           #:defxoptional
           #:defxarray
           #:defxfixed
           #:defxlist
           #:defxencoder
           #:defxdecoder
           #:defxenum
           #:defxtype
	   
	   #:xdr-block 
           #:make-xdr-block
           #:reset-xdr-block
           #:xdr-block-buffer
           #:xdr-block-offset
           #:xdr-block-count

           #:make-xunion
           #:xunion-tag
           #:xunion-val

           #:encode-int32
           #:decode-int32
           #:encode-uint32
           #:decode-uint32
           #:encode-int64
           #:decode-int64
           #:encode-uint64
           #:decode-uint64
           #:encode-boolean
           #:decode-boolean
           #:encode-string
           #:decode-string
           #:encode-opaque
           #:decode-opaque
           #:encode-opaque*
           #:decode-opaque*
           #:encode-list
           #:decode-list
           #:encode-array
           #:decode-array
           #:encode-fixed
           #:decode-fixed
           #:encode-void
           #:decode-void

           #:xdr-error
           #:generate-encoder-name
           #:generate-decoder-name 
           ))

(in-package #:drx)

(define-condition xdr-error (error)
  ((format-string :initarg :format-string :reader xdr-error-format-string)
   (args :initform nil :initarg :args :reader xdr-error-args))
  (:report (lambda (c stream)
             (format stream "XDR-ERROR: ~A"
                     (apply #'format nil
                            (xdr-error-format-string c)
                            (xdr-error-args c))))))

(defconstant +default-buffer-size+ 1024)
(defstruct xdr-block
  (buffer (make-array +default-buffer-size+ :element-type '(unsigned-byte 8) :initial-element 0)
	  :type (vector (unsigned-byte 8)))
  (count +default-buffer-size+ :type integer)
  (offset 0 :type integer))

(defun xdr-block (&optional count)
  (make-xdr-block :buffer (make-array (or count +default-buffer-size+)
				      :element-type '(unsigned-byte 8) :initial-element 0)
		  :count (or count +default-buffer-size+)))

(defun reset-xdr-block (blk)
  (declare (type xdr-block blk))
  (setf (xdr-block-count blk) (length (xdr-block-buffer blk))
        (xdr-block-offset blk) 0))

(defun space-or-lose (blk n)
  (declare (type xdr-block blk)
	   (type integer n))
  (unless (<= (+ (xdr-block-offset blk) n)
              (xdr-block-count blk))
    (error 'xdr-error :format-string "Truncated XDR buffer")))

(defun decode-int32 (blk)
  (declare (type xdr-block blk))
  (space-or-lose blk 4)
  (prog1 (nibbles:sb32ref/be (xdr-block-buffer blk)
                             (xdr-block-offset blk))
    (incf (xdr-block-offset blk) 4)))

(defun decode-uint32 (blk)
  (declare (type xdr-block blk))
  (space-or-lose blk 4)
  (prog1 (nibbles:ub32ref/be (xdr-block-buffer blk)
                             (xdr-block-offset blk))
    (incf (xdr-block-offset blk) 4)))

(defun decode-int64 (blk)
  (declare (type xdr-block blk))
  (space-or-lose blk 8)
  (prog1 (nibbles:sb64ref/be (xdr-block-buffer blk)
                             (xdr-block-offset blk))
    (incf (xdr-block-offset blk) 8)))

(defun decode-uint64 (blk)
  (declare (type xdr-block blk))
  (space-or-lose blk 8)
  (prog1 (nibbles:ub64ref/be (xdr-block-buffer blk)
                             (xdr-block-offset blk))
    (incf (xdr-block-offset blk) 8)))

(defun encode-int32 (blk int32)
  (declare (type xdr-block blk)
	   (type integer int32))
  (space-or-lose blk 4)
  (setf (nibbles:sb32ref/be (xdr-block-buffer blk)
                            (xdr-block-offset blk))
        int32)
  (incf (xdr-block-offset blk) 4))

(defun encode-uint32 (blk int32)
  (declare (type xdr-block blk)
	   (type integer int32))
  (space-or-lose blk 4)
  (setf (nibbles:ub32ref/be (xdr-block-buffer blk)
                            (xdr-block-offset blk))
        int32)
  (incf (xdr-block-offset blk) 4))

(defun encode-int64 (blk int64)
  (declare (type xdr-block blk)
	   (type integer int64))
  (space-or-lose blk 8)
  (setf (nibbles:sb64ref/be (xdr-block-buffer blk)
                            (xdr-block-offset blk))
        int64)
  (incf (xdr-block-offset blk) 8))

(defun encode-uint64 (blk uint64)
  (declare (type xdr-block blk)
	   (type integer uint64))
  (space-or-lose blk 8)
  (setf (nibbles:ub64ref/be (xdr-block-buffer blk)
                            (xdr-block-offset blk))
        uint64)
  (incf (xdr-block-offset blk) 8))

(defun decode-boolean (blk)
  (declare (type xdr-block blk))
  (not (zerop (decode-int32 blk))))

(defun encode-boolean (blk bool)
  (declare (type xdr-block blk))
  (encode-int32 blk (if bool 1 0)))

(defun decode-string (blk)
  (declare (type xdr-block blk))
  (let ((len (decode-uint32 blk)))
    (space-or-lose blk len)
    (prog1 (babel:octets-to-string (xdr-block-buffer blk)
                                   :start (xdr-block-offset blk)
                                   :end (+ (xdr-block-offset blk) len))
      (incf (xdr-block-offset blk) len)
      (let ((m (mod len 4)))
        (unless (zerop m)
          (space-or-lose blk (- 4 m))
          (incf (xdr-block-offset blk) (- 4 m)))))))

(defun encode-string (blk string)
  (declare (type xdr-block blk)
	   (type string string))
  (let* ((octets (babel:string-to-octets string))
         (len (length octets)))
    (encode-uint32 blk len)
    (space-or-lose blk len)
    (let ((buffer (xdr-block-buffer blk))
          (start (xdr-block-offset blk)))
      (dotimes (i len)
        (setf (aref buffer (+ start i)) (aref octets i))))
    (incf (xdr-block-offset blk) len)
    (let ((m (mod len 4)))
      (unless (zerop m)
        (space-or-lose blk (- 4 m))
        (incf (xdr-block-offset blk) (- 4 m))))))

(defun decode-opaque (blk)
  (declare (type xdr-block blk))
  (let ((len (decode-uint32 blk)))
    (space-or-lose blk len)
    (let ((a (make-array len :element-type '(unsigned-byte 8)))
          (start (xdr-block-offset blk))
          (buffer (xdr-block-buffer blk)))
      (dotimes (i len)
        (setf (aref a i) (aref buffer (+ start i))))
      (incf (xdr-block-offset blk) len)
      (let ((m (mod len 4)))
        (unless (zerop m)
          (space-or-lose blk (- 4 m))
          (incf (xdr-block-offset blk) (- 4 m))))
      a)))

(defun encode-opaque (blk arr)
  (declare (type xdr-block blk)
	   (type (or null (vector (unsigned-byte 8))) arr))
  (let* ((len (length arr))
         (m (mod len 4))
         (xlen (if (zerop m) len (+ len (- 4 m)))))
    (space-or-lose blk xlen)
    (encode-uint32 blk len)
    (let ((start (xdr-block-offset blk))
          (buffer (xdr-block-buffer blk)))
      (dotimes (i len)
        (setf (aref buffer (+ start i)) (aref arr i)))
      (incf (xdr-block-offset blk) xlen)))
  nil)

(defun decode-opaque* (blk)
  (declare (type xdr-block blk))
  (let* ((len (decode-uint32 blk))
         (m (mod len 4))
         (xlen (if (zerop m) len (+ len (- 4 m)))))
    (space-or-lose blk xlen)
    (let ((res (list (xdr-block-buffer blk)
                     (xdr-block-offset blk)
                     (+ (xdr-block-offset blk) len))))
      (incf (xdr-block-offset blk) xlen)
      res)))

(defun encode-opaque* (blk arg)
  (declare (type xdr-block blk)
	   (type list arg))
  (destructuring-bind (buffer start end) arg 
    (let* ((len (- end start))
           (m (mod len 4))
           (xlen (if (zerop m) len (+ len (- 4 m)))))
      (encode-uint32 blk len)
      (space-or-lose blk xlen)
      (let ((buf (xdr-block-buffer blk))
            (offset (xdr-block-offset blk)))
        (dotimes (i len)
          (setf (aref buf (+ offset i)) (aref buffer (+ start i))))
        ;; should zero out the remainder
        (dotimes (i m)
          (setf (aref buf (+ offset len i)) 0))
	;; increase the offset
	(incf (xdr-block-offset blk) xlen)))))
          
(defun decode-optional (blk decoder)
  (declare (type xdr-block blk)
	   (type function decoder))
  (let ((p (decode-boolean blk)))
    (when p
      (funcall decoder blk))))

(defun encode-optional (blk encoder val)
  (declare (type xdr-block blk)
	   (type function encoder))
  (encode-boolean blk val)  
  (when val
    (funcall encoder blk val)))

(defun decode-list (blk item-decoder)
  (declare (type xdr-block blk)
	   (type function item-decoder))
  (do ((ret nil)
       (done nil))
      (done ret)
    (let ((item (funcall item-decoder blk)))
      (setf ret (append ret (list item)))
      (let ((p (decode-boolean blk)))
        (unless p (setf done t))))))

(defun encode-list (blk item-encoder list)
  (declare (type xdr-block blk)
	   (type function item-encoder)
	   (type list list))
  (do ((items list (cdr items)))
      ((null items))
    (funcall item-encoder blk (car items))
    (if (cdr items)        
        (encode-boolean blk t)
        (encode-boolean blk nil))))
      
(defun decode-array (blk item-decoder &optional count)
  (declare (type xdr-block blk)
	   (type function item-decoder)
	   (type (or null integer) count))
  (unless count
    (setf count (decode-uint32 blk)))
  (let ((ret (make-array count)))
    (dotimes (i count)
      (setf (aref ret i) (funcall item-decoder blk)))
    ret))

(defun encode-array (blk item-encoder array &optional count)
  (declare (type xdr-block blk)
	   (type function item-encoder)
	   (type vector array)
	   (type (or null integer) count))
  (when count
    (unless (= count (length array))
      (error 'xdr-error
             :format-string "Fixed array count ~A doesn't match array count ~A"
             :args (list count (length array))))
    (encode-uint32 blk count))
  
  (dotimes (i (length array))
    (funcall item-encoder blk (aref array i))))

(defun decode-array-list (blk item-decoder &optional count)
  (declare (type xdr-block blk)
	   (type function item-decoder)
	   (type (or null integer) count))
  (unless count
    (setf count (decode-uint32 blk)))
  (loop :for i :below count :collect
     (funcall item-decoder blk)))

(defun encode-array-list (blk item-encoder array &optional count)
  (declare (type xdr-block blk)
	   (type function item-encoder)
	   (type list array)
	   (type (or null integer) count))
  (when count
    (unless (= count (length array))
      (error 'xdr-error
             :format-string "Fixed array count ~A doesn't match array count ~A"
             :args (list count (length array))))
    (encode-uint32 blk count))

  (dolist (x array)
    (funcall item-encoder blk x)))

(defun decode-fixed (blk count)
  (declare (type xdr-block blk)
	   (type integer count))
  (let ((a (make-array count :element-type '(unsigned-byte 8)))
        (buffer (xdr-block-buffer blk))
        (start (xdr-block-offset blk)))
    (space-or-lose blk count)
    (dotimes (i count)
      (setf (aref a i) (aref buffer (+ start i))))
    (incf (xdr-block-offset blk) count)
    a))

(defun encode-fixed (blk array count)
  (declare (type xdr-block blk)
	   (type (vector (unsigned-byte 8)) array)
	   (type integer count))
  (let ((buffer (xdr-block-buffer blk))
        (start (xdr-block-offset blk)))
    (space-or-lose blk count)
    (dotimes (i count)
      (setf (aref buffer (+ start i)) (aref array i)))
    (incf (xdr-block-offset blk) count)))

(defun decode-void (blk)
  (declare (type xdr-block blk))
  (declare (ignore blk))
  nil)

(defun encode-void (blk val)
  (declare (type xdr-block blk))
  (declare (ignore blk val))
  nil)

;; ----------------------------------------

(defun symbolicate (&rest names)
  (intern (format nil "~{~A~}" names)))

(defun generate-decoder-name (name)
  (case name
    (:int32 'decode-int32)
    (:uint32 'decode-uint32)
    (:int64 'decode-int64)
    (:uint64 'decode-uint64)
    (:boolean 'decode-boolean)
    (:string 'decode-string)
    (:opaque 'decode-opaque)
    (:opaque* 'decode-opaque*)
    (:void 'decode-void)
    (otherwise
     (intern (format nil "DECODE-~A" name) (symbol-package name)))))

(defun generate-encoder-name (name)
  (case name
    (:int32 'encode-int32)
    (:uint32 'encode-uint32)
    (:int64 'encode-int64)
    (:uint64 'encode-uint64)
    (:boolean 'encode-boolean)
    (:string 'encode-string)
    (:opaque 'encode-opaque)
    (:opaque* 'encode-opaque*)
    (:void 'encode-void)
    (otherwise
     (intern (format nil "ENCODE-~A" name) (symbol-package name)))))

(defmacro defxencoder (name (blk val) &body body)
  "Define a custom encoder for the type named by NAME.
BLK, VAL ::= symbols
BODY ::= body of the function."
  `(defun ,(generate-encoder-name name) (,blk ,val)
     ,@body))

(defmacro defxdecoder (name (blk) &body body)
  "Define a custom decoder for the type named by NAME."
  `(defun ,(generate-decoder-name name) (,blk)
     ,@body))

(defmacro defxstruct (name options &rest slots)
  "Define a struct and associated XDR encoder and decoder.
NAME ::= symbol naming the structure.
OPTIONS ::= {OPTION-SPEC}*
OPTION-SPEC ::= (:MODE :STRUCT|:LIST|:PLIST)
SLOTS :: list of forms (slot-name slot-type)* defining each slot.

If the :MODE is :STRUCT (the default) a DEFSTRUCT form will be define the 
type structure to be used as input/output values.
If the :MODE is :LIST, the values are expected to be an ordered list containing 
each of the structure slots.
If the :MODE is :PLIST, the values are expected to be property lists containing
values defined by the slot names."
  (let ((mode (or (cadr (assoc :mode options)) :struct)))
    `(progn
       ,@(when (eq mode :struct)
           `((defstruct ,name
               ,@(mapcar (lambda (slot)
                           (destructuring-bind (slot-name slot-type &rest slot-args) slot
                             (declare (ignore slot-type))
                             `(,slot-name ,@slot-args)))
                         slots))))
     
       (defxdecoder ,name (blk)
         ,(ecase mode
            (:struct 
             `(let ((ret (,(symbolicate 'make- name))))
                ,@(mapcar (lambda (slot)
                            (destructuring-bind (slot-name slot-type &rest slot-args) slot
                              (declare (ignore slot-args))
                              `(setf (,(symbolicate name '- slot-name) ret)
                                     (,(generate-decoder-name slot-type) blk))))
                          slots)
                ret))
            (:list
             `(list ,@(mapcar (lambda (slot)
                                (destructuring-bind (slot-name slot-type) slot
                                  (declare (ignore slot-name))
                                  `(,(generate-decoder-name slot-type) blk)))
                              slots)))
            (:plist
             `(list ,@(mapcan (lambda (slot)
                                (destructuring-bind (slot-name slot-type) slot
                                  (list `',slot-name `(,(generate-decoder-name slot-type) blk))))
                              slots)))))

       (defxencoder ,name (blk val)
         ,(ecase mode
            (:struct 
             `(progn
                ,@(mapcar (lambda (slot)
                            (destructuring-bind (slot-name slot-type &rest slot-args) slot
                              (declare (ignore slot-args))
                              `(,(generate-encoder-name slot-type) blk                       
                                 (,(symbolicate name '- slot-name) val))))
                          slots)))
            (:list
             (let ((gval (gensym)))
               `(let ((,gval val))
                  ,@(mapcar (lambda (slot)
                              (destructuring-bind (slot-name slot-type) slot
                                (declare (ignore slot-name))
                                `(progn
                                   (,(generate-encoder-name slot-type) blk (car ,gval))
                                   (setf ,gval (cdr ,gval)))))
                            slots))))
            (:plist
             `(progn
                ,@(mapcar (lambda (slot)
                            (destructuring-bind (slot-name slot-type) slot
                              `(,(generate-encoder-name slot-type) blk (getf val ',slot-name))))
                          slots))))               
         val))))

(defun make-xunion (tag val)
  (cons tag val))

(defun xunion-tag (union)
  (declare (type cons union))
  (car union))

(defun xunion-val (union)
  (declare (type cons union))
  (cdr union))

(defmacro defxunion (name options &rest arms)
  "Define a discriminated union type.
NAME ::= symbol naming the union type.
OPTIONS ::= list of options.
ARMS ::= list of forms (arm-val arm-type) where arm-val is an integer and 
arm-type is a symbol naming the type of the union on that arm.

The last arm-val may be the symbol T, which indicates it should match any
other value. If this form is not supplied then an error will be signalled
in this case.

Values used as input and output are formed and accessed using the MAKE-XUNION, 
XUNION-TAG and XUNION-VAL functions."
  (let ((encoder (or (let ((e (cadr (assoc :enum options))))
                       (when e (generate-encoder-name e)))
                     'encode-int32))
        (decoder (or (let ((e (cadr (assoc :enum options))))
                       (when e (generate-decoder-name e)))
                     'decode-int32)))
    `(progn
       (defxdecoder ,name (blk)
         (let ((tag (,decoder blk)))
           (cond
             ,@(mapcar (lambda (arm)
                         (destructuring-bind (arm-val type) arm
                           (if (eq arm-val 't)
                               `(t
                                 (make-xunion tag
                                              (,(generate-decoder-name type) blk)))
                               `((eql tag ,arm-val)
                                 (make-xunion tag 
                                              (,(generate-decoder-name type) blk))))))
                       arms)
             ,@(unless (find-if (lambda (arm)
                                  (let ((arm-val (car arm)))
                                    (eq arm-val 't)))
                                arms)
		       `((t (error 'xdr-error
				   :format-string "Unexpected union tag ~A"
				   :args (list tag))))))))

       (defxencoder ,name (blk union)
         (let ((tag (xunion-tag union))
               (val (xunion-val union)))
           (cond 
             ,@(mapcar (lambda (arm)
                         (destructuring-bind (arm-val type) arm
                           (if (eq arm-val 't)
                               `(t
                                 (,encoder blk tag)
                                 (,(generate-encoder-name type) blk val))
                               `((eql tag ,arm-val)
                                 (,encoder blk ,arm-val)
                                 (,(generate-encoder-name type) blk val)))))
                       arms)
             ,@(unless (find-if (lambda (arm)
                                  (let ((arm-val (car arm)))
                                    (eq arm-val 't)))
                                arms)
		       `((t (error 'xdr-error
				   :format-string "Unexpected union tag ~A"
				   :args (list tag)))))))))))

(defmacro defxoptional (name options type)
  "Define an optional-type encoder and decoder.
NAME ::= symbol naming the type.
TYPE ::= symbol naming the optional type."
  (declare (ignore options))
  `(progn
     (declaim (inline ,(generate-decoder-name name)))
     (defxdecoder ,name (blk)
       (decode-optional blk (function ,(generate-decoder-name type))))
     (declaim (inline ,(generate-encoder-name name)))
     (defxencoder ,name (blk val)
       (encode-optional blk (function ,(generate-encoder-name type)) val))))

(defmacro defxarray (name options type &optional count)
  "Define a fixed or variable length array type.
NAME ::= symbol naming the type.
OPTIONS ::= {OPTION-SPEC}*
OPTION-SPEC ::= (:MODE :ARRAY|:LIST)
TYPE ::= symbol naming the array type.
COUNT ::= if supplied, defines a fixed-length array of length count. 
If not supplied, defines a variable array.

If the MODE is :ARRAY (the default), then the value is expected to be an array or vector. If the MODE is :LIST then the value is a list."
  (let ((mode (or (cadr (assoc :mode options)) :array)))
    `(progn
       (defxdecoder ,name (blk)
         ,(ecase mode
            (:array `(decode-array blk (function ,(generate-decoder-name type)) ,count))
            (:list `(decode-array-list blk (function ,(generate-decoder-name type)) ,count))))

       (defxencoder ,name (blk array)
         ,(ecase mode
            (:array `(encode-array blk (function ,(generate-encoder-name type)) array ,count))
            (:list `(encode-array-list blk (function ,(generate-encoder-name type)) array ,count)))))))

(defmacro defxfixed (name options count)
  "Defines a fixed opaque array of length count.
NAME ::= symbol naming the new type.
COUNT ::= integer specifying the length of the fixed array."
  (declare (ignore options))
  `(progn
     (declaim (inline ,(generate-decoder-name name)))
     (defxdecoder ,name (blk)
       (decode-fixed blk ,count))
     (declaim (inline ,(generate-encoder-name name)))
     (defxencoder ,name (blk array)
       (encode-fixed blk array ,count))))

(defmacro defxlist (name options type)
  "Defines encoder and decoder for the commonly found pattern of <item><boolean more?>.
NAME ::= name of the type to define.
OPTIONS ::= list of options.
TYPE ::= symbol naming the existing type."
  (declare (ignore options))
  `(progn
     (declaim (inline ,(generate-decoder-name name)))
     (defxdecoder ,name (blk)
       (decode-list blk (function ,(generate-decoder-name type))))
     (declaim (inline ,(generate-encoder-name name)))
     (defxencoder ,name (blk list)
       (encode-list blk (function ,(generate-encoder-name type)) list))))


(defmacro defxenum (name options &rest vals)
  "Define an enum type, which is a mapping from integers to symbols.
NAME ::= type name
OPTIONS ::= {OPTION-SPEC}* a list of options
OPTION-SPEC ::= (:exclusive exclusive-p)
VALS ::= list of forms (symbol integer)*.

If exclusive-p is true (the default) then an attempt to encode an enum
value which does not have a defined mapping will result in an error. An attempt
to decode an enum which does not have a mappign will also error.
If exclsuive-p is false, an attempt to encode an arbitrary integer will
be permitted in addition to the defined symbol mappings. An attempt to decode
an enum without a defined mapping will return that integer."
  (let ((exclp (if (assoc :exclusive options)
		   (cadr (assoc :exclusive options))
		   t)))
    `(progn
       (defxdecoder ,name (blk)
	 (let ((val (decode-int32 blk)))
	   (cond
	     ,@(mapcar (lambda (v)
			 (destructuring-bind (vname vval) v
			   `((= val ,vval) ',vname)))
		       vals)
	     ,(if exclp
		  ;; exclusive mode, signal an error
		  `(t (error 'xdr-error
			     :format-string "Unexpected enum value ~A expected one of ~A"
			     :args (list val ',(mapcar #'cadr vals))))
		  ;; non-exclusive mode, return the integer
		  `(t val)))))

       (defxencoder ,name (blk sym)
	 (let ((val
		(cond
		  ,@(mapcar (lambda (v)
			      (destructuring-bind (vname vval) v
				`((eq sym ',vname) ,vval)))
			    vals)
		  (t
		   ,(if exclp
			;; exclusive mode, must be a defined symbol
			`(error 'xdr-error
				:format-string "Unexpected enum value ~S expected one of ~A"
				:args (list sym ',(mapcar #'car vals)))
			;; non-exclusive mode, must be a defined symbol
			;; or an integer
			`(if (integerp sym)
			     sym
			     (error 'xdr-error
				    :format-string "Unexpected enum value ~S, expected one of ~A or an integer"
				    :args (list sym ',(mapcar #'car vals)))))))))
	   (encode-uint32 blk val))))))


(defmacro defxtype (name options type)
  "Define a typename alias.
NAME ::= symbol naming the new type.
OPTIONS ::= list of options.
TYPE ::= symbol naming the existing typename to alias."
  (declare (ignore options))
  `(progn
     (declaim (inline ,(generate-encoder-name name)))
     (defxencoder ,name (blk val) (,(generate-encoder-name type) blk val))
     (declaim (inline ,(generate-decoder-name name)))
     (defxdecoder ,name (blk) (,(generate-decoder-name type) blk))))

