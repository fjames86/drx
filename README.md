# DRX

## 1. Introduction
Doctor X (DrX). This is an eXtensible Data Representation (XDR) implementation in Common Lisp.

## 2. Usage
To use Doctor X you must first define a set of functions for encoding and decoding XDR for each of 
the types you wish to operate on. Rather than operating on a stream-based interface (as would be canonical)
instead the DrX interface uses a somewhat simpler abstraction of preallocated buffers filled with the XDR encoding.

Note that this means you need to know in advance how much space to allocate before encoding. This is in contrast 
to the canonical approach of e.g. dynamically expanding buffers written into by a stream. There is a reason for 
this restriction, namely that it permits reuse of buffers and reduces the need for extra memory copying.

### 2.1 XDR blocks
All operation are performed on preallocated fixed-sized buffers containing either XDR to
be deserialized, or empty to be filled in with XDR. 

```
(defparameter *myxdr* #(1 2 3 4 0 0 0 5 104 101 108 108 111 0 0 0))
(defparameter *blk* (make-xdr-block :buffer *myxdr*))

(decode-mystruct *blk*)
--> #S(MYSTRUCT :X 16909060 :Y "hello")

(defparameter *blk* (make-xdr-block :count 16))
(encode-mystruct *blk* *)
--> #(1 2 3 4 0 0 0 5 104 101 108 108 111 0 0 0)
```

### 2.2 Primitive types
* Four types of integers: `:INT32`, `:UINT32`, `:INT64`, `:UINT64`
* Boolean `:BOOLEAN`
* String `:STRING`
* Dynamically allocated opaque octet vector `:OPAQUE`
* Non-copying octet vector `:OPAQUE*`

Note that DrX currently does NOT support the canonical floating point types `:FLOAT32`, `:FLOAT64`, this 
is simply because the author had no need for them. They would be easy to add if required.

### 2.3 Structs
Define structures using `defxstruct`:

```
;; canonical XDR specification
;; struct mystruct {
;;   int x;
;;   string y<>;
;; };
(defxstruct mystruct ()
  (x :int32)
  (y :string))

;; This defines a struct and two functions which would look something like this
(DEFSTRUCT MYSTRUCT X Y)
(DEFUN DECODE-MYSTRUCT (BLK)
  (LET ((RET (MAKE-MYSTRUCT)))
    (SETF (MYSTRUCT-X RET) (DECODE-INT32 BLK))
    (SETF (MYSTRUCT-Y RET) (DECODE-STRING BLK))
    RET))
(DEFUN ENCODE-MYSTRUCT (BLK VAL)
  (ENCODE-INT32 BLK (MYSTRUCT-X VAL))
  (ENCODE-STRING BLK (MYSTRUCT-Y VAL))
  VAL)  
```

### 2.4 Unions
Tagged unions are defined using `defxunion`:

```
(defxunion myunion ()
  ((0 :ok) :string)
  (t :void))

(encode-myunion blk (make-xunion :ok "hello"))
(decode-myunion blk)
 --> (0 . "hello")
```

### 2.5 Arrays
Arrays can be defined using `defxarray`:

```
(defxarray myarray () :int32)

(encode-myarray blk '(1 2 3 4))
(decode-myarray blk)
 --> #(1 2 3 4)
```

### 2.6 Fixed opaque arrays
Arrays of a fixed size number of octets can be defined using `defxfixed`:

```
(defxfixed mybuffer () 8)

(encode-mybuffer blk #(1 2 3 4 5 6 7 8))
(decode-mybuffer blk)
  ->> #(1 2 3 4 5 6 7 8)
```

### 2.7 Enums
Mappings of symbols to integers can be defined using `defxenum`:

```
(defxenum myenum ()
  (:ok 0)
  (:error 1))

(encode-myenum blk :ok)
(decode-myenum blk)
  ->> :ok

```

Note that this defines an exclusive list, attempting to decode an integer which is does
not have a specified symbol mapping results in a decoding error. Likewise if an attempt 
is made to encode a symbol without a mapping.

### 2.8 Custom serializers
Define custom encoder and decoder using `defxencoder` and `defxdecoder`:
```
(defxencoder mycustom (blk val)
  body)
(defxdecoder mycustom (blk)
  body)
```

### 2.9 Lists
A common pattern is to have an item followed by a boolean indicating whether more exist
```
;; struct myitem_list {
;;   struct myitem val;
;    boolean more;
;; };

(defxlist myitem-list () myitem)
```

### 2.10 Optionals
Define optional types using `defxoptional`:

```
(defxoptional myitem-opt () myitem)
```

## 3. License
Licensed under the terms of the MIT license.
Frank James 2015.

