(in-package #:dex-edn)

(defparameter *options* nil)

(defun option (k)
  (getf *options* k))

(defgeneric prn (x))

(defmethod prn ((x number))
  (prin1 x))

(defmethod prn ((x string))
  (prin1 x))

(defmethod prn ((x (eql t)))
  (princ "true"))

(defmethod prn ((x symbol))
  (let ((name (symbol-name x)))
    (if (position-if #'lower-case-p name)
	(if (keywordp x)
	    (princ (concatenate 'string ":" name))
	    (princ name))
	(let ((*print-case* :downcase))
	  (prin1 x)))))

(defun string-uuid-p (string)
  (and (= (length string) 36)
       (eq (aref string  8) #\-)
       (eq (aref string 13) #\-)
       (eq (aref string 18) #\-)
       (eq (aref string 23) #\-)))

(defmethod prn ((x hash-table))
  (write-char #\{)
  (let ((count (hash-table-count x)))
    (loop for key being the hash-keys in x using (hash-value val)
	  for n from 1
          do (if (and (option :hash-key-string-uuid-p)
		      (stringp key)
		      (string-uuid-p key))
		 (progn (princ "#uuid ")
			(prin1 key))
		 (prn key))
	     (write-char #\space)
	     (prn val)
	  when (< n count)
	    do (write-char #\space)))
  (write-char #\}))

(defmethod prn ((x vector))
  (write-char #\[)
  (let ((count (length x)))
    (loop for i across x
	  for n from 1
          do (prn i)
	  when (< n count)
	    do (write-char #\space)))
  (write-char #\]))

(defun plist-p (list)
  (ignore-errors
   (doplist (key val list)
     (unless (symbolp key)
       (error "PLIST key is not symbol: ~s" key)))
   t))

(defun alist-p (list)
  (loop for i in list
	do (unless (consp i) (return))
	finally (return t)))

(defmethod prn ((x list))
  (cond

    ((and (option :plist-as-map-p) (plist-p x))
     (write-char #\{)
     (let ((count (/ (length x) 2)))
       (loop for (key val) on x by #'cddr
	     for n from 1
	     do (prn key)
		(write-char #\space)
		(prn val)
	     when (< n count)
	       do (write-char #\space)))
     (write-char #\}))

    ((and (option :alist-as-map-p) (alist-p x))
     (write-char #\{)
     (let ((count (length x)))
       (loop for (key . val) in x
	     for n from 1
	     do (prn key)
		(write-char #\space)
		(prn val)
	     when (< n count)
	       do (write-char #\space)))
     (write-char #\}))

    (t
     (write-char (if (option :list-as-vector-p) #\[ #\())
     (let ((count (length x)))
       (loop for i in x
	     for n from 1
             do (prn i)
	     when (< n count)
	       do (write-char #\space)))
     (write-char (if (option :list-as-vector-p) #\] #\))))))

(defmethod prn ((x edn-set))
  (write-char #\#)
  (write-char #\{)
  (let* ((value (value x))
	 (count (length value)))
    (loop for i in value
	  for n from 1
          do (prn i)
	  when (< n count)
	    do (write-char #\space)))
  (write-char #\}))

(defmethod prn ((x edn-uuid))
  (princ "#uuid ")
  (prin1 (value x)))

(defmethod prn ((x edn-inst))
  (princ "#inst ")
  (prin1 (value x)))

(defun encode (data &key stream
		         plist-as-map-p
		         alist-as-map-p
		         list-as-vector-p
		         hash-key-string-uuid-p)
  (with-output-to-string (out)
    (let ((*standard-output* (or stream out))
	  (*options* (list :plist-as-map-p plist-as-map-p
			   :alist-as-map-p alist-as-map-p
			   :list-as-vector-p list-as-vector-p
			   :hash-key-string-uuid-p hash-key-string-uuid-p)))
      (prn data))))
