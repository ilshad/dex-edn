(in-package #:dex-edn)

(defparameter *whitespace* '(#\space #\tab #\newline #\return))
(defparameter *ignored* (append *whitespace* '(#\,)))
(defparameter *enclosing* '(#\{ #\} #\[ #\] #\( #\)))
(defparameter *end-of-symbol* (append *ignored* *enclosing*))
(defparameter *tag-follow* (append *whitespace* '(#\" #\{ #\[ #\()))

(define-condition decode-error (error) ())

(define-condition unknown-symbol-error (decode-error)
  ((symbol-name :initarg :symbol-name)))

(define-condition unknown-tag-error (decode-error)
  ((tag-name :initarg :tag-name)))

(define-condition syntax-error (decode-error)
  ((message :initarg :message)))

(defun syntax-error (string char next-char)
  (error 'syntax-error
	 :message (format nil "~a followed by ~a"
			  (or string (char-name char))
			  (char-name next-char))))

(defun %debug (topic value stack map-keys)
  (format t "~:[~;~:*~a ~]~s ~20t~s~50t~s~%"
	  topic
	  value
	  (mapcar #'(lambda (item) (getf item :type)) stack)
	  map-keys))

(defparameter *primitive-symbols* '(("nil" . nil) ("false" . nil) ("true" . t)))

(defun parse-symbol (string)
  (if-let (cons (assoc string *primitive-symbols* :test 'string=))
    (cdr cons)
    (error 'unknown-symbol-error :symbol-name string)))

(defparameter *built-in-tags* '(("inst" . :inst) ("uuid" . :uuid)))

(defun parse-tag (string)
  (or (cdr (assoc string *built-in-tags* :test 'string=))
      (error 'unknown-tag-error :tag-name (format nil "#~a" string))))

(defun tag-literal (keyword)
  (format nil "#~a" (string-downcase (symbol-name keyword))))

(defun decode (in &key debug-p)
  (let ((in (if (stringp in) (make-string-input-stream in) in))
        stack
	map-keys
	reuse
	result)
    (macrolet ((top (prop) `(getf (car stack) ,prop)))
      (labels ((debug (value &optional topic)
		 (when debug-p (%debug topic value stack map-keys)))
	       
               (write-top (char)
		 (write-char char (top :value)))

	       (new (type &optional value)
		 (push (list :type type :value value) stack)
		 (debug type "new"))

	       (begin (char)
		 (case char

		   (#\:
		    (new :keyword (make-string-output-stream))
		    (write-top char))

		   (#\"
		    (new :string (make-string-output-stream)))

		   (#\[
		    (new :vector (make-array 5 :fill-pointer 0 :adjustable t)))

		   (#\(
		    (new :list))

		   (#\{
		    (push nil map-keys)
                    (new :map (make-hash-table :test #'equal)))

		   (#\#
		    (let ((next-char (read-char in)))
                      (case next-char
			(#\{ (new :set))
			(#\_ (new :discard))
			(otherwise
			 (if (alpha-char-p next-char)
			     (progn
			       (new :tagged (list :out (make-string-output-stream)))
			       (setf reuse next-char))
			     (syntax-error nil char next-char))))))

		   (#\;
		    (new :comment))

		   (#\-
		    (let ((next-char (peek-char nil in)))
		      (if (digit-char-p next-char)
			  (progn (new :number (make-string-output-stream))
				 (write-top char))
			  (syntax-error nil char next-char))))

		   (otherwise
		    (cond

		      ((digit-char-p char)
		       (new :number (make-string-output-stream))
		       (write-top char))

		      ((alpha-char-p char)
		       (new :symbol (make-string-output-stream))
		       (write-top char))))))

               (end (&optional (finalize 'identity))
		 (let* ((item (pop stack))
			(value (funcall finalize (getf item :value))))
		   (when (eq (getf item :type) :map)
		     (pop map-keys))
                   (case (top :type)
		     (:map      (if-let (key (car map-keys))
				  (progn
				    (setf (gethash key (top :value)) value)
				    (setf (car map-keys) nil))
				  (setf (car map-keys) value)))
		     (:vector   (vector-push-extend value (top :value)))
		     (:set      (pushnew value (top :value)))
		     (:list     (push value (top :value)))
		     (otherwise (setf result value)))
		   (debug value "end"))))

	(loop for char = (or reuse (read-char in nil))
	      while (and char (not result))
	      do (debug char)
		 (when reuse (setf reuse nil))
		 (case (top :type)

		   (:vector     (if (char= char #\]) (end) (begin char)))
		   (:list       (if (char= char #\)) (end) (begin char)))
		   ((:map :set) (if (char= char #\}) (end) (begin char)))

		   (:number
		    (cond

		      ((or (digit-char-p char) (char= char #\.))
		       (write-top char))

		      ((member char *end-of-symbol*)
		       (end (compose #'read-from-string #'get-output-stream-string))
		       (setf reuse char))))

		   (:keyword
		    (if (member char *end-of-symbol*)
			(progn
			  (end (compose #'read-from-string #'get-output-stream-string))
			  (setf reuse char))
			(write-top char)))

		   (:symbol
		    (if (member char *end-of-symbol*)
			(progn
			  (end (compose #'parse-symbol #'get-output-stream-string))
			  (setf reuse char))
			(write-top char)))

		   (:string
		    (if (char= char #\")
			(end #'get-output-stream-string)
			(write-top char)))

                   (:tagged
		    (flet ((out (value) (get-output-stream-string (getf value :out))))
		      (if-let ((tag (getf (top :value) :tag)))
			(when (not (member char *whitespace*))
                          (let ((started-p (getf (top :value) :started-p)))
			    (if (char= char #\")
				(if started-p
				    (end #'(lambda (value) (list :tagged tag (out value))))
				    (setf (getf (top :value) :started-p) t))
				(if started-p
				    (write-char char (getf (top :value) :out))
                                    (syntax-error (tag-literal tag) nil char)))))
			(if (member char *tag-follow*)
			    (progn
			      (setf (getf (top :value) :tag)
				    (parse-tag (out (top :value))))
                              (when (not (member char *whitespace*))
				(setf reuse char)))
			    (write-char char (getf (top :value) :out))))))

		   (:comment
		    (when (char= char #\newline)
		      (pop stack)))

		   (:discard
		    (flet ((discard () (pop stack) (setf reuse char)))
                      (if (top :value)
			  (when (member char *end-of-symbol*)
                            (discard))
			  (if (member char *enclosing*)
                              (discard)
			      (when (not (member char *whitespace*))
				(setf (top :value) t))))))

		   (otherwise (begin char))))
	result))))
