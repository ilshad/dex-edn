(in-package #:dex-edn)

(defparameter *whitespace* '(#\space #\tab #\newline #\return))
(defparameter *ignored* (append *whitespace* '(#\,)))
(defparameter *enclosing* '(#\{ #\} #\[ #\] #\( #\)))
(defparameter *end-of-symbol* (append *ignored* *enclosing*))
(defparameter *tag-follow* (append *whitespace* '(#\" #\{ #\[ #\()))

(defgeneric read-tagged (tag value))

(define-condition decode-error (error) ())

(define-condition unknown-symbol-error (decode-error)
  ((symbol-name :initarg :symbol-name)))

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

(defun decode (in &key (map-as :hash-table)
		       (vector-as :array)
		       (list-as :list)
		       (uuid-as-string-p t)
		       (inst-as-string-p t)
                       debug-p)
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
		    (new :vector
			 (case vector-as
			   (:array (make-array 5 :fill-pointer 0 :adjustable t))
			   (:list  (list)))))

		   (#\(
		    (new :list
			 (case list-as
			   (:array (make-array 5 :fill-pointer 0 :adjustable t))
			   (:list  (list)))))

		   (#\{
		    (push nil map-keys)
		    (new :map (case map-as
				(:hash-table (make-hash-table :test #'equal))
				((:plist :alist) (list)))))

		   (#\#
		    (let ((next-char (read-char in)))
                      (case next-char
			(#\{ (new :set))
			(#\_ (new :discard))
			(otherwise
			 (if (alpha-char-p next-char)
			     (progn
                               (new :tagged (make-string-output-stream))
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

	       (pop-value ()
		 (let* ((item (pop stack))
			(value (getf item :value)))
		   (case (getf item :type)

		     (:list
		      (if (eq list-as :list)
			  (reverse value)
			  value))

		     (:vector
		      (if (eq vector-as :list)
			  (reverse value)
			  value))

		     (:map
		      (pop map-keys)
		      value)

		     (:keyword
		      (read-from-string
		       (let ((name (get-output-stream-string value)))
                         (if (position-if #'upper-case-p name)
                             (concatenate 'string ":|" (subseq name 1) "|")
			     name))))

		     (:symbol
		      (parse-symbol (get-output-stream-string value)))

		     (:number
		      (read-from-string (get-output-stream-string value)))

		     (:string
		      (get-output-stream-string value))

		     (:tagged
                      (let ((tag (getf item :tag)))
			(or (and (eq tag :|uuid|) uuid-as-string-p value)
			    (and (eq tag :|inst|) inst-as-string-p value)
			    (read-tagged tag value))))

                     (otherwise value))))

               (end ()
		 (let ((value (pop-value)))
		   (debug value "end")
		   (case (top :type)
		     
		     (:map
		      (if-let (key (car map-keys))
			(progn
			  (case map-as
			    (:hash-table (setf (gethash key (top :value)) value))
			    (:plist      (setf (getf (top :value) key) value))
			    (:alist      (push (cons key value) (top :value))))
			  (setf (car map-keys) nil))
			(setf (car map-keys) value)))

		     (:vector
		      (case vector-as
			(:array (vector-push-extend value (top :value)))
			(:list  (push value (top :value)))))

		     (:list
		      (case list-as
			(:array (vector-push-extend value (top :value)))
			(:list  (push value (top :value)))))

		     (:set
		      (pushnew value (top :value)))

		     (:tagged
		      (setf (top :value) value)
		      (end))

		     (otherwise (setf result value))))))

	(loop for char = (or reuse (read-char in nil))
	      while (and char (not result))
	      do (debug char)
		 (when reuse (setf reuse nil))
		 (case (top :type)

		   (:list       (if (char= char #\)) (end) (begin char)))
		   (:vector     (if (char= char #\]) (end) (begin char)))
		   ((:map :set) (if (char= char #\}) (end) (begin char)))
                   (:string     (if (char= char #\") (end) (write-top char)))

		   (:number
		    (if (member char *end-of-symbol*)
			(progn (end) (setf reuse char))
			(if (or (digit-char-p char) (char= char #\.))
			    (write-top char)
			    (syntax-error "number" nil char))))

                   ((:keyword :symbol)
		    (if (member char *end-of-symbol*)
			(progn (end) (setf reuse char))
			(write-top char)))

                   (:tagged
		    (if (top :tag)
			(begin char)
			(if (member char *tag-follow*)
			    (let ((tag (get-output-stream-string (top :value))))
			      (setf (top :tag) (intern tag "KEYWORD"))
			      (setf reuse char))
                            (write-top char))))

                   (:comment
		    (when (char= char #\newline)
		      (pop stack)))

		   (:discard
		    (when (or (and (top :value)
				   (member char *end-of-symbol*))
			      (or (member char *enclosing*)
				  (unless (member char *whitespace*)
				    (setf (top :value) t)
				    nil)))
		      (pop stack)
		      (setf reuse char)))

		   (otherwise (begin char))))
	result))))
