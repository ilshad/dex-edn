(defpackage #:dex-edn/tests
  (:use #:common-lisp #:alexandria #:fiveam #:dex-edn)
  (:export #:all-tests))

(in-package #:dex-edn/tests)

(def-suite* tests)

(defparameter *path* (asdf:system-relative-pathname "dex-edn/tests" "tests/"))

(defun test-file (name)
  (make-pathname :name name :type "edn" :defaults *path*))

(defun test-content (name)
  (read-file-into-string (test-file name)))

(defun clean-content (string)
  (remove-if #'(lambda (char) (member char '(#\Newline #\Tab))) string))

(test decode-encode-vector
  (is (string= (clean-content (test-content "output-vector"))
	       (encode (decode (test-content "input-vector")
			       :wrap-p t)))))

(test decode-encode-map-plist
  (is (string= (clean-content (test-content "output-map-plist"))
	       (encode (decode (test-content "input-map")
			       :map-as :plist
			       :wrap-p t)
		       :plist-as-map-p t))))

(test decode-encode-map-alist
  (is (string= (clean-content (test-content "output-map-alist"))
	       (encode (decode (test-content "input-map")
			       :map-as :alist
			       :wrap-p t)
		       :alist-as-map-p t))))

(test decode-map-defaults
  (with-open-file (in (test-file "input-map"))
    (let ((x (decode in)))
      (macrolet ((is-get (predicate expected key)
		   `(is (,predicate ,expected (gethash ,key x)))))
	(is-get = 42 :int)
	(is-get string= "foo" :string)
        (is-get eq :bar :keyword)
	(is-get eq :|qualified.CamelCase/_Value| :|qualified.Name/key|)
	(is-get eq t :true)
        (is (not (gethash :false x)))
	(is (not (gethash :nil x)))
	(is-get string= "72be4ac3-923d-4878-95f7-ea2b6808744c" :uuid)
	(is-get string= "2025-03-30T20:24:19.264-00:00" :inst)
	(is-get equalp '(42 "bar" :foo) :set)
	(is-get equalp '(:foo "bar" 42) :list)
	(is-get equalp #(:foo "bar" 42) :vector)
	(let ((m (gethash :map x)))
	  (is (string= "bar" (gethash :_foo m)))
	  (is (= 42 (gethash :baz m))))
        (is (zerop (hash-table-count (gethash :empty-map x))))
	(is (zerop (length (gethash :empty-vector x))))
	(let* ((m (gethash :map-of-maps x))
	       (m1 (gethash "1797afd8-58f8-422e-9a74-a0e0711ecdfa" m))
	       (m2 (gethash "45b4a230-49b7-4b11-bd82-7e2c1f74c3bc" m)))
	  (is (string= "Foo" (gethash :str m1)))
	  (is (string= "Bar" (gethash :str m2)))
	  (is (= 1001 (gethash :int m1)))
	  (is (= 1002 (gethash :int m2))))
	(let* ((v (gethash :vector-of-maps x)))
	  (is (string= "Foo" (gethash :str (elt v 0))))
	  (is (string= "Bar" (gethash :str (elt v 1))))
	  (is (= 1001 (gethash :int (elt v 0))))
	  (is (= 1002 (gethash :int (elt v 1)))))))))
