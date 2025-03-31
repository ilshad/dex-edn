(in-package #:dex-edn)

(defclass wrapper () ((value :initarg :value :accessor value)))

(defmethod print-object ((object wrapper) stream)
  (print-unreadable-object (object stream :type t)
    (princ (value object) stream)))

(defclass edn-set (wrapper) ())
(defclass edn-uuid (wrapper) ())
(defclass edn-inst (wrapper) ())

(defun make-set (list) (make-instance 'edn-set :value list))
(defun make-uuid (list) (make-instance 'edn-uuid :value list))
(defun make-inst (list) (make-instance 'edn-inst :value list))
