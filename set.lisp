(in-package #:dex-edn)

(defclass edn-set ()
  ((value :initarg :value :accessor value)))

(defun make-set (list)
  (make-instance 'edn-set :value list))

(defmethod print-object ((object edn-set) stream)
  (print-unreadable-object (object stream :type t)
    (princ (value object) stream)))
