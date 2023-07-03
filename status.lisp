(in-package #:org.shirakumo.simple-tasks)

(defvar +status-started+ '(:created :scheduled :running))
(defvar +status-running+ '(:running))
(defvar +status-ended+ '(:completed :errored :stopped))

(defgeneric status (status-object))
(defgeneric status= (a b))

(defclass status-object ()
  ((status :initform :created :accessor status)))

(defun status-list-p (list)
  (every #'symbolp list))

(deftype status ()
  '(or symbol status-object (and list (satisfies status-list-p))))

(defmethod status= ((a status-object) (b status-object))
  (eql (status a) (status b)))

(defmethod status= ((a status-object) (b list))
  (loop for status in b thereis (eql (status a) status)))

(defmethod status= ((a status-object) b)
  (eql (status a) b))

(defmethod status= ((b list) (a status-object))
  (status= a b))

(defmethod status= (b (a status-object))
  (status= a b))

(defmethod status= ((a list) (b list))
  (loop for status in a thereis (find status b :test #'eql)))
