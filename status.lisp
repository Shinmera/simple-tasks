#|
This file is a part of simple-tasks
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.simple-tasks)

(defvar +status-started+ '(:created :running))
(defvar +status-running+ '(:running))
(defvar +status-ended+ '(:completed :errored :stopped))

(defgeneric status (status-object))
(defgeneric status= (a b))

(defclass status-object ()
  ((status :initform :created :accessor status)))

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
