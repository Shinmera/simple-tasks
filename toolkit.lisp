#|
This file is a part of simple-tasks
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.simple-tasks)

(defclass no-threading-stump ()
  ())

(defmethod print-object ((o no-threading-stump) stream)
  (print-unreadable-object (o stream)
    (format stream "NO THREADING PLACEHOLDER")))

(defvar +no-threading-stump+ (make-instance 'no-threading-stump))
