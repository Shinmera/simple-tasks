#|
This file is a part of simple-tasks
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.simple-tasks)

(define-condition task-condition (condition)
  ((task :initarg :task :accessor task))
  (:default-initargs :task (error "TASK required.")))

(define-condition task-already-scheduled (task-condition error) ()
  (:report (lambda (c s) (format s "Task ~s already scheduled on ~s."
                                 (task c) (runner (task c))))))

(define-condition task-errored (task-condition warning) ()
  (:report (lambda (c s) (format s "Task ~s errored. See the task's ERROR-ENVIRONMENT for more information."
                                 (task c)))))

(defgeneric runner (task))

(defclass task ()
  ((status :initform :created :accessor status)
   (runner :initform NIL :accessor runner)
   (error-environment :initform NIL :accessor error-environment)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type T :identity T)
    (format stream ":STATUS ~s" (status task))))

(defmethod schedule-task :before ((task task) runner)
  (when (runner task)
    (cerror "Schedule anyway." 'task-already-scheduled :task task))
  (setf (runner task) runner))

(defmethod run-task :around ((task task))
  (handler-bind ((error (lambda (err)
                          (setf (status task) :errored)
                          (setf (error-environment task) (dissect:capture-environment err)))))
    (setf (status task) :running)
    (call-next-method)
    (setf (status task) :completed)))

(defgeneric func (call-task))

(defgeneric return-values (call-task))

(defclass call-task (task)
  ((func :initarg :func :accessor func)
   (return-values :initform NIL :accessor return-values))
  (:default-initargs
   :func (error "FUNC required.")))

(defmethod return-values :around ((task call-task))
  (apply #'values (call-next-method)))

(defmethod print-object ((task call-task) stream)
  (print-unreadable-object (task stream :type T :identity T)
    (format stream ":FUNC ~s :STATUS ~s" (func task) (status task))))

(defmethod run-task ((task call-task))
  (setf (return-values task)
        (multiple-value-list (funcall (func task)))))

(defclass blocking-call-task (call-task)
  ((lock :initarg :lock :accessor lock)
   (cvar :initarg :cvar :accessor cvar))
  (:default-initargs
   :lock #-:thread-support *no-threading-stump* #+:thread-support (bt:make-lock "call-task")
   :cvar #-:thread-support *no-threading-stump* #+:thread-support (bt:make-condition-variable :name "call-task")))

#+:thread-support
(defmethod schedule-task :after ((task blocking-call-task) runner)
  (loop while (find (status task) '(:created :running))
        do (bt:with-lock-held ((lock task))
             (bt:condition-wait (cvar task) (lock task)))))

#+:thread-support
(defmethod run-task :around ((task blocking-call-task))
  (unwind-protect
       (call-next-method)
    (bt:thread-yield)
    (bt:with-lock-held ((lock task))
      (bt:condition-notify (cvar task)))))

(defun call-as-task (function runner &optional (task-class 'blocking-call-task))
  (let ((task (make-instance task-class :func function)))
    (schedule-task task runner)
    (case (status task)
      (:completed (return-values task))
      (:errored (warn 'task-errored :task task) (values))
      (T task))))

(defmacro with-body-as-task ((runner &optional (task-class ''blocking-call-task)) &body body)
  
  `(call-as-task (lambda () ,@body) ,runner ,task-class))

(defgeneric callback (callback-task))

(defclass callback-task (call-task)
  ((callback :initarg :callback :accessor callback))
  (:default-initargs
   :callback (error "CALLBACK required.")))

(defmethod run-task :after ((task callback-task))
  (apply (callback task) (return-values task)))
