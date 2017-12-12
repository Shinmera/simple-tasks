#|
This file is a part of simple-tasks
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.simple-tasks)

(defgeneric task (task-condition))

(define-condition task-condition (condition)
  ((task :initarg :task :accessor task))
  (:default-initargs :task (error "TASK required.")))

(define-condition task-already-scheduled (task-condition error) ()
  (:report (lambda (c s) (format s "Task ~s already scheduled on ~s."
                                 (task c) (runner (task c))))))

(define-condition task-errored (task-condition warning) ()
  (:report (lambda (c s) (format s "Task ~s errored. See the task's ERROR-ENVIRONMENT for more information."
                                 (task c)))))

(defgeneric task-ready-p (task))
(defgeneric runner (task))
(defgeneric error-environment (task))
(defgeneric await (task status))

(defclass task (status-object)
  ((runner :initform NIL :accessor runner)
   (error-environment :initform NIL :accessor error-environment)))

(defmethod task-ready-p ((task task))
  (status= task '(:created :scheduled)))

(defmethod print-object ((task task) stream)
  (print-unreadable-object (task stream :type T :identity T)
    (format stream ":STATUS ~s" (status task))))

(defmethod schedule-task :before ((task task) runner)
  (when (and (runner task) (not (task-ready-p task)))
    (cerror "Schedule anyway." 'task-already-scheduled :task task))
  (setf (runner task) runner)
  (setf (status task) :scheduled))

(defmethod run-task :around ((task task))
  (when (task-ready-p task)
    (restart-case
        (handler-bind ((error (lambda (err)
                                (setf (status task) :errored)
                                (setf (error-environment task) (dissect:capture-environment err)))))
          (setf (status task) :running)
          (multiple-value-prog1
              (call-next-method)
            (setf (status task) :completed)))
      (stop ()
        :report "Stop the task."
        (setf (status task) :stopped)))))

(defmethod await ((task task) status)
  #+:thread-support
  (loop until (status= task status)
        do (bt:thread-yield))
  task)

(defmethod interrupt-task ((task task) (null null))
  (when (and (status= task :running) (find-restart 'stop))
    (setf (status task) :stopping)
    (invoke-restart 'stop)))

(defmethod interrupt-task ((task task) (true (eql T)))
  (when (runner task)
    (interrupt-task task (runner task))))

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

(defclass notifying-task (task)
  ((lock :initarg :lock :accessor lock)
   (cvar :initarg :cvar :accessor cvar))
  (:default-initargs
   :lock #-:thread-support +no-threading-stump+ #+:thread-support (bt:make-lock "notifying-task")
   :cvar #-:thread-support +no-threading-stump+ #+:thread-support (bt:make-condition-variable :name "notifying-task")))

#+:thread-support
(defmethod run-task :around ((task notifying-task))
  (unwind-protect
       (call-next-method)
    ;; Make sure we notify about our exit.
    (bt:thread-yield)
    (bt:condition-notify (cvar task))))

#+:thread-support
(defmethod await ((task notifying-task) status)
  (loop until (status= task status)
        do (bt:with-lock-held ((lock task))
             (bt:condition-wait (cvar task) (lock task)))))

(defclass blocking-task (notifying-task)
  ())

(defmethod schedule-task :around ((task blocking-task) runner)
  (let ((interrupt T))
    (unwind-protect
         (restart-case
             (call-next-method)
           (abort ()
             :report "Abort the task.")
           (unblock ()
             :report "Leave the task running and unblock this thread."
             (setf interrupt NIL)))
      ;; If we exit from this without having INTERRUPT set to NIL by UNBLOCK,
      ;; we most likely exited through a restart or some other functionality
      ;; and actually want to interrupt the task to properly simulate the
      ;; same behaviour as we would have if the task were actually running in
      ;; the current thread.
      (when (and interrupt (eql (status task) :running))
        (interrupt-task task runner))))
  task)

(defmethod schedule-task ((task blocking-task) runner)
  ;; Detect if in own runner. If so, just run directly as we'd deadlock otherwise.
  (if (eql *runner* runner)
      (run-task task)
      (call-next-method)))

(defmethod schedule-task :after ((task blocking-task) runner)
  (await task +status-ended+))

(defclass blocking-call-task (call-task blocking-task)
  ())

(defun call-as-task (function runner &optional (task-class 'blocking-call-task))
  (let ((task (make-instance task-class :func function)))
    (schedule-task task runner)
    (case (status task)
      (:completed (return-values task))
      (:errored (error 'task-errored :task task))
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
