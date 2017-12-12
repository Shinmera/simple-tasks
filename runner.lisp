#|
This file is a part of simple-tasks
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.simple-tasks)

(define-condition runner-condition (condition)
  ((runner :initarg :runner :accessor runner))
  (:default-initargs :runner (error "RUNNER required.")))

(define-condition runner-not-started (runner-condition error) ()
  (:report (lambda (c s) (format s "Runner ~s is not started."
                                 (runner c)))))

(define-condition runner-not-stopped (runner-condition warning) ()
  (:report (lambda (c s) (format s "Runner ~s did not stop."
                                 (runner c)))))

(defgeneric start-runner (runner))
(defgeneric stop-runner (runner))
(defgeneric schedule-task (task runner))
(defgeneric run-task (task))
(defgeneric interrupt-task (task runner))

(defvar *current-task* NIL)
(defvar *runner* NIL)
(defclass runner (status-object)
  ())

(defmethod print-object ((runner runner) stream)
  (print-unreadable-object (runner stream :type T :identity T)
    (format stream "~s ~s" :status (status runner))))

(defmethod start-runner :before ((runner runner))
  (when (eql (status runner) :running)
    (cerror "Start anyway." "Runner ~s is already running!" runner))
  (setf (status runner) :running))

(defmethod start-runner :around ((runner runner))
  (let ((*runner* runner))
    (call-next-method)))

(defmethod start-runner ((runner runner)) runner)

(defmethod stop-runner :before ((runner runner))
  (unless (eql (status runner) :running)
    (cerror "Stop anyway." "Runner ~s is not running!" runner))
  (setf (status runner) :stopped))

(defmethod stop-runner ((runner runner)) runner)

(defmethod schedule-task :before (task (runner runner))
  (unless (eql (status runner) :running)
    (error 'runner-not-started :runner runner)))

(defmethod schedule-task (task (runner runner))
  (when (task-ready-p task)
    (let ((*current-task* task))
      (run-task task)))
  task)

(defmethod interrupt-task (task (runner runner))
  task)

(defgeneric queue (runner))
(defgeneric back-queue (runner))
(defgeneric lock (object))
(defgeneric cvar (object))
(defgeneric thread (object))

(defclass queued-runner (runner)
  ((queue :initarg :queue :reader queue :writer %set-queue)
   (back-queue :initarg :back-queue :reader back-queue :writer %set-back-queue)
   (lock :initarg :lock :reader lock)
   (cvar :initarg :cvar :reader cvar)
   (thread :initarg :thread :reader thread :writer %set-thread))
  (:default-initargs
   :queue (make-array 100 :adjustable T :fill-pointer 0)
   :back-queue (make-array 100 :adjustable T :fill-pointer 0)
   :lock #-:thread-support +no-threading-stump+ #+:thread-support (bt:make-lock "task-runner-queue-lock")
   :cvar #-:thread-support +no-threading-stump+ #+:thread-support (bt:make-condition-variable :name "task-runner-condition")
   :thread #-:thread-support +no-threading-stump+ #+:thread-support NIL))

(defvar *current-queue* NIL)
#+:thread-support
(defmethod start-runner ((runner queued-runner))
  (%set-thread (bt:current-thread) runner)
  (let ((lock (lock runner))
        (cvar (cvar runner)))
    (unwind-protect
         (with-simple-restart (stop-runner "Stop the runner ~a entirely." runner)
           (with-simple-restart (abort "Stop the runner ~a entirely." runner)
             (bt:acquire-lock lock)
             (loop while (eql (status runner) :running)
                   do (let ((*current-queue* (queue runner)))
                        (%set-queue (back-queue runner) runner)
                        (%set-back-queue *current-queue* runner)
                        (setf (fill-pointer (queue runner)) 0)
                        (bt:release-lock lock)
                        (loop for task across *current-queue*
                              do (let ((*current-task* task))
                                   (with-simple-restart (skip "Skip running ~a" task)
                                     (run-task task)))))
                      (bt:acquire-lock lock)
                      (loop while (= 0 (length (queue runner)))
                            do (unless (bt:condition-wait cvar lock)
                                 (bt:acquire-lock lock))))))
      (ignore-errors (bt:release-lock lock))
      (%set-thread NIL runner)
      (setf (status runner) :stopped)))
  runner)

#+:thread-support
(defmethod stop-runner ((runner queued-runner))
  (setf (status runner) :stopping)
  (cond ((eq *runner* runner)
         (invoke-restart 'stop-runner))
        (T
         (bt:condition-notify (cvar runner))
         (bt:thread-yield)
         (loop for i from 0 to 500
               do (if (eql (status runner) :stopped)
                      (return)
                      (sleep 0.01))
               finally (warn 'runner-not-stopped :runner runner))
         runner)))

#+:thread-support
(defmethod schedule-task (task (runner queued-runner))
  (bt:with-lock-held ((lock runner))
    (vector-push-extend task (queue runner)))
  (bt:condition-notify (cvar runner))
  task)

#+:thread-support
(defmethod interrupt-task (task (runner queued-runner))
  (bt:interrupt-thread
   (thread runner)
   (lambda ()
     (with-simple-restart (abort "Abort trying to interrupt ~a on ~a" task runner)
       (flet ((clean-vector (vector)
                (loop for el across vector
                      for i from 0
                      when (eql el task)
                      do (setf (status task) :stopped)
                         (return (array-utils:vector-pop-position vector i)))))
         ;; Search runner queue, making sure we're locked while we do it.
         (bt:with-lock-held ((lock runner))
           (clean-vector (queue runner)))
         ;; Search internal queue
         (when *current-queue*
           (clean-vector *current-queue*))
         ;; Potentially abort if running
         (when *current-task*
           (when (or (status= task :running) (eql task *current-task*))
             (interrupt-task task NIL)))))))
  task)

(defun make-runner-thread (runner)
  #+:thread-support
  (prog1
      (bt:make-thread
       (lambda () (start-runner runner))
       :name "runner thread"
       :initial-bindings (append `((*standard-output* . ,*standard-output*)
                                   (*error-output* . ,*error-output*))
                                 bt:*default-special-bindings*))
    (loop until (eql (status runner) :running)
          do (bt:thread-yield)))
  #-:thread-support
  (progn
    (start-runner runner)
    NIL))
