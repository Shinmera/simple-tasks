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

(defclass runner (status-object)
  ())

(defmethod print-object ((runner runner) stream)
  (print-unreadable-object (runner stream :type T :identity T)
    (format stream ":STATUS ~s" (status runner))))

(defmethod start-runner :before ((runner runner))
  (when (eql (status runner) :running)
    (cerror "Start anyway." "Runner ~s is already running!" runner))
  (setf (status runner) :running))

(defmethod start-runner ((runner runner)))

(defmethod stop-runner :before ((runner runner))
  (unless (eql (status runner) :running)
    (cerror "Stop anyway." "Runner ~s is not running!" runner))
  (setf (status runner) :stopped))

(defmethod stop-runner ((runner runner)))

(defmethod schedule-task :before (task (runner runner))
  (unless (eql (status runner) :running)
    (error 'runner-not-started :runner runner)))

(defmethod schedule-task (task (runner runner))
  (when (task-ready-p task)
    (run-task task))
  task)

(defmethod interrupt-task (task (runner runner))
  task)

(defgeneric queue (runner))
(defgeneric lock (object))
(defgeneric cvar (object))
(defgeneric thread (object))

(defclass queued-runner (runner)
  ((queue :initarg :queue :reader queue :writer %set-queue)
   (lock :initarg :lock :reader lock)
   (cvar :initarg :cvar :reader cvar)
   (thread :initarg :thread :reader thread :writer %set-thread))
  (:default-initargs
   :queue (make-array 0 :adjustable T :fill-pointer 0)
   :lock #-:thread-support +no-threading-stump+ #+:thread-support (bt:make-lock "task-runner") 
   :cvar #-:thread-support +no-threading-stump+ #+:thread-support (bt:make-condition-variable :name "task-runner")
   :thread #-:thread-support +no-threading-stump+ #+:thread-support NIL))

(defvar *current-queue* NIL)
(defvar *current-task* NIL)
#+:thread-support
(defmethod start-runner ((runner queued-runner))
  (%set-thread (bt:current-thread) runner)
  (let ((lock (lock runner))
        (cvar (cvar runner)))
    (unwind-protect
         (with-simple-restart (abort "Stop the runner ~a entirely." runner)
           (bt:acquire-lock lock)
           (loop while (eql (status runner) :running)
                 do (let ((*current-queue* (queue runner)))
                      (%set-queue (make-array 0 :adjustable T :fill-pointer 0) runner)
                      (bt:release-lock lock)
                      (loop for task across *current-queue*
                            do (let ((*current-task* task))
                                 (with-simple-restart (skip "Skip running ~a" task)
                                   (run-task task)))))
                    (bt:acquire-lock lock)
                    (when (= 0 (length (queue runner)))
                      (bt:condition-wait cvar lock))))
      (ignore-errors (bt:release-lock lock))
      (%set-thread NIL runner)
      (setf (status runner) :stopped)))
  runner)

#+:thread-support
(defmethod stop-runner ((runner queued-runner))
  (setf (status runner) :stopping)
  (bt:condition-notify (cvar runner))
  (bt:thread-yield)
  (loop for i from 0 to 5
        do (if (eql (status runner) :stopped)
               (return)
               (sleep 1))
        finally (warn 'runner-not-stopped :runner runner))
  runner)

#+:thread-support
(defmethod schedule-task (task (runner queued-runner))
  (bt:with-lock-held ((lock runner))
    (vector-push-extend task (queue runner))
    (bt:condition-notify (cvar runner)))
  task)

#+:thread-support
(defmethod interrupt-task (task (runner queued-runner))
  (bt:interrupt-thread
   (thread runner)
   (lambda ()
     ;; Search runner queue
     (loop for el across (queue runner)
           when (eql el task)
           do (setf (status task) :completed))
     ;; Search internal queue
     (when *current-queue*
       (loop for el across *current-queue*
             when (eql el task)
             do (setf (status task) :completed)))
     ;; Potentially abort if running
     (when *current-task*
       (when (or (null task) (eql task *current-task*))
         (setf task *current-task*)
         (invoke-restart 'stop)))))
  task)

(defun make-runner-thread (runner)
  #+:thread-support
  (bt:make-thread
   (lambda () (start-runner runner))
   :name "runner thread"
   :initial-bindings (append `((*standard-output* . ,*standard-output*)
                               (*error-output* . ,*error-output*))
                             bt:*standard-io-bindings*
                             bt:*default-special-bindings*))
  #-:thread-support
  (progn
    (start-runner runner)
    NIL))
