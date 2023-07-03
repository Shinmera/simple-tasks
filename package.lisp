(in-package #:cl-user)
(defpackage #:simple-tasks
  (:nicknames #:org.shirakumo.simple-tasks)
  (:use #:cl)
  ;; runner.lisp
  (:export
   #:runner-condition
   #:runner
   #:runner-not-started
   #:runner-not-stopped
   
   #:start-runner
   #:stop-runner
   #:schedule-task
   #:run-task
   #:interrupt-task

   #:*runner*
   #:runner

   #:queue
   #:back-queue
   #:lock
   #:cvar
   #:thread

   #:queued-runner

   #:abort
   #:skip
   
   #:make-runner-thread)
  ;; status.lisp
  (:export
   #:+status-started+
   #:+status-running+
   #:+status-ended+
   
   #:status
   #:status=
   
   #:status-object
   #:status)
  ;; task.lisp
  (:export
   #:task
   #:task-condition
   #:task-already-scheduled
   #:task-errored

   #:task-ready-p
   #:runner
   #:await
   #:error-environment
   
   #:task

   #:stop
   
   #:func
   #:return-values
   
   #:call-task

   #:notifying-task

   #:blocking-task
   #:unblock
   
   #:blocking-call-task

   #:call-as-task
   #:with-body-as-task

   #:callback-task
   #:callback)
  ;; task.lisp
  (:export
   #:+no-threading-stump+
   #:no-threading-stump))
