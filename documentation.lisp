#|
This file is a part of simple-tasks
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.simple-tasks)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

;; runner.lisp
(setdocs
  (start-runner
   "Start the the runner.")
  
  (stop-runner
   "Stop the the runner.")
  
  (schedule-task
   "Schedule the task object for running.
The task may or may not be run immediately, depending on the runner and given system support.

See RUN-TASK")
  
  (run-task
   "Run the given task object directly.")
 
  ((runner type)
   "Basic runner. Runs task as soon as scheduled.")
  
  
  (status
   "Current status indicator of the runner/task.
The following statuses are defined:

:CREATED Object has been initialized, but not started.
:RUNNING Object is currently executing.
:STOPPING Runner is in the process of being stopped.
:STOPPED Object has finished execution.
:ERRORED Object ended execution with an error.")
 
  ((queued-runner type)
   "Queued runner. Runs tasks in a thread if threading is available.
Noe that START-RUNNER for this runner will block the current thread.")
  
  (queue
   "The current task queue of the runner.
Do not directly push tasks to this! Use SCHEDULE-TASK instead.")
  
  (lock
   "The lock used to synchronise operations with the object.")
  
  (cvar
   "The condition variable used to exchange signals.

See QUEUED-RUNNER
See BLOCKING-CALL-TASK")
  
  (make-runner-thread
   "Make a thread to call START-RUNNER on RUNNER in.

On platforms with thread support, this returns the new thread.
On platforms without, this simply calls START-RUNNER and returns NIL.

See START-RUNNER"))

;; task.lisp
(setdocs
  ((task type)
   "Basic task class.")
  
  (runner
   "The runner the task is scheduled on.

See TASK")
  
  ((call-task type)
   "Task class to perform a function call once run. Stores the return values.")
  
  (func
   "The function the call-task calls once it is run.

See CALL-TASK")
  
  (return-values
   "A list of the return values that the call returned.

See CALL-TASK")
  
  ((blocking-call-task type)
   "Task class to perform a function call once run. Blocks the scheduling thread until it is done.")
  
  (call-as-task
   "Call function within a task, usually a BLOCKING-CALL-TASK.
Returns the values that have been saved in the task after SCHEDULE-TASK returns.

See SCHEDULE-TASK
See BLOCKING-CALL-TASK")
  
  ((with-body-as-task)
   "Evaluate BODY within a task, usually a BLOCKING-CALL-TASK.

See CALL-AS-TASK")
  
  ((callback-task type)
   "Task class to perform a function call once run and call a callback upon completion.
The callback function is called with each return value as an argument.
Note that the callback function is called within the runner environment
which may be different from the scheduler environment.")
  
  (callback
   "The function to call upon completion of the task.

See CALLBACK-TASK"))
;; task.lisp
(setdocs
  ((no-threading-stump type)
   "Stump class to stand in place of a value on systems without threading support."))
