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
  ((runner-condition type)
   "Condition superclass for conditions related to runner operations.")

  ((runner-not-started type)
   "Condition signalled when the runner is not yet started, but has to be.")

  ((runner-not-stopped type)
   "Condition signalled when the runner did not stop properly.")
  
  (start-runner
   "Start the the runner.")
  
  (stop-runner
   "Stop the the runner.")
  
  (schedule-task
   "Schedule the task object for running.
The task may or may not be run immediately, depending on the runner and given
system support. Tasks are guaranteed to be run in the same order as they are
scheduled.

See RUN-TASK")
  
  (run-task
   "Run the given task object directly.")
 
  ((runner type)
   "Basic runner. Runs task as soon as scheduled.")
  
  
  (status
   "Current status indicator of the runner/task.
The following statuses are defined:

:CREATED     Object has been initialized, but not started.
:RUNNING     Object is currently executing.
:STOPPING    Runner is in the process of being stopped.
:STOPPED     Runner has been stopped.
:COMPLETED   Task successfully completed execution.
:ERRORED     Task ended execution with an error.")
 
  ((queued-runner type)
   "Queued runner. Runs tasks in a thread if threading is available.
Noe that START-RUNNER for this runner will block the current thread.")
  
  (queue
   "The current task queue of the runner.

Do not directly push tasks to this! Use SCHEDULE-TASK instead.
This queue is also NOT indicative of which tasks have yet to be run,
or which ones have. When the queued runner runs, it retains the current
queue for processing and sets a new, empty queue on the runner. As such,
when you look at the queue at any particular moment, tasks that are not
in it might not have run yet.")
  
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
  ((task-condition type)
   "Condition superclass for task operation related conditions.")

  (task
   "The task related to the condition.")

  ((task-already-scheduled type)
   "Condition signalled when attempting to reschedule an already scheduled task.")

  ((task-errored type)
   "Condition signalled when a task failed to run properly.")
  
  ((task type)
   "Basic task class.")
  
  (runner
   "The runner the task is scheduled on.

See TASK")

  (error-environment
   "An environment object that is stored in case the task fails to run properly.

See DISSECT:ENVIRONMENT")
  
  ((call-task type)
   "Task class to perform a function call once run. Stores the return values.")
  
  (func
   "The function the call-task calls once it is run.

See CALL-TASK")
  
  (return-values
   "Returns the values that the call returned.

See CALL-TASK")
  
  ((blocking-call-task type)
   "Task class to perform a function call once run.
Blocks the scheduling thread until it is done.")
  
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
