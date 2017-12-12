#|
This file is a part of simple-tasks
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.simple-tasks)

(defun checkdocs (&optional (package *package*))
  "Check that all functions, classes, and variables have docstrings."
  (do-symbols (symb package)
    (when (eql (symbol-package symb) package)
      (when (and (fboundp symb) (not (documentation symb 'function)))
        (warn "No documentation for function ~s." symb))
      (when (and (boundp symb) (not (documentation symb 'variable)))
        (warn "No documentation for variable ~s." symb))
      (when (and (find-class symb NIL) (not (documentation symb 'type)))
        (warn "No documentation for class ~s." symb)))))

(defmacro setdocs (&body pairs)
  "Easily set the documentation."
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
   "Run the given task object directly.

One restart is established:
STOP    to forcibly stop (interrupt) the task. Assigns the :STOPPED status.")

  (interrupt-task
   "Interrupt the TASK to stop it from execution on RUNNER.

If the task is currently on the RUNNER's queue to be executed (:SCHEDULED), it
is removed from the queue. If the task is currently running, it is forcibly
aborted. In either case, the task's status is changed to :STOPPED and it 
will not execute further. On systems without thread support this does nothing.

If this is called with the RUNNER being T, the current runner of the TASK
is used, if possible. If this is called with the RUNNER being NIL, the
actual termination mechanism for the task is performed, leading it to be
terminated.")

  ((*runner* variable)
   "Bound to the current runner if within a runner context.
Otherwise, set to NIL. Useful to detect if a task is run in
a particular runner to avoid conflicts.")
 
  ((runner type)
   "Basic runner. Runs task as soon as scheduled.")

  (queue
   "The current task queue of the runner.

Do not directly push tasks to this! Use SCHEDULE-TASK instead.
This queue is also NOT indicative of which tasks have yet to be run,
or which ones have. When the queued runner runs, it retains the current
queue for processing and sets a new, empty queue on the runner. As such,
when you look at the queue at any particular moment, tasks that are not
in it might not have run yet.")

  (back-queue
   "The current back-queue of the runner.

Used to swap with QUEUE when events are handled.

See QUEUE")
  
  (lock
   "The lock used to coordinate task scheduling with the runner.

See QUEUED-RUNNER")
  
  (cvar
   "The condition variable used to exchange signals.

See QUEUED-RUNNER
See BLOCKING-CALL-TASK")

  (thread
   "Returns the current thread associated with the queued-runner if any.

See QUEUED-RUNNER")
 
  ((queued-runner type)
   "Queued runner. Runs tasks in a thread if threading is available.
Noe that START-RUNNER for this runner will block the current thread.")

  ((*current-queue* variable)
   "Bound to either NIL or the current queue being processed by the queued-runner.

See QUEUED-RUNNER")

  ((*current-task* variable)
   "Bound to either NIL or the current task being processed by the queued-runner.")
  
  (make-runner-thread
   "Make a thread to call START-RUNNER on RUNNER in.

On platforms with thread support, this returns the new thread.
On platforms without, this simply calls START-RUNNER and returns NIL.

See START-RUNNER"))

;; status.lisp
(setdocs
  ((+status-started+ variable)
   "Constant matching any started (not necessarily running) status.

:CREATED     Object has been initialized, but not started.
:SCHEDULED   Object has been scheduled for execution.
:RUNNING     Object is currently executing.")

  ((+status-running+ variable)
   "Constant matching a running status.

:RUNNING     Object is currently executing.")

  ((+status-ended+ variable)
   "Constant matching any ended (not necessarily successful) status.

:STOPPING    Object is in the process of being stopped.
:STOPPED     Object has been stopped.
:COMPLETED   Object successfully completed execution.
:ERRORED     Object ended execution with an error.")
  
  (status
   "Current status indicator of the status-object.")

  (status=
   "Compare two statuses with each other (commutative).")

  ((status-object type)
   "A class that has a status.")

  (status-list-p
   "Returns true if every element in the list is a symbol and the list thus qualifies as a STATUS.")

  ((status type)
   "A status can be a SYMBOL, a STATUS-OBJECT, or a LIST composed of SYMBOLs."))

;; task.lisp
(setdocs
  (task
   "The task related to the condition.")
  
  ((task-condition type)
   "Condition superclass for task operation related conditions.")

  ((task-already-scheduled type)
   "Condition signalled when attempting to reschedule an already scheduled task.")

  ((task-errored type)
   "Condition signalled when a task failed to run properly.")
  
  ((task type)
   "Basic task class.")
  
  (runner
   "The runner the task is scheduled on.

See TASK")

  (await
   "Wait for the TASK to match a certain STATUS.

On systems without thread support this does nothing.

See STATUS=")

  (error-environment
   "An environment object that is stored in case the task fails to run properly.

See DISSECT:ENVIRONMENT")

  (task-ready-p
   "Returns T if the task is ready to be run.")
  
  ((call-task type)
   "Task class to perform a function call once run. Stores the return values.")
  
  (func
   "The function the call-task calls once it is run.

See CALL-TASK")
  
  (return-values
   "Returns the values that the call returned.

See CALL-TASK")

  ((notifying-task type)
   "A task that will notify a condition upon completion.

This is particularly useful in conjunction with AWAIT.")

  ((blocking-task type)
   "A task that will block after being scheduled until it is done or interrupted.

When SCHEDULE-TASK is called on this, it establishes two restarts:
ABORT    to forcibly abort (interrupt) the task.
UNBLOCK  to resume execution in the current thread and leave the task running.

If a restart or similar exit functionality is invoked that leaves the scope of
SCHEDULE-TASK, the task is interrupted.

Note that the restarts are not useful on systems without thread support. In such
a case the task will be interrupted either way, due to the very nature of running
in the current thread.

See AWAIT
See INTERRUPT-TASK
See NOTIFYING-TASK")
  
  ((blocking-call-task type)
   "Task class to perform a function call once run.
Blocks the scheduling thread until it is done.

See CALL-TASK
See BLOCKING-TASK")
  
  (call-as-task
   "Call function within a task, usually a BLOCKING-CALL-TASK.
Depending on the task's STATUS after SCHEDULE-TASK returns, the following happens.
  :COMPLETED  The task's return values are returned.
  :ERRORED    A condition of type TASK-ERRORED is signalled.
  T           The task is returned.

See SCHEDULE-TASK
See BLOCKING-CALL-TASK")
  
  ((with-body-as-task)
   "Evaluate BODY within a task, usually a BLOCKING-CALL-TASK.

See CALL-AS-TASK")
  
  ((callback-task type)
   "Task class to perform a function call once run and call a callback upon completion.

If the task completes successfully, the callback function is called with each return
value as an argument. Note that the callback function is called within the runner
environment which may be different from the scheduler environment.

See CALL-TASK")
  
  (callback
   "The function to call upon completion of the task.

See CALLBACK-TASK"))
;; toolkit.lisp
(setdocs
  ((no-threading-stump type)
   "Stump class to stand in place of a value on systems without threading support.")

  ((+no-threading-stump+ variable)
   "Constant to hold an instance of NO-THREADING-STUMP."))
