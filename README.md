## About simple-tasks
Recently I noticed that I seem to write a similar pattern way too often: Some form of scheduling or task management part.
Usually this is due to the need of executing some task in a specific thread, or simply wanting to schedule something for processing in the background.

## Basic Usage
Load it through ASDF or Quicklisp:

    (ql:quickload :simple-tasks)

Before we can do anything, we need to have a manager to run tasks with. These are called `runner`s. Usually you will want a `queued-runner` which will use threading capabilities if the system provides them, and thus can process tasks in the background.

    (defvar *runner* (make-instance 'simple-tasks:queued-runner))

Next we'll want to start the runner, if possible in a separate thread. The standard `start-runner` function does not do this, as you might want to set up the thread scenario yourself, or perhaps even inject the runner into a different thread. However, for the common scenario, there's `make-runner-thread`.

    (defvar *thread* (simple-tasks:make-runner-thread *runner*))

Now we can finally create `task`s and send them off for processing! Out of the box, simple-tasks provides three classes to do remote evaluation with: `call-task`, `blocking-call-task`, and `callback-task`. However, adding your own is very easy. Just subclass `task` and implement `run-task`.

    (defvar *task* (make-instance 'simple-tasks:call-task :func (lambda () (print "hi"))))

Finally we need to send off the task for processing.

    (simple-tasks:schedule-task *task* *runner*)

All `call-task`s also save the `return-values` of the function they're calling, so you can read that out later. Usually if you want to do so, you'll want to either block until the task is done, or delegate to a callback function. The former is made convenient through `with-body-as-task`.

    (simple-tasks:with-body-as-task (*runner*)
      (sleep 1)
      (expt 2 12))

In the case that a task encounters a failure during a run, it sets its status to `:errored` and saves the current environment. You can inspect this environment at any later point by fetching it with `error-environment` and looking at its contents with the various functions Dissect provides. This is mostly useful in a scenario where you cannot use a debugger and thus just automatically invoke the `continue` restart in the runner thread. With the environment saved, the error can still be inspected elsewhere or at a later date.

    (dissect:present
      (simple-tasks:error-environment
        (simple-tasks:schedule-task 
          (make-instance 'simple-tasks:blocking-call-task :func (lambda () (error "Hi"))) *runner*)))

And that's pretty much it.

    (simple-tasks:stop-runner *runner*)

## Extending simple-tasks
If you want to add flexibility by creating your own specialised task classes, you should look at `task`, `run-task`, and `schedule-task`. Usually you can get away by just subclassing `task`, and adding a `run-task` method to do your calculations in. If you also need to modify the behaviour of the scheduling, adding `:after` methods to `schedule-task` can also be useful.

In case that the existing runners aren't suited to your needs, adding one should also not be much of a problem. Simply subclass `runner`, and implement appropriate methods for `start-runner`, ` stop-runner`, and `schedule-task`. The existing methods on `runner` will take care of keeping the `status` in sync and making sure no invalid calls can be made.

## Also See

* [Dissect](https://shinmera.github.io/dissect) for error environment capture and inspection.
