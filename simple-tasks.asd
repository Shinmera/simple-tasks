#|
This file is a part of simple-tasks
(c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem simple-tasks
  :version "1.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A very simple task scheduling framework."
  :homepage "https://github.com/Shinmera/simple-tasks"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "runner")
               (:file "task")
               (:file "documentation"))
  :depends-on (:bordeaux-threads
               :dissect))
