(asdf:defsystem simple-tasks
  :version "1.3.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A very simple task scheduling framework."
  :homepage "https://shinmera.com/docs/simple-tasks/"
  :bug-tracker "https://shinmera.com/project/simple-tasks/issues"
  :source-control (:git "https://shinmera.com/project/simple-tasks.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "status")
               (:file "runner")
               (:file "task")
               (:file "documentation"))
  :depends-on (:bordeaux-threads
               :array-utils
               :dissect))
