;;; -*- mode: lisp -*-
(defpackage :archive-system
  (:use :cl :asdf))
(in-package :archive-system)

(asdf:defsystem :archive
  :version "0.8.1"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :description "A package for reading and writing archive (tar, cpio, etc.) files."
  :depends-on (#+sbcl sb-posix trivial-gray-streams cl-fad)
  :components ((:file "package")
               (:file "conditions" :depends-on ("package"))
               (:file "generics" :depends-on ("package"))
               (:file "macros" :depends-on ("generics"))
               (:file "formats" :depends-on ("macros"))
               (:file "stream" :depends-on ("package"))
               (:file "archive" :depends-on ("generics" "stream" "macros"))
               (:file "compat" :depends-on ("package"))
               (:file "tar" :depends-on ("compat" "formats" "generics" "archive"))
               (:file "cpio" :depends-on ("compat" "formats" "generics" "archive"))
               (:static-file "README")
               (:static-file "TODO")
               (:static-file "NEWS")
               (:static-file "LICENSE")))

(defmethod perform :around ((o compile-op) (c (eql (find-system 'archive))))
  (let ((use-sb-posix #+(and sbcl (not win32)) t))
    (if use-sb-posix
        (let ((*features* (cons :use-sb-posix *features*)))
          (call-next-method))
        (call-next-method))))
