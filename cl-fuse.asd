; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((loaded-file (truename (or *compile-file-pathname*
                                    *load-pathname*)))
         (target-dir (pathname-directory loaded-file))
         (lib (make-pathname :directory target-dir
                             :name "libfuse-launcher"
                             :type "so"))
         (source (make-pathname :directory target-dir
                                :name "fuse-launcher"
                                :type "c-minus")))
    (format *error-output* "Lib: ~s ~s~%" lib (probe-file lib))
    (unless (and (probe-file lib)
                 (<= (file-write-date source)
                     (file-write-date lib)))
      (uiop:run-program `("gcc"
                          "-x"
                          "c"
                          ,(namestring source)
                          "-fPIC"
                          "--shared"
                          "-lfuse"
                          "-o"
                          ,(namestring lib))))))

(asdf:defsystem :cl-fuse
  :name "cl-fuse"
  :depends-on (:cffi
               :cl-utilities
               :bordeaux-threads
               :trivial-backtrace
               :iterate
               :trivial-utf-8)
  :defsystem-depends-on (:cffi-grovel)
  :author "Michael Raskin <fb08af68@rambler.ru>"
  :maintainer "Michael Raskin <fb08af68@rambler.ru>"
  :license "LLGPL"
  :description "CFFI bindings to FUSE (Filesystem in user space)"
  :serial t
  :components ((:file "package")
               (:file "encoding-helper")
               (:cffi-grovel-file "fuse-types")
               (:file "fuse-functions")
               (:file "default-callbacks")
               (:file "fuse-wrapper-helpers")
               (:file "getattr-helpers")
               (:file "fuse-wrapper")))
