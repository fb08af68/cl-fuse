; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(cl:eval-when (:load-toplevel :execute)
    (asdf:operate 'asdf:load-op 'cffi-grovel))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (let* (
         (loaded-file (truename (or 
                                 *compile-file-pathname*
                                 *load-pathname*
                                 )))
         (target-dir (pathname-directory loaded-file))
         (lib (make-pathname :directory target-dir
                             :name "libfuse-launcher"
                             :type "so"
                             ))
         (source (make-pathname :directory target-dir
                                :name "fuse-launcher"
                                :type "c-minus"
                                ))
         (grovel-source (make-pathname :directory target-dir
                                       :name "fuse.grovel"
                                       :type "lisp"
                                       ))
         (grovel-target (make-pathname :directory target-dir
                                       :name "fuse-grovel-result"
                                       :type "lisp"
                                       ))
         (grovel-real-target (make-pathname :directory target-dir
                                       :name "fuse-grovel-result.grovel-tmp"
                                       :type "lisp"
                                       ))
         )
        (format *error-output* "Lib: ~s ~s~%" lib (probe-file lib))
        (unless (and (probe-file lib)
                     (<= (file-write-date source)
                         (file-write-date lib)))
                #+ccl (ccl:run-program
                       "gcc"
                       `(
                         "-x"
                         "c"
                         ,(namestring source)
                         "-fPIC"
                         "--shared"
                         "-lfuse"
                         "-o" 
                         ,(namestring lib)
                         )
                       :search t
                       :output t
                       )
                #+sbcl (sb-ext:run-program
                        "gcc"
                        `(
                          "-x"
                          "c"
                          ,(namestring source)
                         "-fPIC"
                          "--shared"
                          "-lfuse"
                          "-o" 
                          ,(namestring lib)
                          )
                        :search t
                        :output t
                        )
                #+ecl (ext:system
                       (format 
                        nil
                        "~a~{ '~a'~}"
                        "gcc"
                        `(
                          "-x"
                          "c"
                          ,(namestring source)
                          "--shared"
                          "-lfuse"
                          "-o" 
                          ,(namestring lib)
                          )
                        :search t
                        :output t
                        ))
                )

        (defpackage :cl-fuse (:use :common-lisp :cffi))
        (unless (and (probe-file grovel-real-target) 
                     (<= (file-write-date grovel-source)
                         (file-write-date grovel-real-target)))
                (cffi-grovel:process-grovel-file 
                 grovel-source
                 grovel-target))
        ))

(asdf:defsystem :cl-fuse
  :name "cl-fuse"
  :depends-on (cffi cl-utilities
    bordeaux-threads trivial-backtrace
    iterate trivial-utf-8)
  :author "Michael Raskin <fb08af68@rambler.ru>"
  :maintainer "Michael Raskin <fb08af68@rambler.ru>"
  :license "LLGPL"
  :description "CFFI bindings to FUSE (Filesystem in user space)"
  :serial t
  :components (
    (:file "package")
    (:file "encoding-helper")
    (:file "fuse-grovel-result.grovel-tmp")
    (:file "fuse-functions")
    (:file "default-callbacks")
    (:file "fuse-wrapper-helpers")
    (:file "getattr-helpers")
    (:file "fuse-wrapper")
  ))
