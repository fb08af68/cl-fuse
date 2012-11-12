; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :cl-fuse)

(defvar *break-on-errors* nil)
(defvar *trace-functions* nil)
(defvar *last-wrapper-error* "No error")

(defun fuse-complain (&rest reason)
  (format *error-output* "CL-FUSE complains: ~a~%" (apply 'format nil reason))
  (finish-output *error-output*))

(defmacro just-print-errors (&rest body)
  `(block nil
          (handler-case (progn ,@body) 
                 (error (x) 
                    (progn (fuse-complain "Error occured: ~s~%" x)
                           (setf *last-wrapper-error*
                                 (format nil
                                  "Error:~%~a~%~s~%Backtrace:~%~a~%"
                                  x x
                                  (with-output-to-string 
                                   (s)
                                   (trivial-backtrace:print-backtrace-to-stream s)
                                   )
                                  ))
                           (return (- error-EIO))
                           )))))

(defmacro fuse-callback (name type args &rest body)
  (let* (
         (the-package (find-package :cl-fuse))
         (function-name (intern (string-upcase 
                                 (concatenate 'string "fuse-wrapper-" 
                                              (symbol-name name) "-function"))
                                the-package))
         (callback-name (intern (string-upcase 
                                 (concatenate 'string "fuse-wrapper-" 
                                              (symbol-name name)))
                                the-package))
         (arglist (mapcar 'car args))
         )
        `(progn
          (defun ,function-name ,arglist
                 (when 
                  *trace-functions*
                  (format *error-output* 
                          "Entering ~s with args ~s~%" 
                          ',function-name (list ,@arglist))
                  )
                 (prog1
                  (if *break-on-errors*
                      (progn ,@body)
                      (just-print-errors ,@body)
                      )
                  (when 
                   *trace-functions*
                   (format *error-output* 
                           "Leaving ~s with args ~s~%" 
                           ',function-name (list ,@arglist))
                   )
                  )
                 )
          (defcallback ,callback-name ,type ,args 
                       (,function-name ,@arglist)))))

(defun find-name (name body &optional (fallback t))
  (let (res)
       (or
        (loop for x in body 
              when (and (symbolp x) (equal (symbol-name x) (string name))) return x
              when (and (listp x) (setf res (find-name name x nil))) return res
              )
        (and fallback (intern (string name)))
        )))

(defmacro fuse-path-callback (name type args &rest body)
  (let* (
         (path (find-name "PATH" body))
         (split-path (find-name "SPLIT-PATH" body))
         )
  `(fuse-callback 
    ,name ,type ((,path :string) ,@args)               
    (if (stringp ,path)
        (let*
         (
          (,split-path (cdr (cl-utilities:split-sequence #\/ ,path)))
          )
         ,@body
         )
        (- error-ENOENT))
    )))

(defun fuse-funcall (f &rest args)
  (when f (apply f args)))

(defmacro fuse-and (&rest body)
  `(block fuse-and
          ,@(loop for x in body collect
                  `(let ((res ,x))
                        (if
                         (or
                          (not res)
                          (and
                           (integerp res)
                           (< res 0))
                          (and
                           (not (eq res t))
                           (not (integerp res))
                           )
                          )
                         (progn
                          (fuse-complain "fuse-and: failure~%")
                          (fuse-complain "in ~s~%" ',x)
                          (fuse-complain "got ~s~%" res)
                          (return-from fuse-and nil)
                          )
                         res)))
          ))
