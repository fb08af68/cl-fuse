; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :cl-fuse)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro file-here (name &optional type)
            (make-pathname :name name
			   :type type
                           :directory (pathname-directory 
                                       (or
                                        *compile-file-pathname*
                                        *load-pathname*
                                        ))
                           :device (pathname-device
                                    (or
                                     *compile-file-pathname*
                                     *load-pathname*
                                     ))
                           :host (pathname-host
                                  (or
                                   *compile-file-pathname*
                                   *load-pathname*
                                   ))
                           ))
  )

(cffi:define-foreign-type cffi-string-latin-1 (cffi::foreign-string-type)
  () (:default-initargs :encoding :latin-1)
  (:simple-parser cffi-string-latin-1))

(define-foreign-library libfuse 
  (t (:default "libfuse")))
(define-foreign-library 
  fuse-launcher 
  (t (:or
       #.(namestring (file-here "libfuse-launcher" "so"))
       #+ccl #.(ccl:native-translated-namestring (file-here "libfuse-launcher" "so"))
       "libfuse-launcher"
       #.(file-here "libfuse-launcher")
       #.(file-here "libfuse-launcher" "so")
       #.(namestring (file-here "libfuse-launcher"))
       )))

(use-foreign-library libfuse)
(use-foreign-library fuse-launcher)

(defcfun "fuse_mount" :pointer
  (mountpoint :pointer)
  (args :pointer))

(defcfun "fuse_parse_cmdline" :int
  (args :pointer)
  (mountpoint :pointer)
  (multithreaded :pointer)
  (foreground :pointer))

(defcfun "fuse_opt_add_arg" :int
  (args :pointer)
  (arg :pointer)
  )

(defcfun "fuse_new" :pointer
  (ch :pointer)
  (args :int)
  (op :pointer)
  (op_size size)
  (user_data :pointer))

(defcfun "fuse_new_proxy" :pointer
  (ch :pointer)
  (args :pointer)
  (op :pointer)
  (op_size size)
  (user_data :pointer))

(defcfun "fuse_destroy" :void
  (f fuse-data))

(defcfun "fuse_unmount" :void
  (mountpoint :pointer)
  (ch :pointer))

(defcfun "fuse_get_session" :pointer
  (f :pointer))

(defcfun "fuse_chan_bufsize" size
  (ch :pointer))

(defcfun "fuse_session_exited" :int
  (se :pointer))

(defcfun "fuse_session_exit" :void
  (se :pointer))

(defcfun "fuse_chan_recv" :int
  (ch :pointer)
  (buf :pointer)
  (size size))

(defcfun "fuse_session_process" :void
  (se :pointer)
  (buf :pointer)
  (len size)
  (ch :pointer))

(defcfun "fuse_lowlevel_new" :pointer
  (args :pointer)
  (op :pointer)
  (op-size size)
  (userdata :pointer)
  )

(defcfun "fuse_session_add_chan" :void
  (se :pointer)
  (ch :pointer))

(defcfun "fuse_setup" :pointer
  (argc :int)
  (argv :pointer)
  (op :pointer)
  (op-size size)
  (mountpoint :pointer)
  (multithreaded :pointer)
  (user-data :pointer))

(defcfun "fuse_session_next_chan" :pointer
  (se :pointer)
  (ch :pointer))

(defcfun "fuse_opt_free_args" :void
  (args :pointer))

(defcfun "fuse_get_context" :pointer)

(defun fuse-main-lisp (operations args &optional 
  (call-manager (lambda (f &rest x) 
                        (declare (ignore x)) 
                        (funcall f))))
  (let* (
         (operation-list (foreign-alloc 'fuse-ops))
         (args-fuse (foreign-alloc 'fuse-args))
         (chan-fuse nil)
         (data-fuse nil)
         (session nil)
         (bufsize nil)
         (buf-fuse nil)
         (foreground (foreign-alloc :int))
         (multithreaded (foreign-alloc :int))
         (mountpoint (foreign-alloc :pointer))
         (fuse-ops-size (foreign-type-size 'fuse-ops))
         (argc (length args))
         (argv (foreign-alloc :pointer :count (+ 1 argc)))
         (failure nil)
         (dummy nil)
         )
        (loop for i from 0 upto (- fuse-ops-size 1) do 
              (setf (mem-aref operation-list :uint8 i) 0))
        (loop for i in operations do 
              (setf (foreign-slot-value operation-list 'fuse-ops (car i))
                    (get-callback (cadr i))))
        (loop for i from 0 upto (- argc 1) do
              (setf (mem-aref argv :pointer i) 
                    (foreign-string-alloc (nth i args))))
        (setf (mem-aref argv :pointer argc) (null-pointer))
        (setf (foreign-slot-value args-fuse 'fuse-args 'argc) argc
              (foreign-slot-value args-fuse 'fuse-args 'argv) argv
              (foreign-slot-value args-fuse 'fuse-args 'allocated) 1
              )
        (setf dummy (fuse-parse-cmdline args-fuse 
                                        mountpoint 
                                        multithreaded 
                                        foreground))
        (setf chan-fuse (fuse-mount (mem-ref mountpoint :pointer) 
                                    args-fuse))
        (when (= (pointer-address chan-fuse) 0)
              (return-from fuse-main-lisp nil)
              )
        (setf data-fuse 
              (fuse-new-proxy chan-fuse 
                               args-fuse
                               operation-list 
                               (foreign-type-size 'fuse-ops)
                               (null-pointer)
                               ))
        (when (= (pointer-address data-fuse) 0)
              (fuse-unmount (mem-ref mountpoint :pointer) chan-fuse)
              (return-from fuse-main-lisp nil)
              )

        (fuse-opt-free-args args-fuse)
        
        (setf session (fuse-get-session data-fuse))
        (setf chan-fuse (fuse-session-next-chan session (null-pointer)))
        (setf bufsize (fuse-chan-bufsize chan-fuse))

        (loop until (or failure (/= (fuse-session-exited session) 0))
              do (let* (
                        (tmp-chan (foreign-alloc :pointer))
                        (dummy (setf (mem-ref tmp-chan :pointer)
                                     chan-fuse))
                        (buf-fuse (foreign-alloc :char :count bufsize))
                        (read-success 
                         (fuse-chan-recv tmp-chan buf-fuse bufsize))
                        )
                       (declare (ignore dummy))
                       (cond
                        ((= read-success (- error-EINTR)) nil)
                        ((= read-success 0) nil)
                        ((< read-success 0) (setf failure t))
                        (t (funcall 
                            call-manager
                            (lambda () 
                                    (fuse-session-process 
                                     session buf-fuse read-success 
                                     (mem-ref tmp-chan :pointer))
                                    (when buf-fuse (foreign-free buf-fuse))
                                    (when tmp-chan (foreign-free tmp-chan))
                                    )
                            ))
                        )
                       ))

        (fuse-unmount (mem-ref mountpoint :pointer) chan-fuse)
        (unless (= (pointer-address data-fuse) 0)
                (fuse-destroy data-fuse))
        (mapcar 'foreign-free 
                (list
                 operation-list args-fuse foreground
                 multithreaded mountpoint))
        ))
