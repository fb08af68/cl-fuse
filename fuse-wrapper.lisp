; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

; This file provides wrappers for easy creation of FUSE file systems with
; cl-fuse. SBCL tests show that once you use fuse_main_real, you have to 
; quit right after it terminates. As for most situations your program only
; serves one FS during its lifetime, this is written as to make its 
; one-shot-only use obvious

; I tried to write a honest macro using gensyms and passing functions and 
; whatnot. It failed if it was precompiled. Be warned, and patches welcome.

; Probably with "call-callback-yourself" hack you can make this work, but 
; why bother

(in-package :cl-fuse)

(defvar *fuse-wrapper-directoryp*)
(defvar *fuse-wrapper-directory-content*)
(defvar *fuse-wrapper-symlink-target*)
(defvar *fuse-wrapper-symlinkp*)
(defvar *fuse-wrapper-file-open*)
(defvar *fuse-wrapper-file-release*)
(defvar *fuse-wrapper-file-read*)
(defvar *fuse-wrapper-file-size*)
(defvar *fuse-wrapper-file-write*)
(defvar *fuse-wrapper-file-write-whole*)
(defvar *fuse-wrapper-file-writeable-p*)
(defvar *fuse-wrapper-file-executable-p*)
(defvar *fuse-wrapper-file-create*)
(defvar *fuse-wrapper-chmod*)
(defvar *fuse-wrapper-chown*)
(defvar *fuse-wrapper-truncate*)
(defvar *fuse-wrapper-file-flush*)
(defvar *fuse-wrapper-mkdir*)
(defvar *fuse-wrapper-unlink*)
(defvar *fuse-wrapper-rmdir*)
(defvar *fuse-wrapper-rename*)
(defvar *fuse-wrapper-symlink*)

(defvar *fuse-wrapper-rename-emulation* t)
(defvar *fuse-wrapper-max-write-whole-size* 1048576)

(fuse-path-callback getattr :int ((content stat-data))
  (let (
	(file-size 0)
	)
    ;(fuse-complain "Entering getattr on ~s" split-path)
    (prog1
      (cond
	((fuse-funcall *fuse-wrapper-directoryp* split-path) 
	 (progn (generic-directory content) 0))
	((fuse-funcall *fuse-wrapper-symlinkp* split-path) 
	 (progn (generic-symlink content) 0))
	((progn 
	   (setf file-size (fuse-funcall *fuse-wrapper-file-size* split-path))
	   file-size)
	 (progn (generic-plain-file content file-size) 
		(when (fuse-funcall *fuse-wrapper-file-writeable-p* split-path)
		  (writeable-file content))
		(when (fuse-funcall *fuse-wrapper-file-executable-p* split-path)
		  (executable-file content))
		0))
	(t (- error-ENOENT))
	)
    ;(fuse-complain "Leaving getattr on ~s" split-path)
      )
    ))

(fuse-path-callback readlink :int 
  ((buf :pointer) (bufsize size))
  (let (
        (res (fuse-funcall *fuse-wrapper-symlink-target*
                      (cdr (cl-utilities:split-sequence #\/ path))))
        )
       (cond 
        ((null res) (- error-ENOENT))
        ((listp res) (progn 
            (lisp-string-to-foreign 
             (namestring (make-pathname 
                          :directory (cons :absolute 
                                           (butlast res)) 
                          :name (car (last res))))
             buf bufsize)
            0))
        ((stringp res) (progn 
                        (lisp-string-to-foreign res buf bufsize)
                        0))
        (t (- error-ENOENT))
        )))

(fuse-path-callback readdir :int ((buf :pointer) 
  (filler :pointer) (offset offset) 
  (file-info fuse-file-info))
  (if (fuse-funcall *fuse-wrapper-directoryp* split-path)
      (progn 
       (foreign-funcall-pointer filler () 
                                :pointer buf 
                                :string "."  
                                :pointer (null-pointer) 
                                offset 0)
       (foreign-funcall-pointer filler () 
                                :pointer buf 
                                :string ".."  
                                :pointer (null-pointer) 
                                offset 0)
       (let ((children (fuse-funcall *fuse-wrapper-directory-content* split-path)))
	    ;(fuse-complain "Obtained children of ~s:~%~s~%" split-path children)
            (loop for i in children do 
		  (if 
		    (or
		      (stringp i)
		      (arrayp i)
		      )
		    (progn
		      ;(fuse-complain "Not giving extra info on ~s" i)
		      (foreign-funcall-pointer filler () 
					       :pointer buf 
					       :string i
					       :pointer (null-pointer)
					       offset 0)
		      )
		    (let*
		      (
		       (stat-data (foreign-alloc 'stat-data))
		       (attrs (second i))
		       )
		      (generic-file stat-data)
		      (cond
			((eq attrs :symlink) (generic-symlink stat-data))
			((eq attrs :directory) (generic-directory stat-data))
			((eq attrs :empty-writeable) (writeable-file stat-data))
			((integerp attrs) (generic-plain-file stat-data attrs))
			((listp attrs)
			 (loop 
			   for x in attrs do
			   (cond
			     ((eq x :writeable) (writeable-file stat-data))
			     ((eq x :executable) (executable-file stat-data))
			     ((integerp x) (generic-plain-file stat-data x))
			     )
			   ))
			)
		      ;(fuse-complain "Giving attrs on ~s : ~s" split-path i)
		      (foreign-funcall-pointer filler () 
					       :pointer buf 
					       :string (first i)
					       :pointer stat-data
					       offset 0))
		    )))
       0)
      (- error-ENOTDIR)))

(fuse-path-callback file-open :int ((file-info fuse-file-info))
  (let* (
         (fh (fuse-funcall *fuse-wrapper-file-open* split-path 
                      (foreign-slot-value file-info 'fuse-file-info :open-flags)))
         )
        (if fh 
            (if (>= fh 0) (progn 
                           (setf (foreign-slot-value file-info 
                                                     'fuse-file-info :file-handle) 
                                 fh)
                           0)
                fh)
            (- error-ENOENT)
            )))

(defun process-buffer (data f size offset)
  (cond 
   ((stringp data)
    (process-buffer (cl-fuse::string-to-octets data :full-range)
                    f size offset))
   ((arrayp data)
    (loop for i from 0 upto (min (- (length data) 1) size)
          do (funcall f i (aref data i)))
    (min (length data) size))
   ((listp data)
    (if (eq (car data) :offset)
        (process-buffer (subseq (caddr data) 
                                (- offset (cadr data)))
                        f size offset)
        (progn
         (loop for i from 0 upto (min (- (length data) 1) size)
               do (funcall f i (nth i data)))
         (min (length data) size))))
   (t nil)))
 
(fuse-path-callback file-read :int ((buf :pointer) (size size)
  (offset offset) (file-info fuse-file-info))
  (let* (
         (data (fuse-funcall *fuse-wrapper-file-read* split-path size offset 
                        (foreign-slot-value file-info
                                            'fuse-file-info :file-handle))))
        (if (not data) (- error-EIO)
            (or 
             (process-buffer data 
                             (lambda (i x) 
                                     (setf (mem-aref buf :uint8 i) x)) 
                             size offset)
             (- error-EIO)))))

(fuse-path-callback file-write :int ((buf :pointer) (size size)
  (offset offset) (file-info fuse-file-info))
  (let* (
         (file-handle (foreign-slot-value file-info 
                                          'fuse-file-info 
                                          :file-handle))
         (old-data (when (and *fuse-wrapper-file-write-whole* 
                          (not *fuse-wrapper-file-write*)
                          *fuse-wrapper-file-read*)
                     (fuse-funcall *fuse-wrapper-file-read* split-path 
                                   *fuse-wrapper-max-write-whole-size* 0
                              file-handle)))
         (new-data (when (or *fuse-wrapper-file-write*
                             *fuse-wrapper-file-write-whole*)
                    (make-array (if *fuse-wrapper-file-write*
                                    size 
                                    (max
                                     (+ offset size)
                                     (if 
                                      (and (listp old-data)
                                           (eq (car old-data)
                                               :offset))
                                      (+ (cadr old-data)
                                         (length (caddr old-data)))
                                      (length old-data)
                                      )))
                                :element-type 
                                '(unsigned-byte 8))))
         (return-val (cond
         (*fuse-wrapper-file-write*
          (loop for i from 0 to (- size 1) do
                (setf (aref new-data i) (mem-aref buf :uint8 i)))
          (fuse-funcall *fuse-wrapper-file-write* 
                   split-path new-data offset file-handle))
         (*fuse-wrapper-file-write-whole*
          (process-buffer old-data 
                          (lambda (i x)
                                  (setf (aref new-data i) x)
                                  )
                          *fuse-wrapper-max-write-whole-size* 0)
          (loop for i from 0 to (- size 1) do
                (setf (aref new-data (+ offset i)) 
                      (mem-aref buf :uint8 i)))
          (let ((res 
                 (fuse-funcall *fuse-wrapper-file-write-whole*
                          split-path new-data file-handle)))
               (if (find res `(t ,(length new-data)))
                   size
                   (- error-EIO))))
         (t (- error-EROFS))))
         )
        (cond
         ((eq return-val t) size)
         ((null return-val) (- error-EIO))
         ((integerp return-val) return-val)
         (t (- error-EIO))
         )
        ))

(fuse-path-callback file-create :int ((mode mode) (dev dev))
  (let* (
         (result (and *fuse-wrapper-file-create*
                      (fuse-funcall *fuse-wrapper-file-create* split-path mode dev)))
         )
        (cond 
         ((eq result nil) (- error-EACCES))
         ((eq result t) 0)
         ((integerp result) result)
         (t (- error-EIO))
         )))

(fuse-path-callback chmod :int ((mode mode))
  (let* (
         (res (fuse-funcall *fuse-wrapper-chmod* split-path mode))
         )
        (cond 
         ((eq res nil) (- error-EACCES))
         ((eq res t) 0)
         ((integerp res) res)
         (t (- error-EIO))
         )))

(fuse-path-callback chown :int ((uid uid) (gid gid))
  (let* (
         (res (fuse-funcall *fuse-wrapper-chown* split-path uid gid))
         )
        (cond 
         ((eq res nil) (- error-EACCES))
         ((eq res t) 0)
         ((integerp res) res)
         (t (- error-EIO))
         )))

(fuse-path-callback truncate :int ((offset offset))
  (let* (
         (res (fuse-funcall *fuse-wrapper-truncate* split-path offset))
         )
        (cond 
         ((eq res nil) (- error-EIO))
         ((eq res t) 0)
         ((integerp res) res)
         (t (- error-EIO))
         )))

(fuse-path-callback file-flush :int ((file-info fuse-file-info))
  (let* (
         (fh (foreign-slot-value file-info 'fuse-file-info :file-handle))
         (res (fuse-funcall *fuse-wrapper-file-flush* split-path fh))
         )
        (cond 
         ((eq res nil) (- error-EIO))
         ((eq res t) 0)
         ((integerp res) res)
         (t (- error-EIO))
         )))

(fuse-path-callback file-release :int ((file-info fuse-file-info))
  (let* (
         (fh (foreign-slot-value file-info 'fuse-file-info :file-handle))
         (res (fuse-funcall *fuse-wrapper-file-release* split-path fh))
         )
        (cond 
         ((eq res nil) (- error-EIO))
         ((eq res t) 0)
         ((integerp res) res)
         (t (- error-EIO))
         )))

(fuse-path-callback mkdir :int ((mode mode))
  (let* (
         (res (fuse-funcall *fuse-wrapper-mkdir* split-path mode)))
        (cond 
         ((eq res nil) (- error-EACCES))
         ((eq res t) 0)
         ((integerp res) res)
         (t (- error-EIO))
         )))

(fuse-path-callback unlink :int ()
  (let* (
         (res (fuse-funcall *fuse-wrapper-unlink* split-path)))
        (cond 
         ((eq res nil) (- error-EACCES))
         ((eq res t) 0)
         ((integerp res) res)
         (t (- error-EIO))
         )))

(fuse-path-callback rmdir :int ()
  (let* (
         (res (fuse-funcall *fuse-wrapper-rmdir* split-path)))
        (cond 
         ((eq res nil) (- error-EACCES))
         ((eq res t) 0)
         ((integerp res) res)
         (t (- error-EIO))
         )))

(fuse-path-callback rename :int ((target :string))
  (cond
   ((functionp *fuse-wrapper-rename*)
    (let*
     (
      (res (funcall *fuse-wrapper-rename* split-path target))
      )
     (cond
      ((eq res nil) (- error-EACCES))
      ((eq res t) 0)
      ((integerp res) res)
      (t (- error-EIO))
      )))
   ((fuse-and
     (eq t *fuse-wrapper-rename-emulation*)
     (functionp *fuse-wrapper-file-read*)
     (functionp *fuse-wrapper-file-create*)
     (or
      (functionp *fuse-wrapper-file-write-whole*)
      (functionp *fuse-wrapper-file-write*)
      )
     (functionp *fuse-wrapper-unlink*)
     (not (not (fuse-funcall *fuse-wrapper-file-writeable-p* split-path)))
     (not (fuse-funcall *fuse-wrapper-directoryp* split-path))
     (not (fuse-funcall *fuse-wrapper-directoryp* 
                        (cdr (cl-utilities:split-sequence #\/ target))))
     (not (fuse-funcall *fuse-wrapper-symlinkp* split-path))
     (not (fuse-funcall *fuse-wrapper-symlinkp* 
                        (cdr (cl-utilities:split-sequence #\/ target))))
     )
    (let* 
     ((split-target (cdr (cl-utilities:split-sequence #\/ target)))
      (source-handle nil)
      (target-handle nil)
      (res 
       (fuse-and
        (or 
         (not (fuse-funcall *fuse-wrapper-file-size* split-target))
         (fuse-funcall *fuse-wrapper-unlink* split-target))
        (fuse-funcall *fuse-wrapper-file-create* split-target #o0644 0)
        (setf source-handle (funcall *fuse-wrapper-file-open* 
                                     split-path (logior open-RDONLY)))
        (setf target-handle (funcall *fuse-wrapper-file-open* 
                                     split-target (logior open-WRONLY)))
        (if *fuse-wrapper-file-write*
            (iterate
             (for chunk := 8192)
             (for n upfrom 0)
             (for data := (fuse-funcall *fuse-wrapper-file-read* split-path 
                                        chunk (* n chunk) source-handle))
             (for write-buf := (make-array (length (third data))
                                :element-type '(unsigned-byte 8)))
             (process-buffer data
                             (lambda (i x) (setf (aref write-buf i) x))
                             chunk 0)
             (for write-result := (fuse-funcall *fuse-wrapper-file-write*
                                                split-target write-buf (* n chunk)
                                                target-handle))
             (while (and (> (length data) 0)
                         (eq (fuse-and write-result t) t)))
             (finally (return write-result))
             )
            (let*
             (
              (content (funcall *fuse-wrapper-file-read* split-path 
                                *fuse-wrapper-max-write-whole-size* 0
                                source-handle))
              (write-buf (make-array (length (third content))
                                     :element-type '(unsigned-byte 8)))
              )
             (process-buffer content 
                             (lambda (i x) (setf (aref write-buf i) x))
                             *fuse-wrapper-max-write-whole-size* 0)
             (funcall *fuse-wrapper-file-write-whole*
                      split-target
                      write-buf
                      target-handle))
            )
        (if *fuse-wrapper-file-flush* 
            (funcall *fuse-wrapper-file-flush* split-target target-handle)
            t)
        (progn
         (funcall *fuse-wrapper-file-release* 
                  split-path source-handle)
         (funcall *fuse-wrapper-file-release* 
                  split-target target-handle)
         t)
        (funcall *fuse-wrapper-unlink* split-path)
        )))
     (cond
      ((eq res nil) (- error-EACCES))
      ((eq res t) 0)
      ((integerp res) res)
      (t (- error-EIO))
      )
     ))
    (t (- error-EACCES))
    ))

(fuse-callback 
  symlink :int ((content :string) (path :string))
  (let*
    (
     (split-path (cdr (cl-utilities:split-sequence #\/ path)))
     (res (fuse-funcall *fuse-wrapper-symlink* split-path content))
     )
    (cond
      ((eq res nil) (- error-EACCES))
      ((eq res t) 0)
      ((integerp res) res)
      (t (- error-EIO))
      )
    )
  )

(defun fuse-run (args &key 
  (directoryp 'fuse-wrapper-default-directoryp)
  (directory-content 'fuse-wrapper-default-directory-content)
  (symlink-target 'fuse-wrapper-default-symlink-target)
  (symlinkp symlink-target)
  (file-open 'fuse-wrapper-default-file-open)
  (file-release 'fuse-wrapper-default-file-release)
  (file-read 'fuse-wrapper-default-file-read)
  (file-size 'fuse-wrapper-default-file-size)
  (file-write nil)
  (file-write-whole nil)
  (file-writeable-p 'fuse-wrapper-default-file-writeable-p)
  (file-executable-p 'fuse-wrapper-default-file-executable-p)
  (file-create 'fuse-wrapper-default-file-create)
  (chmod 'fuse-wrapper-default-chmod)
  (chown 'fuse-wrapper-default-chown)
  (truncate 'fuse-wrapper-default-truncate)
  (file-flush 'fuse-wrapper-default-file-flush)
  (mkdir 'fuse-wrapper-default-mkdir)
  (unlink 'fuse-wrapper-default-unlink)
  (rmdir 'fuse-wrapper-default-rmdir)
  (symlink 'fuse-wrapper-default-symlink)
  (rename nil)
  (define-fs-only nil)
  (call-manager (lambda (f &rest x) 
                        (declare (ignore x)) 
                        (funcall f)))
  )
  (setf
   *fuse-wrapper-directoryp* directoryp
   *fuse-wrapper-directory-content* directory-content
   *fuse-wrapper-symlink-target* symlink-target
   *fuse-wrapper-symlinkp* symlinkp
   *fuse-wrapper-file-open* file-open
   *fuse-wrapper-file-release* file-release
   *fuse-wrapper-file-read* file-read
   *fuse-wrapper-file-size* file-size
   *fuse-wrapper-file-write* file-write
   *fuse-wrapper-file-write-whole* file-write-whole
   *fuse-wrapper-file-writeable-p* file-writeable-p
   *fuse-wrapper-file-executable-p* file-executable-p
   *fuse-wrapper-file-create* file-create
   *fuse-wrapper-chmod* chmod
   *fuse-wrapper-chown* chown
   *fuse-wrapper-truncate* truncate
   *fuse-wrapper-file-flush* file-flush
   *fuse-wrapper-mkdir* mkdir
   *fuse-wrapper-unlink* unlink
   *fuse-wrapper-rmdir* rmdir
   *fuse-wrapper-rename* rename
   *fuse-wrapper-symlink* symlink
   )
  (if (not define-fs-only)
                     (fuse-main-lisp '(
                                       (:getattr  fuse-wrapper-getattr     ) 
                                       (:readdir  fuse-wrapper-readdir     ) 
                                       (:readlink fuse-wrapper-readlink    ) 
                                       (:open     fuse-wrapper-file-open   ) 
                                       (:release  fuse-wrapper-file-release) 
                                       (:read     fuse-wrapper-file-read   ) 
                                       (:write    fuse-wrapper-file-write  ) 
                                       (:mknod    fuse-wrapper-file-create ) 
                                       (:chmod    fuse-wrapper-chmod       ) 
                                       (:chown    fuse-wrapper-chown       ) 
                                       (:truncate fuse-wrapper-truncate    ) 
                                       (:flush    fuse-wrapper-file-flush  ) 
                                       (:mkdir    fuse-wrapper-mkdir       ) 
                                       (:unlink   fuse-wrapper-unlink      ) 
                                       (:rmdir    fuse-wrapper-rmdir       ) 
                                       (:rename   fuse-wrapper-rename      ) 
                                       (:symlink  fuse-wrapper-symlink     ) 
                                       )
                                     args 
                                     call-manager)))
