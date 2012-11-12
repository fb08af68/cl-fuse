; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

; example FS 
; Does nothing of interest 
; mirrors /
; Fully low-level
; SBCL-only for now

(require :cl-fuse)
(require :sb-posix)
(require :cffi)

(use-package :cl-fuse)
(use-package :cffi)

(fuse-callback 
  mirror-getattr :int 
  ((path :string) (content stat-data))
  (let*
    (
     (data (sb-posix:stat path))
     )
    (setf 
      (foreign-slot-value content 'stat-data :on-dev) 0
      ;(foreign-slot-value content 'stat-data :inode) 0
      (foreign-slot-value content 'stat-data :mode) (sb-posix:stat-mode data)
      (foreign-slot-value content 'stat-data :link-count) (sb-posix:stat-nlink data)
      (foreign-slot-value content 'stat-data :uid) (sb-posix:stat-uid data)
      (foreign-slot-value content 'stat-data :gid) (sb-posix:stat-gid data)
      (foreign-slot-value content 'stat-data :device-id) 0
      (foreign-slot-value content 'stat-data :size) (sb-posix:stat-size data)
      (foreign-slot-value content 'stat-data :blocksize) 0
      (foreign-slot-value content 'stat-data :blockcount) 0
      (foreign-slot-value content 'stat-data :access-time) (sb-posix:stat-atime data)
      (foreign-slot-value content 'stat-data :modification-time) (sb-posix:stat-mtime data)
      (foreign-slot-value content 'stat-data :change-time) (sb-posix:stat-ctime data)
      )
    0
    ))

(fuse-callback 
  mirror-readdir :int 
  (
   (path :string) (buf :pointer) (filler :pointer)
   (offset offset) (file-info fuse-file-info)
   )
  (foreign-funcall-pointer
    filler ()
    :pointer buf :string "." :pointer (null-pointer) offset 0)
  (foreign-funcall-pointer
    filler ()
    :pointer buf :string ".." :pointer (null-pointer) offset 0)
  (loop 
    for x in (directory (concatenate 'string path "/*.*"))
    do
    (foreign-funcall-pointer
      filler () :pointer buf
      :string (car (last (pathname-directory (concatenate 'string (namestring x) "/"))))
      :pointer (null-pointer) offset 0)
    )
  0)

(fuse-callback
  mirror-open :int
  ((path :string) (file-info fuse-file-info))
  (setf
    (foreign-slot-value file-info 'fuse-file-info :file-handle)
    (sb-posix:open path (foreign-slot-value file-info 'fuse-file-info :open-flags)))
  0)

(fuse-callback
  mirror-read :int
  ((path :string) (buf :pointer) (size size) (offset offset) (file-info fuse-file-info))
  (let*
    ((fd (foreign-slot-value file-info 'fuse-file-info :file-handle)))
    (sb-posix:lseek fd offset 0)
    (sb-posix:read fd buf size)))

(defun fuse-mirror-test ()
  (ensure-directories-exist  "/tmp/cl-fuse-mirror-test/")
  (setf cl-fuse::*break-on-errors* t)
  (setf cl-fuse::*trace-functions* t)
  (fuse-main-lisp
    '(
      (:getattr cl-fuse::fuse-wrapper-mirror-getattr)
      (:readdir cl-fuse::fuse-wrapper-mirror-readdir)
      (:open cl-fuse::fuse-wrapper-mirror-open)
      (:read cl-fuse::fuse-wrapper-mirror-read)
      )
    '(
      "-d" "/tmp/cl-fuse-mirror-test/"
      )
    (lambda (f) (funcall f))
    ))
