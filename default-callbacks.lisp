; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :cl-fuse)

(defun fuse-wrapper-default-directoryp (path)
  (equalp (car path) ""))

(defun fuse-wrapper-default-symlink-target (path)
  nil)

(defun fuse-wrapper-default-directory-content (path)
  nil)

(defun fuse-wrapper-default-file-open (path flags)
  0)

(defun fuse-wrapper-default-file-release (path flags)
  0)

(defun fuse-wrapper-default-file-read (path size offset fh)
  nil)

(defun fuse-wrapper-default-file-size (path)
  nil)

(defun fuse-wrapper-default-file-writeable-p (path)
  nil)

(defun fuse-wrapper-default-file-create (path mode dev)
  nil)

(defun fuse-wrapper-default-chmod (path mode)
  0)

(defun fuse-wrapper-default-chown (path mode)
  0)

(defun fuse-wrapper-default-truncate (path offset)
  0)

(defun fuse-wrapper-default-file-flush (path fh)
  0)

(defun fuse-wrapper-default-mkdir (path mode)
  (- error-EACCES))

(defun fuse-wrapper-default-unlink (path)
  (- error-EACCES))

(defun fuse-wrapper-default-rmdir (path)
  (- error-EACCES))

(defun fuse-wrapper-default-symlink (path content)
  (- error-EACCES))
