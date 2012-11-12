; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :cl-fuse)

;; Feel free to override these two functions
;; You must support :direct and :full-range
;; :direct (usually Latin-1) should be able to parse 
;; any given octet sequence; :full-range should be able
;; to serialize any given Unicode string
(defun string-to-octets (str encoding)
  (cond
   ((eq encoding :direct) (string-to-octets str :latin-1))
   ((eq encoding :full-range) (string-to-octets str :utf-8))
   ((eq encoding :latin-1)
    (map '(vector (unsigned-byte 8)) 'char-code str))
   ((eq encoding :utf-8)
    (trivial-utf-8:string-to-utf-8-bytes str))
   #+sbcl (t (sb-ext:string-to-octets str :external-format encoding))
   #+ccl (t (ccl:encode-string-to-octets str :external-format encoding))
   (t (error "Encoding is unsupported: ~s" encoding))
   ))

(defun octets-to-string (octs encoding)
  (cond
   ((eq encoding :direct) (octets-to-string octs :latin-1))
   ((eq encoding :full-range) (octets-to-string octs :utf-8))
   ((eq encoding :latin-1)
    (map 'string 'code-char octs))
   ((eq encoding :utf-8)
    (trivial-utf-8:utf-8-bytes-to-string octs))
   #+sbcl (t (sb-ext:octets-to-string octs :external-format encoding))
   #+ccl (t (ccl:decode-string-from-octets octs :external-format encoding))
   (t (error "Encoding is unsupported: ~s" encoding))
   ))

(defun recast-string (str from to)
  (when str (octets-to-string (string-to-octets str from) to)))
