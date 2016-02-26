; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(in-package :cl-fuse)
(define "FUSE_USE_VERSION" "27")
(define "_FILE_OFFSET_BITS" "64")
(define "_XOPEN_SOURCE" "600")
(include "fuse.h")
(include "fuse/fuse_lowlevel.h")
(include "sys/types.h")
(include "sys/stat.h")
(include "unistd.h")
(include "errno.h")
(c " struct fuse_session {}; struct fuse {}; ")
(ctype pid "pid_t")
(ctype gid "gid_t")
(ctype uid "uid_t")
(ctype dev "dev_t")
(ctype ino "ino_t")
(ctype mode "mode_t")
(ctype nlink "nlink_t")
(ctype size "size_t")
(ctype offset "off_t")
(ctype blksize "blksize_t")
(ctype blkcnt "blkcnt_t")
(ctype time-t "time_t")

;( Non-translated type
 ;/** Function to add an entry in a readdir() operation
 ; *
 ; * @param buf the buffer passed to the readdir() operation
 ; * @param name the file name of the directory entry
 ; * @param stat file attributes, can be NULL
 ; * @param off offset of the next entry or zero
 ; * @return 1 if buffer is full, zero otherwise
 ; */
 ;typedef int (*fuse_fill_dir_t) (void *buf, const char *name,
 ;				const struct stat *stbuf, off_t off);
 ;)

(cstruct fuse-ops "struct fuse_operations"
	; int (*getattr) (const char *, struct stat *);
	(:getattr "getattr" :type :pointer)
	; int (*readlink) (const char *, char *, size_t);
	(:readlink "readlink" :type :pointer)
	; This is called for creation of all non-directory, non-symlink nodes. 
	; If the filesystem defines a create() method, then for regular files 
	; that will be called instead.
	; int (*mknod) (const char *, mode_t, dev_t);
	(:mknod "mknod" :type :pointer)
	; int (*mkdir) (const char *, mode_t);
	(:mkdir "mkdir" :type :pointer)
	; int (*unlink) (const char *);
	(:unlink "unlink" :type :pointer)
	; int (*rmdir) (const char *);
	(:rmdir "rmdir" :type :pointer)
	; int (*symlink) (const char *, const char *);
	(:symlink "symlink" :type :pointer)
	; int (*rename) (const char *, const char *);
	(:rename "rename" :type :pointer)
	; int (*link) (const char *, const char *);
	(:link "link" :type :pointer)
	; int (*chmod) (const char *, mode_t);
	(:chmod "chmod" :type :pointer)
	; int (*chown) (const char *, uid_t, gid_t);
	(:chown "chown" :type :pointer)
	; int (*truncate) (const char *, off_t);
	(:truncate "truncate" :type :pointer)
	; No creation, or truncation flags (O_CREAT, O_EXCL, O_TRUNC)
	; will be passed to open().  Open should check if the operation
	; is permitted for the given flags.  Optionally open may also
	; return an arbitrary filehandle in the fuse_file_info structure,
	; which will be passed to all file operations.
	; int (*open) (const char *, struct fuse_file_info *);
	(:open "open" :type :pointer)
	; Read should return exactly the number of bytes requested except
	; on EOF or error, otherwise the rest of the data will be
	; substituted with zeroes.	 
	; int (*read) (const char *, char *, size_t, off_t,
	;	     struct fuse_file_info *);
	(:read "read" :type :pointer)
	; int (*write) (const char *, const char *, size_t, off_t,
	;	      struct fuse_file_info *);
	(:write "write" :type :pointer)
	; The 'f_frsize', 'f_favail', 'f_fsid' and 'f_flag' fields are ignored
	; int (*statfs) (const char *, struct statvfs *);
	(:statfs "statfs" :type :pointer)
	; NOTE: The flush() method may be called more than once for each
	; open().	This happens if more than one file descriptor refers
	; to an opened file due to dup(), dup2() or fork() calls.	It is
	; not possible to determine if a flush is final, so each flush
	; should be treated equally.  Multiple write-flush sequences are
	; relatively rare, so this shouldn't be a problem.
	;
	; Filesystems shouldn't assume that flush will always be called
	; after some writes, or that if will be called at all.
	; int (*flush) (const char *, struct fuse_file_info *);
	(:flush "flush" :type :pointer)
	; int (*release) (const char *, struct fuse_file_info *);
	(:release "release" :type :pointer)
	; int (*fsync) (const char *, int, struct fuse_file_info *);
	(:fsync "fsync" :type :pointer)
	; int (*setxattr) (const char *, const char *, const char *, size_t, int);
	(:setxattr "setxattr" :type :pointer)
	; int (*getxattr) (const char *, const char *, char *, size_t);
	(:getxattr "getxattr" :type :pointer)
	; int (*listxattr) (const char *, char *, size_t);
	(:listxattr "listxattr" :type :pointer)
	; int (*removexattr) (const char *, const char *);
	(:removexattr "removexattr" :type :pointer)
	; This method should check if the open operation is permitted for
	; this  directory
	; int (*opendir) (const char *, struct fuse_file_info *);
	(:opendir "opendir" :type :pointer)
	; The filesystem may choose between two modes of operation:
	;
	; 1. The readdir implementation ignores the offset parameter, and
	; passes zero to the filler function's offset.  The filler
	; function will not return '1' (unless an error happens), so the
	; whole directory is read in a single readdir operation.  This
	; works just like the old getdir() method.
	;
	; 2. The readdir implementation keeps track of the offsets of the
	; directory entries.  It uses the offset parameter and always
	; passes non-zero offset to the filler function.  When the buffer
	; is full (or an error happens) the filler function will return
	; '1'.
	; int (*readdir) (const char *, void *, fuse_fill_dir_t, off_t,
	;		struct fuse_file_info *);
	(:readdir "readdir" :type :pointer)
	; int (*releasedir) (const char *, struct fuse_file_info *);
	(:releasedir "releasedir" :type :pointer)
	; int (*fsyncdir) (const char *, int, struct fuse_file_info *);
	(:fsyncdir "fsyncdir" :type :pointer)
	; void *(*init) (struct fuse_conn_info *conn);
	(:init "init" :type :pointer)
	; void (*destroy) (void *);
	(:destroy "destroy" :type :pointer)
	; This will be called for the access() system call.  If the
	; 'default_permissions' mount option is given, this method is not
	; called.
	; int (*access) (const char *, int);
	(:access "access" :type :pointer)
	; int (*create) (const char *, mode_t, struct fuse_file_info *);
	(:create "create" :type :pointer)
	; This method is called instead of the truncate() method if the
	; truncation was invoked from an ftruncate() system call.
	; int (*ftruncate) (const char *, off_t, struct fuse_file_info *);
	(:ftruncate "ftruncate" :type :pointer)
	; int (*fgetattr) (const char *, struct stat *, struct fuse_file_info *);
	(:fgetattr "fgetattr" :type :pointer)
	; int (*lock) (const char *, struct fuse_file_info *, int cmd,
	;	     struct flock *);
	(:lock "lock" :type :pointer)
	; int (*utimens) (const char *, const struct timespec tv[2]);
	(:utimens "utimens" :type :pointer)
	; int (*bmap) (const char *, size_t blocksize, uint64_t *idx);
	(:bmap "bmap" :type :pointer)
  )
(cstruct fuse-ctx "struct fuse_context"
  (:fuse "fuse" :type :pointer)
  (:uid "uid" :type uid)
  (:gid "gid" :type gid)
  (:pid "pid" :type pid)
  (:private-data "private_data" :type :pointer)
  )
(cstruct fuse-file-info "struct fuse_file_info"
  (:open-flags "flags" :type :int)
  (:by-writepage "writepage" :type :int)
  ;(operation-mode "direct_io" :type uint32) ; direct_io, keep_cache, flush
  (:file-handle "fh" :type :uint64)
  (:lock-owner "lock_owner" :type :uint64)
  )

(cstruct stat-data "struct stat"
  (:on-dev            "st_dev"     :type dev     ) ; ID of device containing file 
  (:inode             "st_ino"     :type ino     ) ; inode number 
  (:mode              "st_mode"    :type mode    ) ; protection 
  (:link-count        "st_nlink"   :type nlink   ) ; number of hard links 
  (:uid               "st_uid"     :type uid     ) ; user ID of owner 
  (:gid               "st_gid"     :type gid     ) ; group ID of owner 
  (:device-id         "st_rdev"    :type dev     ) ; device ID (if special file) 
  (:size              "st_size"    :type offset  ) ; total size, in bytes 
  (:blocksize         "st_blksize" :type blksize ) ; blocksize for file system I/O 
  (:blockcount        "st_blocks"  :type blkcnt  ) ; number of 512B blocks allocated 
  (:access-time       "st_atime"   :type time-t  ) ; time of last access 
  (:modification-time "st_mtime"   :type time-t  ) ; time of last modification 
  (:change-time       "st_ctime"   :type time-t  ) ; time of last status change 
  )

(cstruct fuse-args "struct fuse_args" 
  (argc "argc" :type :int)
  (argv "argv" :type :pointer)
  (allocated "allocated" :type :int)
  )

(cstruct fuse-session "struct fuse_session"
  )

(cstruct fuse-session-ops "struct fuse_session_ops"
  )

(cstruct fuse-data "struct fuse"
  )

(constant (mode-directory   "S_IFDIR" ))
(constant (mode-char-dev    "S_IFCHR" ))
(constant (mode-block-dev   "S_IFBLK" ))
(constant (mode-regular     "S_IFREG" ))
(constant (mode-fifo        "S_IFIFO" ))
(constant (mode-link        "S_IFLNK" ))
(constant (mode-socket      "S_IFSOCK") :optional t)

(constant (error-EPERM   "EPERM"))    ; Operation not permitted 
(constant (error-ENOENT  "ENOENT"))   ; No such file or directory 
(constant (error-ESRCH   "ESRCH"))    ; No such process 
(constant (error-EINTR   "EINTR"))    ; Interrupted system call 
(constant (error-EIO     "EIO"))      ; I/O error 
(constant (error-ENXIO   "ENXIO"))    ; No such device or address 
(constant (error-E2BIG   "E2BIG"))    ; Argument list too long 
(constant (error-ENOEXEC "ENOEXEC"))  ; Exec format error 
(constant (error-EBADF   "EBADF"))    ; Bad file number 
(constant (error-ECHILD  "ECHILD"))   ; No child processes 
(constant (error-EAGAIN  "EAGAIN"))   ; Try again 
(constant (error-ENOMEM  "ENOMEM"))   ; Out of memory 
(constant (error-EACCES  "EACCES"))   ; Permission denied 
(constant (error-EFAULT  "EFAULT"))   ; Bad address 
(constant (error-ENOTBLK "ENOTBLK"))  ; Block device required 
(constant (error-EBUSY   "EBUSY"))    ; Device or resource busy 
(constant (error-EEXIST  "EEXIST"))   ; File exists 
(constant (error-EXDEV   "EXDEV"))    ; Cross-device link 
(constant (error-ENODEV  "ENODEV"))   ; No such device 
(constant (error-ENOTDIR "ENOTDIR"))  ; Not a directory 
(constant (error-EISDIR  "EISDIR"))   ; Is a directory 
(constant (error-EINVAL  "EINVAL"))   ; Invalid argument 
(constant (error-ENFILE  "ENFILE"))   ; File table overflow 
(constant (error-EMFILE  "EMFILE"))   ; Too many open files 
(constant (error-ENOTTY  "ENOTTY"))   ; Not a typewriter 
(constant (error-ETXTBSY "ETXTBSY"))  ; Text file busy 
(constant (error-EFBIG   "EFBIG"))    ; File too large 
(constant (error-ENOSPC  "ENOSPC"))   ; No space left on device 
(constant (error-ESPIPE  "ESPIPE"))   ; Illegal seek 
(constant (error-EROFS   "EROFS"))    ; Read-only file system 
(constant (error-EMLINK  "EMLINK"))   ; Too many links 
(constant (error-EPIPE   "EPIPE"))    ; Broken pipe 
(constant (error-EDOM    "EDOM"))     ; Math argument out of domain of func 
(constant (error-ERANGE  "ERANGE"))   ; Math result not representable 

(constant (open-ACCMODE  "O_ACCMODE"))
(constant (open-RDONLY   "O_RDONLY"))
(constant (open-WRONLY   "O_WRONLY"))
(constant (open-RDWR     "O_RDWR"))
(constant (open-CREAT    "O_CREAT"))
(constant (open-EXCL     "O_EXCL"))
(constant (open-NOCTTY   "O_NOCTTY"))
(constant (open-TRUNC    "O_TRUNC"))
(constant (open-APPEND   "O_APPEND"))
(constant (open-NONBLOCK "O_NONBLOCK"))
(constant (open-NDELAY   "O_NDELAY"))
(constant (open-SYNC     "O_SYNC"))
(constant (open-FSYNC    "O_FSYNC"))
(constant (open-ASYNC    "O_ASYNC"))
