#lang racket/base
;
; libdevmapper.so.1.02 bindings
;

(require
  (rename-in ffi/unsafe (-> -->)))

(require ffi/unsafe/define
         ffi/unsafe/alloc)

(require misc1/throw)

(provide
  (all-defined-out))


(struct exn:fail:dm exn:fail
  ())


(define-ffi-definer define-scheme #f)
(define-ffi-definer define-dm (ffi-lib "libdevmapper" '("1.02")))


(define-syntax-rule (define-ffi-wrapper ((name arg ...) proc) body ...)
  (define ((name arg ...) proc)
    (let ((wrapper (begin body ...))
          (name (object-name proc)))
      (if name (procedure-rename wrapper name) wrapper))))

(define-ffi-wrapper ((check-result) proc)
  (λ args
    (unless (apply proc args)
      (let ((name (or (object-name proc) 'dm)))
        (throw exn:fail:dm name "operation failed")))))

(define-ffi-wrapper ((producing-checked-result) proc)
  (λ args
    (let ((result (apply proc args)))
      (unless result
        (let ((name (or (object-name proc) 'dm)))
          (throw exn:fail:dm name "operation failed")))
      result)))


(define-cpointer-type _dm-task-pointer)


(define dm-task-types '(create reload remove remove-all suspend resume info
                        deps rename version status table waitevent list clear
                        mknodes list-versions target-msg set-geometry))

(define _dm-task-type (_enum dm-task-types))

(define (dm-task-type? v)
  (and (member v dm-task-types) #t))


(define-cstruct _struct-dm-info
  ((exists?         _bool)
   (suspended?      _bool)
   (live-table?     _bool)
   (inactive-table? _bool)
   (open-count      _int32)
   (event-number    _uint32)
   (major           _uint32)
   (minor           _uint32)
   (read-only?      _bool)
   (target-count    _int32)))


(define-cstruct _dm-names
  ((dev  _uint64)
   (next _uint32)))


(define-dm dm_task_destroy
           (_fun _dm-task-pointer --> _void)
           #:wrap (releaser))

(define-dm dm_task_create
           (_fun _dm-task-type --> _dm-task-pointer)
           #:wrap (allocator dm_task_destroy))

(define-dm dm_task_set_name
           (_fun _dm-task-pointer _string/utf-8 --> _bool)
           #:wrap (check-result))

(define-dm dm_task_set_uuid
           (_fun _dm-task-pointer _string/utf-8 --> _bool)
           #:wrap (check-result))

(define-dm dm_task_set_newname
           (_fun _dm-task-pointer _string/utf-8 --> _bool)
           #:wrap (check-result))

(define-dm dm_task_set_newuuid
           (_fun _dm-task-pointer _string/utf-8 --> _bool)
           #:wrap (check-result))

(define-dm dm_task_run
           (_fun _dm-task-pointer --> _bool)
           #:wrap (check-result))

(define-dm dm_task_get_info
           (_fun _dm-task-pointer
                 (info : (_ptr o _struct-dm-info))
                 --> (result : _bool)
                 --> (and result info))
           #:wrap (producing-checked-result))

(define-dm dm_task_update_nodes
           (_fun --> _void))

(define-dm dm_task_add_target
           (_fun _dm-task-pointer
                 _uint64
                 _uint64
                 _string/utf-8
                 _string/utf-8
                 --> _bool)
           #:wrap (check-result))

(define-dm dm_get_next_target
           (_fun _dm-task-pointer
                 _pointer
                 (start : (_ptr o _uint64))
                 (length : (_ptr o _uint64))
                 (ttype : (_ptr o _string/utf-8))
                 (params : (_ptr o _string/utf-8))
                 --> (next : _pointer)
                 --> (values start length ttype params next)))

(define (names->list names)
  (let* ((next (dm-names-next names))
         (size (- next 12))
         (bstr (make-sized-byte-string (ptr-add names 12) size)))
    (if (> next 0)
        (cons (bytes-copy bstr)
              (names->list (ptr-add names next)))
        (list (bytes-copy bstr)))))

(define-dm dm_task_get_names
           (_fun _dm-task-pointer
                 --> (names : _dm-names-pointer/null)
                 --> (and names (names->list names)))
           #:wrap (producing-checked-result))


; vim:set ts=2 sw=2 et:
