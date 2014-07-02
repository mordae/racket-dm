#lang racket/base
;
; libdevmapper.so.1.02 bindings
;

(require racket/contract
         (rename-in ffi/unsafe (-> -->))
         ffi/unsafe/define
         misc1/throw)

(provide (all-defined-out))


(define-struct/contract (exn:fail:dm exn:fail) ())


(define-ffi-definer define-scheme #f)
(define-ffi-definer define-dm (ffi-lib "libdevmapper" '("1.02")))


(define/contract (check-result func result)
                 (-> symbol? any/c void?)
  (unless result
    (throw exn:fail:dm
           'dm "operation failed"
           "function" func
           "result" result)))


(define (with-finalizer result finalizer)
  (when result
    (register-finalizer result finalizer))
  result)


(define _string/utf-8/free
  (make-ctype _bytes
              (lambda (str)
                (if str
                  (string->bytes/utf-8 str)
                  #f))
              (lambda (bstr)
                (if bstr
                  (let ((str (bytes->string/utf-8 bstr)))
                    (free bstr)
                    str)
                  #f))))


(define-cpointer-type _dm-task-pointer)


(define dm-task-types '(create reload remove remove-all suspend resume info
                        deps rename version status table waitevent list clear
                        mknodes list-versions target-msg set-geometry))

(define _dm-task-type (_enum dm-task-types))

(define (dm-task-type? v)
  (and (member v dm-task-types) #t))


(define-cstruct _struct-dm-info
  ((exists         _bool)
   (suspended      _bool)
   (live-table     _bool)
   (inactive-table _bool)
   (open-count     _int32)
   (event-nr       _uint32)
   (major          _uint32)
   (minor          _uint32)
   (read-only      _bool)
   (target-count   _int32)))


(define-cstruct _dm-names
  ((dev  _uint64)
   (next _uint32)))


(define-dm dm_task_destroy
           (_fun _dm-task-pointer --> _void))

(define-dm dm_task_create
           (_fun _dm-task-type
                 --> (result : _dm-task-pointer)
                 --> (with-finalizer result dm_task_destroy)))

(define-dm dm_task_set_name
           (_fun _dm-task-pointer
                 _string/utf-8
                 --> (result : _bool)
                 --> (check-result 'dm_task_set_name result)))

(define-dm dm_task_set_uuid
           (_fun _dm-task-pointer
                 _string/utf-8
                 --> (result : _bool)
                 --> (check-result 'dm_task_set_uuid result)))

(define-dm dm_task_set_newname
           (_fun _dm-task-pointer
                 _string/utf-8
                 --> (result : _bool)
                 --> (check-result 'dm_task_set_newname result)))

(define-dm dm_task_set_newuuid
           (_fun _dm-task-pointer
                 _string/utf-8
                 --> (result : _bool)
                 --> (check-result 'dm_task_set_newuuid result)))

(define-dm dm_task_run
           (_fun _dm-task-pointer
                 --> (result : _bool)
                 --> (check-result 'dm_task_run result)))

(define-dm dm_task_get_info
           (_fun _dm-task-pointer
                 (info : (_ptr o _struct-dm-info))
                 --> (result : _bool)
                 --> (begin
                       (check-result 'dm_task_get_info result)
                       info)))

(define-dm dm_task_update_nodes
           (_fun --> _void))

(define-dm dm_task_add_target
           (_fun _dm-task-pointer
                 _uint64
                 _uint64
                 _string/utf-8
                 _string/utf-8
                 --> (result : _bool)
                 --> (check-result 'dm_task_add_target result)))

(define-dm dm_get_next_target
           (_fun _dm-task-pointer
                 _pointer
                 (start : (_ptr o _uint64))
                 (length : (_ptr o _uint64))
                 (ttype : (_ptr o _string/utf-8/free))
                 (params : (_ptr o _string/utf-8/free))
                 --> (next : _pointer)
                 --> (values start length ttype params next)))

(define-dm dm_task_get_names
           (_fun _dm-task-pointer
                 --> (result : _dm-names-pointer)
                 --> (with-finalizer result free)))


; vim:set ts=2 sw=2 et:
