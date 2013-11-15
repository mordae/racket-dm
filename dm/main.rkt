#lang racket/base
;
; Device Mapper Tools
;

(require (rename-in ffi/unsafe (-> -->))
         williams/uuid1/uuid
         racket/contract
         racket/string)

(require "private/ffi.rkt")

(provide (all-defined-out))


(define-struct/contract dm-info
  ((exists         boolean?)
   (suspended      boolean?)
   (live-table     boolean?)
   (inactive-table boolean?)
   (open-count     integer?)
   (event-nr       integer?)
   (major          integer?)
   (minor          integer?)
   (read-only      boolean?)
   (target-count   integer?))
  #:transparent)


(define-struct/contract dm-target
  ((start  integer?)
   (length integer?)
   (type   symbol?)
   (params (listof (or/c string? integer?))))
  #:transparent)


(define/contract (dm-list)
                 (-> (listof string?))
  (let ((task (dm_task_create 'list)))
    (dm_task_run task)
    (let loop ((names (dm_task_get_names task)))
      (let ((bstr (make-sized-byte-string (ptr-add names 12)
                                          (- (dm-names-next names) 12))))
        (let ((str (string-replace (bytes->string/utf-8 bstr) "\0" "")))
          (if (> (dm-names-next names) 0)
            (cons str (loop (ptr-add names (dm-names-next names))))
            (list str)))))))


(define/contract (dm-get-info name)
                 (-> string? dm-info?)
  (let ((task (dm_task_create 'info)))
    (dm_task_set_name task name)
    (dm_task_run task)
    (apply dm-info (struct-dm-info->list (dm_task_get_info task)))))


(define/contract (dm-get-table name)
                 (-> string? (listof dm-target?))
  (let ((task (dm_task_create 'table)))
    (dm_task_set_name task name)
    (dm_task_run task)
    (let loop ((next #f))
      (let-values (((start len ttype params next)
                    (dm_get_next_target task next)))
        (let ((value (dm-target start len (string->symbol ttype)
                                          (map (lambda (p)
                                                 (if (string->number p)
                                                   (string->number p)
                                                   p))
                                                 (string-split params)))))
          (if next
            (cons value (loop next))
            (list value)))))))


(define/contract (dm-remove name)
                 (-> string? void?)
  (let ((task (dm_task_create 'remove)))
    (dm_task_set_name task name)
    (dm_task_run task)
    (dm_task_update_nodes)))


(define/contract (dm-rename name new-name #:new-uuid (new-uuid #f))
                 (->* (string? string?) (#:new-uuid (or/c #f string?)) void?)
  (let ((task (dm_task_create 'rename)))
    (dm_task_set_name task name)
    (dm_task_set_newname task new-name)
    (when new-uuid
      (dm_task_set_newuuid task new-uuid))
    (dm_task_run task)
    (dm_task_update_nodes)))


(define/contract (dm-create name #:uuid (uuid #f) . targets)
                 (->* (string?) (#:uuid (or/c #f string?))
                      #:rest (listof dm-target?) void?)
  (let ((task (dm_task_create 'create)))
    (dm_task_set_name task name)
    (dm_task_set_uuid task (if uuid uuid (uuid->string (make-uuid-4))))
    (for-each (lambda (target)
                (dm_task_add_target task
                  (dm-target-start target)
                  (dm-target-length target)
                  (symbol->string (dm-target-type target))
                  (string-join (map (lambda (p)
                                      (if (number? p)
                                        (number->string p)
                                        p))
                                    (dm-target-params target)) " ")))
              targets)
    (dm_task_run task)
    (dm_task_update_nodes)))


(define/contract (dm-reload name . targets)
                 (->* (string?) () #:rest (listof dm-target?) void?)
  (let ((task (dm_task_create 'reload)))
    (dm_task_set_name task name)
    (for-each (lambda (target)
                (dm_task_add_target task
                  (dm-target-start target)
                  (dm-target-length target)
                  (symbol->string (dm-target-type target))
                  (string-join (map (lambda (p)
                                      (if (number? p)
                                        (number->string p)
                                        p))
                                    (dm-target-params target)) " ")))
              targets)
    (dm_task_run task)
    (dm_task_update_nodes)))


(define/contract (dm-suspend name)
                 (-> string? void?)
  (let ((task (dm_task_create 'suspend)))
    (dm_task_set_name task name)
    (dm_task_run task)))


(define/contract (dm-resume name)
                 (-> string? void?)
  (let ((task (dm_task_create 'resume)))
    (dm_task_set_name task name)
    (dm_task_run task)))


; vim:set ts=2 sw=2 et:
