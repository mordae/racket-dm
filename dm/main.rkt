#lang racket/base
;
; Device Mapper Tools
;

(require
  (rename-in ffi/unsafe (-> -->)))

(require racket/contract
         racket/string)

(require williams/uuid1/uuid)

(require "private/ffi.rkt")

(provide
  (contract-out
    (dm-info? predicate/c)
    (dm-target? predicate/c)
    (dm-info-exists? (-> dm-info? boolean?))
    (dm-info-suspended? (-> dm-info? boolean?))
    (dm-info-live-table? (-> dm-info? boolean?))
    (dm-info-inactive-table? (-> dm-info? boolean?))
    (dm-info-open-count (-> dm-info? integer?))
    (dm-info-event-number (-> dm-info? integer?))
    (dm-info-major (-> dm-info? integer?))
    (dm-info-minor (-> dm-info? integer?))
    (dm-info-read-only? (-> dm-info? boolean?))
    (dm-info-target-count (-> dm-info? integer?))
    (dm-target-start (-> dm-target? integer?))
    (dm-target-length (-> dm-target? integer?))
    (dm-target-type (-> dm-target? symbol?))
    (dm-target-params (-> dm-target? (listof (or/c string? integer?))))
    (dm-list (-> (listof string?)))
    (dm-get-info (-> string? dm-info?))
    (dm-get-table (-> string? (listof dm-target?)))

    (dm-suspend! (-> string? void?))
    (dm-resume! (-> string? void?))
    (dm-remove! (-> string? void?))
    (dm-rename! (->* (string? string?) (#:new-uuid (or/c #f string?)) void?))
    (dm-reload! (->* (string?) () #:rest (listof dm-target?) void?))

    (dm-create!
      (->* (string?)
           (#:uuid (or/c #f string?))
           #:rest (listof dm-target?)
           void?))))


(struct dm-info
  (exists? suspended? live-table? inactive-table? open-count
   event-number major minor read-only? target-count)
  #:transparent)

(struct dm-target
  (start length type params)
  #:transparent)


(define (number->string/safe maybe-number)
  (if (number? maybe-number)
      (number->string maybe-number)
      maybe-number))

(define (string->number/safe string)
  (or (string->number string) string))


(define (dm-list)
  (let ((task (dm_task_create 'list)))
    (dm_task_run task)
    (for/list ((bstr (dm_task_get_names task)))
      (cast bstr _bytes _string/utf-8))))


(define (dm-get-info name)
  (let ((task (dm_task_create 'info)))
    (dm_task_set_name task name)
    (dm_task_run task)
    (apply dm-info (struct-dm-info->list (dm_task_get_info task)))))


(define (dm-get-table name)
  (let ((task (dm_task_create 'table)))
    (dm_task_set_name task name)
    (dm_task_run task)
    (let loop ((next #f))
      (let-values (((start len ttype params next)
                    (dm_get_next_target task next)))
        (if ttype
            (let ((value (dm-target start
                                    len
                                    (string->symbol ttype)
                                    (map string->number/safe
                                         (string-split params)))))
              (if next
                  (cons value (loop next))
                  (list value)))
            null)))))


(define (dm-remove! name)
  (let ((task (dm_task_create 'remove)))
    (dm_task_set_name task name)
    (dm_task_run task)
    (dm_task_update_nodes)))


(define (dm-rename! name new-name #:new-uuid (new-uuid #f))
  (let ((task (dm_task_create 'rename)))
    (dm_task_set_name task name)
    (dm_task_set_newname task new-name)
    (when new-uuid
      (dm_task_set_newuuid task new-uuid))
    (dm_task_run task)
    (dm_task_update_nodes)))


(define (dm-create! name #:uuid (uuid #f) . targets)
  (let ((task (dm_task_create 'create)))
    (dm_task_set_name task name)
    (dm_task_set_uuid task (if uuid uuid (uuid->string (make-uuid-4))))
    (for ((target targets))
      (dm_task_add_target task
                          (dm-target-start target)
                          (dm-target-length target)
                          (symbol->string (dm-target-type target))
                          (string-join (map number->string/safe
                                            (dm-target-params target)) " ")))
    (dm_task_run task)
    (dm_task_update_nodes)))


(define (dm-reload! name . targets)
  (let ((task (dm_task_create 'reload)))
    (dm_task_set_name task name)
    (for ((target targets))
      (dm_task_add_target task
                          (dm-target-start target)
                          (dm-target-length target)
                          (symbol->string (dm-target-type target))
                          (string-join (map number->string/safe
                                            (dm-target-params target)) " ")))
    (dm_task_run task)
    (dm_task_update_nodes)))


(define (dm-suspend! name)
  (let ((task (dm_task_create 'suspend)))
    (dm_task_set_name task name)
    (dm_task_run task)))


(define (dm-resume! name)
  (let ((task (dm_task_create 'resume)))
    (dm_task_set_name task name)
    (dm_task_run task)))


; vim:set ts=2 sw=2 et:
