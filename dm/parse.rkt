#lang racket/base
;
; Detailed parameter parsers.
;

(require racket/contract
         racket/generator)

(provide (all-defined-out))


;; Convert linear multipath mapping parameters to a tree.
(define/contract (dm-parse-multipath params)
                 (-> (listof (or/c string? integer?)) hash?)
  (define next-param
    (sequence->generator (in-list params)))

  (define (next-list)
    (for/list ((i (next-param)))
      (next-param)))

  (define feature-args
    (next-list))

  (define hw-handler
    (let ((args# (next-param)))
      (and (> args# 0)
           (cons (next-param)
                 (for/list ((i args#))
                   (next-param))))))

  (define groups#
    (next-param))

  (define initial-priority-group
    (next-param))

  (define priority-groups
    (for/list ((i groups#))
      (hasheq 'selector (string->symbol (next-param))
              'args (next-list)
              'paths (let ((n-paths (next-param))
                           (n-path-args (next-param)))
                       (for/list ((i n-paths))
                         (cons (next-param)
                               (for/list ((i n-path-args))
                                 (next-param))))))))

  (hasheq 'feature-args feature-args
          'hw-handler hw-handler
          'initial-priority-group initial-priority-group
          'priority-groups priority-groups))


; vim:set ts=2 sw=2 et:
