#lang br
(require "lexer.rkt" brag/support)

(define (make-tokenizer port)
  (define (next-token) (basic-lexer port))
  next-token)

(provide make-tokenizer)