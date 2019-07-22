#lang br/quicklang
(require gregor)
(require dyoo-while-loop)

(define-macro (ac-module-begin PARSE-TREE)
  #'(#%module-begin
    PARSE-TREE))
(provide (rename-out [ac-module-begin #%module-begin]))

;;;;;;;;;;;;;;;;;;;;;;
;; ac-program funcs ;;
;;;;;;;;;;;;;;;;;;;;;;

(define (fold-funcs apl ac-funcs)
  (for/fold ([current-apl apl])
            ([ac-func (in-list ac-funcs)])
    (define ret (apply ac-func current-apl))
    (if (equal? (length ret) 2) ret (cdr ret))))

(define-macro (ac-program ENTRIES ...)
  #'(begin
      (define ledger (list empty null))
      (void (fold-funcs ledger (list ENTRIES ...)))))
(provide ac-program)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; journal-entry funcs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (journal-entry INFO ...)
  #'(lambda (ledger date)
      (define entry (list INFO ...))
      (define dt (first entry))
      (set! entry (rest entry))
      (define d (first entry))
      (set! entry (rest entry))
      (define c (first entry))
      (define e (list dt d c))
      (if (null? date)
          (set! date dt)
          (if (date<? dt date)
              (error "incorrect order of date")
              (set! date dt)))
      (list (cons e ledger) date)))
(provide journal-entry)

;;;;;;;;;;;;;;;;;;;;;;
;; entry-date funcs ;;
;;;;;;;;;;;;;;;;;;;;;;

(define-macro (entry-date DATE ...)
  #' (iso8601->date (apply string-append (list DATE ...))))
(provide entry-date)

;;;;;;;;;;;;;;;;;;
;; debits funcs ;;
;;;;;;;;;;;;;;;;;;

(define-macro (debits DEBITS ...)
  #' (fold-accs empty (list DEBITS ...))
  )
(provide debits)

;;;;;;;;;;;;;;;;;
;; debit funcs ;;
;;;;;;;;;;;;;;;;;

(define-macro (debit INFO ...)
  #' (fold-accs empty (list INFO ...))
  )
(provide debit)

;;;;;;;;;;;;;;;;;;;
;; credits funcs ;;
;;;;;;;;;;;;;;;;;;;

(define-macro (credits CREDITS ...)
  #' (fold-accs empty (list CREDITS ...)))
(provide credits)

;;;;;;;;;;;;;;;;;;
;; credit funcs ;;
;;;;;;;;;;;;;;;;;;

(define-macro (credit INFO ...)
  #' (fold-accs empty (list INFO ...))
  )
(provide credit)

;;;;;;;;;;;;;;;;;;;
;; account funcs ;;
;;;;;;;;;;;;;;;;;;;

(define-macro (account ACT)
  #' ACT)
(provide account)

;;;;;;;;;
;; amt ;;
;;;;;;;;;

(define-macro (amt NUM)
  #' NUM)
(provide amt)

;;;;;;;;;;;;;;;;;;;
;; command funcs ;;
;;;;;;;;;;;;;;;;;;;

(define-macro (command COMMAND ...)
  #' "command")
(provide command)

;;;;;;;;;;;;;;;;
;; show funcs ;;
;;;;;;;;;;;;;;;;

(define (show-entries dates journal date)
  (if (date? (car dates))
      (for/fold ([entries empty])
                ([date (in-list dates)])
        (cons (for/fold ([entry empty])
                        ([e (in-list journal)])
                (cond
                  [(date=? (car e) date) e]
                  [else entry])) entries))
      (if (number? (car dates))
          (car dates)
          (if (string? (car dates))
              (car dates)
              (car (apply (car dates) (list journal date)))
              )
          )
      )
  )
  

(define-macro (show ARGS ...)
  #' (lambda (journal date)
       (define args (cdr (list ARGS ...)))
       (if (empty? args) (displayln journal) (displayln (show-entries args journal date)))
       (list journal date)))
(provide show)

;;;;;;;;;;;;;;;;;
;; clear funcs ;;
;;;;;;;;;;;;;;;;;

(define (delete-entries date journal)
  
  (if (date? date)
      (for/fold ([entry empty])
                ([e (in-list journal)])
        (cond
          [(date=? (car e) date) entry]
          [else (cons e entry)]))
      (list-tail journal date)
      )
  )

(define-macro (clear ARGS ...)
  #' (lambda (journal date)
       (define args (cdr (list ARGS ...)))
       (if (empty? args) (list empty null) (list (delete-entries (car args) journal) date))))
(provide clear)

;;;;;;;;;;;;;;;
;; len funcs ;;
;;;;;;;;;;;;;;;

(define-macro (len ARG)
  #' (lambda (journal date)
       (list (length journal) journal date)))
(provide len)

;;;;;;;;;;;;;;;;;;
;; ledger funcs ;;
;;;;;;;;;;;;;;;;;;

(define (fold-accs ledger acs)
  (for/fold ([current-apl ledger])
            ([ac (in-list acs)])
    (cons ac current-apl)))

(define-macro (ledger ARG)
  #' (lambda (journal date)
      (print-ledger journal)
       (list journal date)))
(provide ledger)

(define (print-ledger journal)
  (define ast (assets))
  (define lbt (liabilities))
  (define r (rde))
  (display (for/fold ([cl (list ast lbt r)])
            ([e (in-list journal)])
    (set! e (rest e))
    (define d (first e))
    (set! e (rest e))
    (define c (first e))
    (add-to-ledger cl d c)))
  journal)

(define (add-to-ledger l d c)
  (define l-one (for/fold ([led l])
            ([ds (in-list d)])
    (define val (first ds))
    (set! ds (rest ds))
    (define ac (first ds))
    (cond
      [(equal? "cash" ac) (access-cash-d led val)]
      [(equal? "equipment" ac) (access-equipment-d led val)]
      [else (access-supplies-d led val)])))
  (for/fold ([led l-one])
            ([cs (in-list c)])
    (define val (first cs))
    (set! cs (rest cs))
    (define ac (first cs))
    (cond
      [(equal? "cash" ac) (access-cash-c led val)]
      [else (access-stock-c led val)])))

(define (access-cash-d led val)
  (define ats (first led))
  (define fst (first (assets-cash ats)))
  (set! fst (cons val fst))
  (define c (list fst (second (assets-cash ats))))
  (set-assets-cash! ats c)
  (list ats (second led) (third led)))

(define (access-equipment-d led val)
  (define ats (first led))
  (define fst (first (assets-equipment ats)))
  (set! fst (cons val fst))
  (define c (list fst (second (assets-equipment ats))))
  (set-assets-equipment! ats c)
  (list ats (second led) (third led)))

(define (access-supplies-d led val)
  (define ats (first led))
  (define fst (first (assets-supplies ats)))
  (set! fst (cons val fst))
  (define c (list fst (second (assets-supplies ats))))
  (set-assets-supplies! ats c)
  (list ats (second led) (third led)))

(define (access-cash-c led val)
  (define ats (first led))
  (define snd (second (assets-cash ats)))
  (set! snd (cons val snd))
  (define c (list (first (assets-cash ats)) snd))
  (set-assets-cash! ats c)
  (list ats (second led) (third led)))

(define (access-stock-c led val)
  (define ats (third led))
  (define snd (second (rde-stock ats)))
  (set! snd (cons val snd))
  (define c (list (first (rde-stock ats)) snd))
  (set-rde-stock! ats c)
  (list (first led) (second led) ats))

(struct assets ([cash #:auto #:mutable] [equipment #:auto #:mutable] [supplies #:auto #:mutable])
  #:auto-value '(() ())
  #:transparent)

(struct liabilities ([accounts-payable #:auto #:mutable] [unearned-revenue #:auto #:mutable])
  #:auto-value '(() ())
  #:transparent)

(struct rde ([service-revenue #:auto #:mutable] [dividend #:auto #:mutable] [stock #:auto #:mutable])
  #:auto-value '(() ())
  #:transparent)

;;;;;;;;;;;;;;;;
;; loop funcs ;;
;;;;;;;;;;;;;;;;

(define-macro (loop "/" ARGS ... "/" ACLINES ... "?")
  #' (lambda (journal date)
       (define cond (list ARGS ...))
       (define condition (if (equal? (length cond) 1)
                             (set! cond (car (apply (car cond) (list journal date))))
                             (if (date? (car (apply (car (list-tail cond 1)) (list journal date))))
                                 ((get-date-func (car (apply (car cond) (list journal date))))
                                  (car (apply (car (list-tail cond 1)) (list journal date)))
                                  (car (apply (car (list-tail cond 2)) (list journal date))))
                                 ((car (apply (car cond) (list journal date)))
                                  (car (apply (car (list-tail cond 1)) (list journal date)))
                                  (car (apply (car (list-tail cond 2)) (list journal date)))))))
       (define contents (list ACLINES ...))
       (define args (list journal date))
       (while condition
              (set! condition (if (equal? (length cond) 1)
                                  (set! cond (car (apply (car cond) args)))
                                  (if (date? (car (apply (car (list-tail cond 1)) args)))
                                      ((get-date-func (car (apply (car cond) args)))
                                       (car (apply (car (list-tail cond 1)) args))
                                       (car (apply (car (list-tail cond 2)) args)))
                                      ((car (apply (car cond) (list journal date)))
                                       (car (apply (car (list-tail cond 1)) args))
                                       (car (apply (car (list-tail cond 2)) args))))))
              (set! args (fold-funcs args contents))
              )
       args))
(provide loop)

;;;;;;;;;;;;;;;;;;;;;;;
;; conditional funcs ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (conditional "[" ARGS ... "]" CMF ":" CMS)
  #' (lambda (journal date)
       (define cond (list ARGS ...))
       (if (equal? (length cond) 1)
           (set! cond (car (apply (car cond) (list journal date))))
           (if (date? (car (apply (car (list-tail cond 1)) (list journal date))))
                      (set! cond ((car (apply (get-date-func (car (apply (car cond) (list journal date)))) (list journal date)))
                                  (car (apply (car (list-tail cond 1)) (list journal date)))
                                  (car (apply (car (list-tail cond 2)) (list journal date)))))
                      (set! cond ((car (apply (car cond) (list journal date)))
                                  (car (apply (car (list-tail cond 1)) (list journal date)))
                                  (car (apply (car (list-tail cond 2)) (list journal date)))))))
       (if cond
           (apply CMF (list journal date))
           (apply CMS (list journal date)))))
(provide conditional)

;;;;;;;;;;;;;;;;;
;; bool funcs ;;
;;;;;;;;;;;;;;;;;

(define-macro (bool ARG)
  #' (lambda (journal date)
      (if (equal? ARG "#t")
                  (list #t journal date)
                  (list #f journal date))))
(provide bool)

;;;;;;;;;;;;;;;
;; int funcs ;;
;;;;;;;;;;;;;;;

(define-macro (int ARG)
  #' (lambda (journal date)
       (list ARG journal date)))
(provide int)

;;;;;;;;;;;;;;;;
;; date funcs ;;
;;;;;;;;;;;;;;;;

(define-macro (date ARGS ...)
  #' (lambda (journal date)
       (define args (list ARGS ...))
       (if (= (length args) 1)
           (list date journal date)
           (list (iso8601->date (car (cdr args))) journal date))))
(provide date)

;;;;;;;;;;;;;;;;;
;; bool funcs ;;
;;;;;;;;;;;;;;;;;

(define-macro (bool-func ARG)
  #' (lambda (journal date)
       (if (equal? ARG "<")
           (list < journal date)
           (if (equal? ARG ">")
               (list > journal date)
               (if (equal? ARG "<=")
                   (list <= journal date)
                   (if (equal? ARG ">=")
                       (list >= journal date)
                       (list equal? journal date)))))))
(provide bool-func)

(define-macro (get-date-func ARG)
  #' (lambda (journal date)
       (if (equal? ARG <)
           (list date<? journal date)
           (if (equal? ARG >)
               (list date>? journal date)
               (if (equal? ARG <=)
                   (list date<=? journal date)
                   (if (equal? ARG >=)
                       (list date>=? journal date)
                       (list date=? journal date)))))))
(provide bool-func)