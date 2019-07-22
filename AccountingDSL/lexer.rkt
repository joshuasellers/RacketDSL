#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define-lex-abbrev date (:: (:= 4 (char-set "0123456789")) "-" (:** 1 2 (char-set "0123456789")) "-" (:** 1 2 (char-set "0123456789")) ))

(define-lex-abbrev account (intersection (:+ alphabetic) (complement "ledger") (complement "date") (complement "show") (complement "clear") (complement "len")))

(define-lex-abbrev bool (union "#t" "#f"))

(define basic-lexer
  (lexer-srcloc
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [date (token 'DATE lexeme)]
   [digits (token 'INTEGER (string->number lexeme))]
   [account (token 'ACCOUNT lexeme)]
   [bool (token 'BOOL lexeme)]
   [(:or "clear" "ledger" "show" "len" "date" ">" "<" "=" "<=" ">=") (token lexeme lexeme)]
   [(char-set "/=%<>{}#[],?:") lexeme]))

(provide basic-lexer)