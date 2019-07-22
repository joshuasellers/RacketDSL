# AccountingDSL

###### Description:
For this project, I created a Domain-Specific Language (DSL) for the first two steps in the Accounting Cycle.  I used the language Racket for the project.  The domain was specified to be anything contained within those first two steps in the cycle:

- Journals Entries
- Ledger accounts
- All of the information needed to make the structures

The goal of the project was to make a basic language that accountants could use to make their initial journal of entries (all of the information that is used in the later steps of the accounting process).  Then, commands could be utilized to turn that journal into things like the ledger and other data structures used further on in the cycle.

### Installing

To install the project, make sure you have Racket downloaded on your device.  Additionally, make sure you have the modules of Racket known as: Beautiful Racket (bf), while-loop, and Gregor (gregor).  These will be needed to run the DSL.  To install them, run this command:

```
raco pkg install --auto while-loop
```

### Overall Usability

There are six files for this project: the parser, the lexer, the tokenizer, the example code, the reader and the expander.  The example file contains sample code and can be edited for your convenience as you wish to test additional cases.  The parser and lexer contain the rules for the language:
- Every file is an ac-line
- each ac-line can either be a  journal-entry, command, conditional, value or loop
- The commands are show, clear and ledger
- The values are len, bool, int and date
- the journal entry is formatted as so: entry-date{debits}{credits}
- an entry date is of the form: yyyy-mm-dd
- debits and credits are lists of the debit/credit accounts and the amounts going into them



### Description
The reader uses the parser and lexer to tokenize and parse files. I don't have any specific error catching mechanisms in place, so incorrect code should return some type of parsing error or reading error.

Parsing:
```
ac-program : [@ac-line] (/NEWLINE [@ac-line])*
ac-line : journal-entry | @command | conditional | @value | loop


conditional : "["((bool-func @value @value) | bool) "]" /"?" @command ":" @command
...
```
Lexing:
```
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
```
The tokenized, lexed and parsed code is then sent to the expander.  This is were the accountingDSL code is expanding into usable racket s-expressions.  In order to more accurately mimic the format of racket, I have the the expander work in a functional manner (e.g. no stored global values or data structures).  Essentially, I fold over the ac-lines.  Each time the fold function encounters a journal-entry, it adds it to the fold output value using cons.  For the commands and other non-entry options, the action is performed.  Everything returns a list containing the current journal and date.

Sample expander code:
```
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
```
### Example Code
I made an example to show some basic usage of the DSL.  The file for this example is: `accounting-test.rkt`. 
### Next Steps
Overall, this is a solid DSL.  This was my first time working in Racket, so in the future I'd like to move away from beautiful racket and stick to the actual code (beautiful racket hides some aspects of racket to make things easier for beginners).  Also, there are a lot of rules in acounting.  I only covered a couple in this DSL, but I would need to factor in all of them for a full version of this DSL.  A full version for this DSL would be the next step (the other steps in the Accounting Cyle would mainly involved updates to my ledger code, which is very basic right now).
