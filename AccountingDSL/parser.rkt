#lang brag

ac-program : [@ac-line] (/NEWLINE [@ac-line])*
ac-line : journal-entry | @command | conditional | @value | loop


conditional : "["((bool-func @value @value) | bool) "]" /"?" @command ":" @command
loop : "/"((bool-func @value @value) | bool) "/"  (@ac-line )* "?"
bool-func: "<" | "<=" | ">" | ">=" | "=="
value : len | bool | int | date
int : INTEGER
len : "len"
bool : BOOL
date : "date" [DATE]


command : show | clear | ledger
show : "show" ([entry-date] (entry-date)* | [ACCOUNT] | [INTEGER] | [len] | [date])
clear : "clear" ([entry-date] | [INTEGER])
ledger : "ledger"


journal-entry : entry-date /"{" debits /"}" /"{" credits /"}"
entry-date : DATE
debits : debit (/"," debit)*
credits : credit (/"," credit)*
debit : (account amt){1}
credit : (account amt){1}
account : ACCOUNT
amt : INTEGER

