#lang racket
(provide notes)
(require (for-syntax syntax/parse
                     racket/syntax
                     syntax/id-set))

;; Filter out lines containing given string. E.g. 'labor' returns lines 2, 3, 6
#|
1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
2 incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
3 nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
4 Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
5 eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt
6 in culpa qui officia deserunt mollit anim id est laborum.
|#

(define-syntax (notes form)
  (syntax-parse form
    ((notes note:expr ...)
     #`(begin
         #,@(map (lambda (note-content)
                   #;(printf "note-content: ~a~n" note-content)
                   #`(printf "note-content: ~a~n" #,note-content)
                   )
                 (syntax->list #`(note ...)))
         #;#,@(filter (lambda (note)
                      (regexp-match #px"^.*labor.*" note))
                    (syntax->list #`(note ...)))))))


