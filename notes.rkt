#lang racket
(provide notes)
(require
 (for-syntax syntax/parse
             racket/function ;; curry
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
    ((notes
      text:string
      note:expr ...)
     #`(begin
         #;#,@((compose
             (curry map number->string)
             (curry filter (lambda (n) (odd? n))))
               '(1 2 3 4))
         #,@((compose
              (curry map bytes->string/utf-8)
              (curry filter (curry regexp-match
                                   #;#px"^.*labor.*"
                                   ;; #rx"^.*labor.*"
                                   (regexp (format "^.*~a.*"
                                                   (syntax->datum #`text)))
                                   ))
              (curry map syntax->datum))
             (syntax->list #`(note ...)))))))

#|
(define t1
  "aaa
bbb

ccc")

(define t2
  "aaa
bbb
ccc")

(define t3
  "aaa
bbb
ccc
")


(define t4
  "aaaa
abbb
accc


baaa
bbbb
bccc
")

(define t
  "1 Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
2 incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
3 nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

4 Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
5 eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt
6 in culpa qui officia deserunt mollit anim id est laborum.


aaa
bbb

ccc
")

(regexp-match #rx".*?(\n\n|\n$|$)" t1)
;; => '("aaa\nbbb\n\n" "\n\n")
(regexp-match #rx".*?(\n\n|\n$|$)" t2)
;; => '("aaa\nbbb\nccc" "")
(regexp-match #rx".*?(\n\n|\n$|$)" t3)
;; => '("aaa\nbbb\nccc\n" "\n")
(regexp-match #rx".*?(\n\n|\n$|$)" t4)
|#
