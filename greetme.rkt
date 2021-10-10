#lang racket

(require "notes.rkt")
(require
 "notes-reader.rkt"
 #;(for-syntax
  ;; syntax/parse
  racket/syntax
  ;; "notes-reader.rkt"
  "notes.rkt"))


;; parameter My-Name is one of:
;; - #false
;; - String
(define my-name (make-parameter #false))

;; command line parser
(define parser
  (command-line
   #:usage-help
   "Have the computer greet you!"

   #:once-each
   [("-n" "--name") NAME
                    "Set your name"
                    (my-name NAME)]

   #:args () (void)))

;; get-greeting : My-Name -> String
;; Gets the greeting for the given My-Name
(define (get-greeting mn)
  (cond
    [(boolean? mn) "Hello, unknown person!"]
    [(string? mn) (string-append "Hello, " mn "!")]))

;; prints result to the command line
#;(printf "~a\n" (get-greeting (my-name)))


;; echo '#lang reader "notes-reader.rkt"' | cat - nds.org > nds.rkt
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
(define in (open-input-string t))
(define src #f)

(define exp `(notes ,@(cons (my-name) (parse-notes src in))))

;; the expression must be evaluated in a namespace.
;; Thanks to https://stackoverflow.com/q/16266934 for a hint
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(printf "~a" (eval exp ns))


