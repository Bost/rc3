#lang racket

(require "notes.rkt"
         "notes-reader.rkt")

(define pattern (make-parameter ""))
(define files (make-parameter ""))

;; Thanks to https://jackwarren.info/posts/guides/racket/racket-command-line/
(define parser
  (command-line
   #:usage-help
   "Search in note-file(s) for a pattern. Return note-block(s)"

   #:once-each
   [("-f" "--files") NAME
                    "File(s) to search through"
                    (files NAME)]
   [("-p" "--pattern") NAME
                    "Search pattern"
                    (pattern NAME)]

   #:args () (void)))

;; echo '#lang reader "notes-reader.rkt"' | cat - nds.org > nds.rkt

(define t
  "a b c

a x c

aa bb cc

aaa bbb ccc
")
(define in
  (open-input-file (files))
  #;(open-input-string t))

(define src #f)

(define exp `(notes ,@(cons (pattern) (parse-notes src in))))
#;exp
;; the expression must be evaluated in a namespace.
;; Thanks to https://stackoverflow.com/q/16266934 for a hint
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(printf "~a\n" (string-join (eval exp ns) "\n\n"))



