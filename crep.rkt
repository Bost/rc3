#lang racket

(require
 ;; (prefix-in com: "common.rkt")
 "notes.rkt" ;; is used indeed
 "notes-reader.rkt")

(define pattern (make-parameter ""))
(define files (make-parameter (list)))

;; Thanks to https://jackwarren.info/posts/guides/racket/racket-command-line/
(define parser
  (command-line
   #:usage-help
   "Search in note-file(s) for a pattern. Return note-block(s).
E.g.:
  racket crep.rkt -f ~/dev/notes/org-roam/20210805195404-guix.org -p title

  racket crep.rkt -f ~/dev/notes/org-roam/20210805195404-guix.org \\
                  -f ~/dev/notes/org-roam/20210729234518-racket.org \\
                  -p title"
   #:multi
   [("-f" "--files") fs
                     "File(s) to search through"
                     (files
                      ;; `files` was initialized to be '()
                      (append (files) (list fs)))]
   #:once-each
   [("-p" "--pattern") NAME
                    "Search pattern"
                    (pattern NAME)]

   ;; no other arguments are accepted
   #;#;#;#:args () (void)))

(define add-src-location-info #f)
(define-namespace-anchor a)

((compose
  (curry printf "~a")
  (lambda (strs) (string-join strs "\n"))
  (curry map (lambda (f)
               (define inf (open-input-file f) #;(open-input-string t))
               (define exp
                 `(notes ,@(cons (pattern)
                                 (parse-notes add-src-location-info inf))))
               ;; the expression must be evaluated in a namespace.
               ;; Thanks to https://stackoverflow.com/q/16266934 for a hint
               (define ns (namespace-anchor->namespace a))
               (define strs (string-join (eval exp ns) "\n\n"))
               #;(close-input-port inf)
               (format "~a\n~a\n" f strs)))
  (curry filter (curry string<? "")))
 (files))
