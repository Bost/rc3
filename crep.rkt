#lang racket

(require
 ;; (prefix-in com: "common.rkt")
 "notes.rkt" ;; is used indeed
 "notes-reader.rkt"
 ansi-color
 racket/list
 racket/match)

(define pattern (make-parameter ""))
(define files (make-parameter (list)))

(define case-sensitive "i")
(define case-insensitive "-i")

(define case-sensitivity (make-parameter case-sensitive))

(define csd (format "case-sensitive `~a`~a or case-insensitive `~a`~a search."
                    case-sensitive
                    (if (eq? case-sensitive (case-sensitivity)) " (default)" "")
                    case-insensitive
                    (if (eq? case-sensitive (case-sensitivity)) "" " (default)")))

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
   [("-c" "--case-sensitivity") cs
                                (csd)
                                (case-sensitivity cs)]
   [("-p" "--pattern") NAME
                    "Search pattern"
                    (pattern NAME)]

   ;; no other arguments are accepted
   #;#;#;#:args () (void)))

(define add-src-location-info #f)
(define-namespace-anchor a)

(define (colorize ls pattern)
  (match ls
    [(list) (color-display "")]
    [(list l) (color-display l)]
    [_
     (let ((txt (car ls)))
       #;(printf "~s\n" 'else)
       (color-display txt)
       (with-colors 'red (lambda () (color-display pattern)))
       (colorize (cdr ls) pattern))]))

((compose
  #;(curry printf "~a")
  ;; if last elem is "" add the pattern put the (pattern) before it
  (lambda (s)
    (let* ((ptrn (pattern))
           (lst (regexp-split (regexp (format "(?~a:~a)" (case-sensitivity) ptrn))
                              s)))
      (colorize lst ptrn)))
  (lambda (strs) (string-join strs "\n"))
  (curry map (lambda (f)
               (define inf (open-input-file f) #;(open-input-string t))
               (define exp
                 `(notes ,@(cons (pattern)
                                 (cons
                                  (case-sensitivity)
                                  (parse-notes add-src-location-info inf)))))
               ;; the expression must be evaluated in a namespace.
               ;; Thanks to https://stackoverflow.com/q/16266934 for a hint
               (define ns (namespace-anchor->namespace a))
               (define strs (string-join (eval exp ns) "\n\n"))
               #;(close-input-port inf)
               (format "~a\n~a\n" f strs)))
  (curry filter (curry string<? "")))
 (files))
