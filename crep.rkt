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
                    (if (equal? case-sensitive (case-sensitivity)) " (default)" "")
                    case-insensitive
                    (if (equal? case-sensitive (case-sensitivity)) "" " (default)")))

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
   ;; TODO parameterize displayed color
   ;; TODO check if the case-sensitivity value is allowed
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

;; TODO implement interleave
;; (interleave (repeat "a") [1 2 3])
;; =>("a" 1 "a" 2 "a" 3)
;; (require racket/list)
(define (interpose elem ls)
  ;; TODO implement tail-call version of `interpose`; see also string-join
  (if (or (empty? (cdr ls)) (empty? ls))
      ls
      (append (list (car ls) elem) (interpose elem (cdr ls)))))

(define (colorize ls pattern)
  (match ls
    [(list) (color-display "")]
    [(list l) (color-display l)]
    [_
     (let ((txt (car ls)))
       (color-display txt)
       (with-colors 'red (lambda () (color-display pattern)))
       (colorize (cdr ls) pattern))]))

((compose
  (lambda (_) (display ""))
  (curry map
         (lambda (f-strs)
           (let ((strs (cdr f-strs)))
             (unless (empty? strs)
               (let* ((s (string-join strs "\n"))
                      (ptrn (pattern))
                      ;; TODO use regexp-match* instead of regexp-split.
                      ;; e.g. (regexp-match* #rx"(?i:x*)" "12X4x6")
                      (lst (regexp-split (regexp
                                          (format "(?~a:~a)"
                                                  (case-sensitivity) ptrn))
                                         s)))
                 (with-colors 'white (lambda () (color-displayln (car f-strs))))
                 (colorize lst ptrn)
                 (printf "\n\n")))
             strs)))
  #;(lambda (strs) (string-join strs "\n"))
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
               (list f strs)))
  (curry filter (curry string<? "")))
 (files))
