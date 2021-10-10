#lang racket
(provide (rename-out (notes-read-syntax read-syntax))
         parse-notes)
(require syntax/readerr)

(define (notes-read-syntax src in)
  (datum->syntax
   #f
   `(module notes racket
      (require "notes.rkt")
      (notes
       ,@(parse-notes src in)))))

;; `src` - context information about what file this code resides in. The value
;; is provided by the racket system
;; `in` - input-port - a reference to an open file "or something like that"[sic.]
;; (parse-notes src in)
(define (parse-notes src in)
  (define note (parse-note src in))
  (if (eof-object? note)
      '()
      (cons note (parse-notes src in))))

(define (parse-note src in)
  (regexp-try-match #px"^\\s+" in)
  (if (eof-object? (peek-char in))
      eof
      (let ()
        (define note-content (parse-note-content src in))
        note-content
        ;; `(,note-content)
        )))

(define (skip-whitespace in)
  (regexp-try-match #px"^[ \t]+" in))

(define (complain src in msg)
  (define-values (line col pos) (port-next-location in))
  (raise-read-error msg src line col pos 1))

(define (last-index-of str substr)
  (cdar
   (regexp-match-positions
    (regexp (format "(~a).*" substr))
    str)))

(define (next-token-x src in (peek? #f))
  (skip-whitespace in)
  (define match-fn (if peek? regexp-match-peek regexp-try-match))
  (cond
    ;; note is a single line:
    #;((match-fn #rx"[^\n]*" in)
     #;(match-fn #rx".*?\n" in)
     => (lambda (match)
          (let ((fst (car match)))
            (printf "next-token: string? ~a; bytes?: ~a; ~a\n" (string? fst) (bytes? fst) fst)
            fst)))

    ;; note is a block of lines
    ((match-fn #rx".*?(\n\n|\n$|$)" in)
     => (lambda (match-raw)
          (let* ((match (map bytes->string/utf-8 match-raw))
                 (fst (car match))
                 (snd (cadr match)))
            ((compose
              string->bytes/utf-8
              (lambda (substr-index)
                (if (zero? substr-index)
                    fst
                    (substring fst 0 substr-index)))
              last-index-of)
             fst snd))))

    ((eof-object? (peek-char in))
     eof)
    ((equal? #\newline (peek-char in))
     (read-char in)
     eof)
    ((match-fn "^$" in)
     eof)
    (else
     (complain src in "unknown lexeme"))))

(define (next-token src in (peek? #f))
  (let ((nt (next-token-x src in peek?)))
    #;(printf "peek?: ~a; next-token: ~a~n" peek? nt)
    nt))

(define (parse-note-content src in)
  (define first-token (next-token src in))
  (when (eof-object? first-token)
    (error "eof detected"))
  (cond
    #;((eq? 'PRINT first-token)
     `(found ,@(parse-arguments src in)))
    (#t
     first-token)))
