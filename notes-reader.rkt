#lang racket
(provide (rename-out (notes-read-syntax read-syntax)))
(require syntax/readerr)

(define (notes-read-syntax src in)
  (datum->syntax
   #f
   `(module notes racket
      (require "notes.rkt")
      (notes
       ,@(parse-notes src in)))))

(define (parse-notes src in)
  (define note (parse-line src in))
  (if (eof-object? note)
      '()
      (cons note (parse-notes src in))))

;; `src` - context information about what file this code resides in. The value
;; is provided by the racket system
;; `in` - input-port - a reference to an open file "or something like that"[sic.]
(define (parse-line src in)
  (regexp-try-match #px"^\\s+" in)
  (if (eof-object? (peek-char in))
      eof
      (let ()
        ;;(define line-number (get-line-number src in))
        (define line (parse-command src in))
        `(,line))))

(define (skip-whitespace in)
  (regexp-try-match #px"^[ \t]+" in))

(define (complain src in msg)
  (define-values (line col pos) (port-next-location in))
  (raise-read-error msg src line col pos 1))

(define (next-token src in (peek? #f))
  (skip-whitespace in)
  (define match (if peek? regexp-match-peek regexp-try-match))
  (cond
    ((match #rx"^(PRINT|GOTO|GOSUB|RETURN|IF|THEN|ELSE|\\*|\\+|-|/|=|<=|>=|<|>)" in)
     => (lambda (match)
          (string->symbol (bytes->string/utf-8 (car match)))))
    ((match #rx"^\\(" in)
     'open-paren)
    ((match #rx"^\\)" in)
     'closed-paren)
    ((match #rx"^," in)
     'comma)
    ((match #rx"^[0-9]+" in)
     => (lambda (match)
          (string->number (bytes->string/utf-8 (car match)))))
    ((match #rx"^[a-zA-Z]+$?" in)
     => (lambda (match)
          (string->symbol (bytes->string/utf-8 (car match)))))
    ((match #rx"\"([^\"]+)\"" in)
     => (lambda (match)
          (bytes->string/utf-8 (cadr match))))
    ((eof-object? (peek-char in))
     eof)
    ((equal? #\newline (peek-char in))
     (read-char in)
     eof)
    ((match "^$" in)
     eof)
    (else
     (complain src in "unknown lexeme"))))

(define (parse-command src in)
  (define first-token (next-token src in))
  (when (eof-object? first-token)
    (error "no command after line number"))
  (cond
    ((eq? 'PRINT first-token)
     `(found ,@(parse-arguments src in)))
    ((eq? 'GOTO first-token)
     `(found ,(get-line-number src in)))))

