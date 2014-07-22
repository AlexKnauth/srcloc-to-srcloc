#lang racket/base

(provide srcloc/kw
         srcloc->srcloc
         srcloc->list
         srcloc->syntax
         define-srcloc-stuff)

(require racket/match
         unstable/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     (for-syntax racket/base)))


(begin-for-syntax
  (define-syntax kw (make-rename-transformer #'keyword)))


(define-syntax define-srcloc-stuff
  (lambda (stx)
    (syntax-parse stx
      [(define-srcloc-stuff id:id)
       (with-syntax ([id.source (format-id #'id "~a.source" #'id #:source #'id)]
                     [id.line (format-id #'id "~a.line" #'id #:source #'id)]
                     [id.column (format-id #'id "~a.column" #'id #:source #'id)]
                     [id.position (format-id #'id "~a.position" #'id #:source #'id)]
                     [id.span (format-id #'id "~a.span" #'id #:source #'id)])
         #'(match-define (srcloc id.source id.line id.column id.position id.span)
             (srcloc->srcloc id)))]
      [(define-srcloc-stuff id:id srcloc-expr:expr)
       #'(begin (define id (srcloc->srcloc srcloc-expr))
                (define-srcloc-stuff id))]
      [(define-srcloc-stuff id:id (~and (~seq stuff ...) (~seq (~seq kw:kw arg:expr) ...)))
       #'(begin (define id (srcloc/kw stuff ...))
                (define-srcloc-stuff id))]
      )))
                  

(define (srcloc/kw #:source [source #f]
                   #:line [line #f]
                   #:column [column #f]
                   #:position [position #f]
                   #:span [span #f])
  (srcloc->srcloc (srcloc source line column position span)))


(define (srcloc->list loc)
  (match-define (srcloc source line column position span)
    (srcloc->srcloc loc))
  (list source line column position span))


(define (srcloc->srcloc loc)
  (match-define (or (srcloc source line column position span)
                    (list source line column position span)
                    (vector source line column position span)
                    (? syntax?
                       (as ([source (syntax-source loc)]
                            [line (syntax-line loc)]
                            [column (syntax-column loc)]
                            [position (syntax-position loc)]
                            [span (syntax-span loc)])
                         _)))
    loc)
  (cond [(and source line column position span)
         (srcloc source line column position span)]
        [(and source line column position)
         (srcloc source line column position span)]
        [(and (path-string? source) line column)
         (define position (find-position #:source source #:line line #:column column))
         (srcloc source line column position span)]
        [(and (path-string? source) position)
         (define-values (line column)
           (find-line+column #:source source #:position position))
         (srcloc source line column position span)]
        [else
         (srcloc source line column position span)]))


(define (srcloc->syntax loc #:ctxt [ctxt #f])
  (match-define (srcloc source line column position span)
    (srcloc->srcloc loc))
  (define lst (list source line column position span))
  (cond [(syntax? ctxt) (datum->syntax ctxt `(quote ,lst) lst)]
        [else
         (define namespace
           (cond [(and (module-path? source) (module-declared? source))
                  (module->namespace source)]
                 [else (current-namespace)]))
         (parameterize ([current-namespace namespace])
           (namespace-syntax-introduce
            (datum->syntax #f `(quote ,lst) lst)))]))





(define (find-position #:source source #:line line #:colunm column)
  (define in (open-input-file source))
  (define (vals #:position pos #:line line #:colunm column)
    (values pos line column))
  (define-values (position _ __)
    (for/fold ([current-position 1] [current-line 1] [current-column 0])
      ([char (in-input-port-chars in)])
      #:break (and (= current-line line)
                   (= current-column column))
      (cond [(char=? char #\newline)
             (vals #:position (add1 current-position)
                   #:line (add1 current-line)
                   #:column 0)]
            [else
             (vals #:position (add1 current-position)
                   #:line current-line
                   #:column (add1 current-column))])))
  position)


(define (find-line+column #:source source #:position position)
  (define in (open-input-file source))
  (define (vals #:position pos #:line line #:colunm column)
    (values pos line column))
  (define-values (_ line column)
    (for/fold ([current-position 1] [current-line 1] [current-column 0])
      ([char (in-input-port-chars in)])
      #:break (= current-position position)
      (cond [(char=? char #\newline)
             (vals #:position (add1 current-position)
                   #:line (add1 current-line)
                   #:column 0)]
            [else
             (vals #:position (add1 current-position)
                   #:line current-line
                   #:column (add1 current-column))])))
  (values line column))
