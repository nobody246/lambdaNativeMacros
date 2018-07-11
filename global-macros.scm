(define-macro (alist-set! alst key val)
  `(set! ,alst (alist-set ,alst ,key ,val)))

(define-macro (add1! var)
  `(set! ,var (+ ,var 1)))

(define-macro (sub1! var)
  `(set! ,var (- ,var 1)))

(define-macro (add1 var)
  `(+ ,var 1))

(define-macro (sub1 var)
  `(- ,var 1))

(define-macro (add! var1 #!rest var2)
  `(set! ,var1 (+ ,var1 ,@var2)))

(define-macro (sub! var1 #!rest var2)
  `(set! ,var1 (- ,var1 ,@var2)))

(define-macro (string-append! str #!rest strings..)
  `(set! ,str (string-append ,str ,@strings..)))

;(define-macro (stringlist-append strings..)
;  `(let ((s ,strings..))
;     (eval `(string-append ,@s))))

(define-macro (stringlist-append strings..)
  `(let ((s ,strings..))
     (apply string-append s)))

(define-macro (string-join strings.. #!optional (delim " "))
  `(let ((retv ""))
     (let loop ((i 0))
       (if (< i (length ,strings..))
           (begin (string-append! retv
                                  (list-ref ,strings.. i)
                                  ,delim)
                  (loop (add1 i 1)))
           retv))))

(define-macro (symbol-append #!rest vars..)
  `(string->symbol
    (stringlist-append
     (map symbol->string (list ,@vars..)))))
     
(define-macro (symbol-append! sy #!rest symbols..)
  `(set! ,sy (symbol-append ,sy ,@symbols..)))

(define-macro (append! li #!rest lists..)
  `(set! ,li (append ,li ,@lists..)))

(define-macro (char->string chr)
  `(list->string (list ,chr)))

(define-macro (numeric-char->number chr)
  `(string->number (char->string ,chr)))

;sprintf, takes ~A for obj ~S for "string" ~% and ~N for line-break
(define-macro (sprintf #!rest args..)
  `(let* ((a (list ,@args..))
          (arg-cnt (length a))
          (fmt-str (if (> arg-cnt 0)
                       (car a)
                       #f))
          (fmt-args (if (and fmt-str
                             (> arg-cnt 1))
                        (cdr a)
                        '()))
          (fmt-flag-mode #f)
          (ret ""))
     (for-each
      (lambda(x)
        (let* ((fmt-arg (if (and fmt-args (not (null? fmt-args)))
                            (car fmt-args)
                            ""))
               (fmt-arg (if (not (string? fmt-arg))
                            (with-output-to-string "" (lambda() (write fmt-arg)))
                            fmt-arg)))
          (cond ((and (not fmt-flag-mode) (eq? x #\~))
                 (set! fmt-flag-mode #t))
                (fmt-flag-mode
                 (set! fmt-flag-mode #f)
                 (cond ((eq? x #\~)
                        (string-append! ret "~"))
                       ((eq? x #\A)
                        (string-append! ret fmt-arg)
                        (if (not (null? fmt-args))
                            (set! fmt-args (cdr fmt-args))))
                       ((eq? x #\S)
                        (string-append! ret (string-append "\"" fmt-arg "\""))
                        (if (not (null? fmt-args))
                            (set! fmt-args (cdr fmt-args))))
                       ((or (eq? x #\N) (eq? x #\%))
                        (string-append! ret (char->string #\newline)))
                       (else
                        (raise (string-append
                                "Invalid format marker : ~"
                                (char->string x) ".")))))
                (else (string-append! ret (char->string x))))))
      (string->list fmt-str))
     ret))
  

     
