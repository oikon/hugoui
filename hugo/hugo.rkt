#lang racket/base

(require (for-syntax racket/base)
         racket/runtime-path)

(provide (struct-out hugo-process)
         hugo-run
         hugo-wait
         hugo-kill
         hugo-status
         hugo-pid
         hugo-dir?)

(define-runtime-path hugo-exe
  (case (system-type 'os)
    [(windows) "bin/windows/hugo.exe"]
    [(macosx) "bin/macosx/hugo"]
    ;; other platforms, drop hugo executable here...
    [else "bin/other/hugo"]))

(struct hugo-process (sp stdin stdout stderr))

(define (hugo-run #:stdout [stdout #f]
                  #:stdin  [stdin #f]
                  #:stderr [stderr #f]
                  #:callback [callback void]
                  . args)

  (define cust (make-custodian))

  (define-values (sp iport:sp-out oport:sp-in iport:sp-err)
    (parameterize ([current-custodian cust]
                   [current-subprocess-custodian-mode 'kill])
      (apply subprocess stdout stdin stderr hugo-exe args)))

  (define hp
    (hugo-process sp
                  (or oport:sp-in stdin)
                  (or iport:sp-out stdout)
                  (or iport:sp-err stderr)))

  (thread
   (lambda ()
     (dynamic-wind
       (lambda () (void))
       (lambda () (subprocess-wait sp))
       (lambda ()
         (callback hp)
         (custodian-shutdown-all cust)))))
  hp)

(define (hugo-kill hp [force? #t])
  (subprocess-kill (hugo-process-sp hp) force?))

(define (hugo-wait hp)
  (subprocess-wait (hugo-process-sp hp)))

(define (hugo-status hp)
  (subprocess-status (hugo-process-sp hp)))

(define (hugo-pid hp)
  (subprocess-pid (hugo-process-sp hp)))

(define (hugo-dir? path)
  (and (directory-exists? path)
       (ormap (lambda (x) (file-exists? (build-path path x)))
              '("config.toml" "config.json" "config.yaml"))))
