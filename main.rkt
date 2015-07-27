#lang racket

(require "hugo/hugo.rkt"
         racket/gui
         syntax/parse
         net/sendurl
         framework
         (for-syntax racket/syntax
                     syntax/parse))

(application:current-app-name "Hugo GUI")

(define frame
  (new frame% [label (application:current-app-name)]))

(define (maybe-type test)
  (lambda (x)
    (or (eq? #f x)
        (test x))))

(define path-field%
  (class object%
    (init-field parent label [default ""] [type 'directory]
                [update #f])

    (define key (string->symbol label))
    (preferences:set-default key default string?)

    (define path (preferences:get key))

    (define panel (new horizontal-panel% [parent parent]))

    (define choose-button
      (new button%
           [label label]
           [parent panel]
           [callback (lambda (btn evt)
                       (define selection
                         (case type
                           [(directory) (get-directory)]
                           [(file) (get-file)]))
                       (when selection
                         (set-path selection)))]))

    (define value-message
      (new message%
           [label (preferences:get key)]
           [parent panel]
           [auto-resize #t]))

    (define update-button
      (and update
           (new button%
                [label "\U21BB"]
                [parent panel]
                [callback update])))

    (define/public (enable ?)
      (send choose-button enable ?)
      (send value-message enable ?)
      (when update-button
        (send update-button enable ?)))

    (define/public (get-path) (string->path path))
    (define/public (set-path p)
      (set! path (path->string p))
      (preferences:set key path)
      (send value-message set-label path))

    (super-new)))

(define blog-folder
  (new path-field%
       [parent frame]
       [label "Blog"]))

(define public-folder
  (new path-field%
       [parent frame]
       [label "Public"]
       [update
        (lambda (c e)
          (send public-folder
                set-path (build-path (send blog-folder get-path) "public/")))]))

(define hugo (make-parameter #f))
(define (is-running? hp)
  (and (hugo-process? hp)
       (eq? 'running (hugo-status hp))))

(define (enable x) (send x enable #t))
(define (disable x) (send x enable #f))

(define (hugo-server-start/stop btn evt)

  (define (start-hook)
    (map enable (list open-button))
    (map disable (list blog-folder public-folder build-drafts? build-future?)))

  (define (stop-hook)
    (map enable (list blog-folder public-folder build-drafts? build-future?))
    (map disable (list open-button)))

  (define (when-arg test arg)
    (if test arg ""))

  (cond

    ;; stop
    [(is-running? (hugo))
     (hugo-kill (hugo))
     (send btn set-label "Start")
     (stop-hook)]

    ;; start
    [else
     (when (ensure-hugo-blog (send blog-folder get-path))
       (send btn set-label "Stop")
       (start-hook)
       (hugo
        (hugo-run "server" "--watch" (~a "--source=" (send blog-folder get-path))
                  (~a "--destination=" (send public-folder get-path))
                  (send build-drafts? get-argument)
                  (send build-future? get-argument)
                  #:stdout #f
                  #:callback
                  (lambda (hp)
                    (send btn set-label "Start")
                    (stop-hook)))))]))

(define bottom-panel
  (new horizontal-panel%
       [parent frame]
       [alignment '(right center)]))

(define boolean-argument%
  (class check-box%
    (init-field label true-value false-value
                [default #f] [callback void])

    (inherit get-value set-value)

    (super-new
     [label label]
     [callback (lambda (c v)
                 (preferences:set key (get-value))
                 (callback c v))])

    (define/public (get-argument)
      (if (get-value) true-value false-value))

    (define key (string->symbol label))
    (preferences:set-default key default boolean?)
    (set-value (preferences:get key))))

(define build-drafts?
  (new boolean-argument%
       [parent bottom-panel]
       [label "Drafts?"]
       [true-value "-D"]
       [false-value ""]
       [default #t]))

(define build-future?
  (new boolean-argument%
       [parent bottom-panel]
       [label "Future?"]
       [true-value "-F"]
       [false-value ""]
       [default #f]))

(define start-button
  (new button%
       [label "Start"]
       [parent bottom-panel]
       [callback hugo-server-start/stop]))

(define open-button
  (new button%
       [label "Open"]
       [parent bottom-panel]
       [enabled #f]
       [callback (lambda (b e)
                   (send-url "http://127.0.0.1:1313/"))]))

(define (ensure-hugo-blog dir)
  (cond
    [(hugo-dir? dir) dir]
    [else
     (define answer
       (message-box/custom
        "Blog Selection"
        "Selected directory does not contain an Hugo blog. Would you like to create one?"
        "Yes" "No" #f
        frame                               ; parent
        '(caution disallow-close default=2) ; style
        ))
     (cond
       [(= answer 2) #f]
       [else
        (cond
          [(not (null? (directory-list dir)))
           (message-box "Blog Selection"
                        "Directory must be empty. Nothing was done..."
                        frame '(ok stop))])
        (define hp (hugo-run "new" "site" dir))
        (hugo-wait hp)
        (if (zero? (hugo-status hp)) dir #f)])]))

(module+ main
  (send frame show #t))
