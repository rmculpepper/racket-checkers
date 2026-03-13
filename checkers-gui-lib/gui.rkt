;; Copyright 2026 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/class
         racket/contract/base
         racket/gui/base
         mrlib/hierlist
         framework
         framework/notify
         data/gvector
         checkers/private/result
         checkers/private/check
         checkers/private/test
         (submod "private/gui-util.rkt" prefs)
         (submod "private/gui-util.rkt" cache)
         (submod "private/gui-util.rkt" editor)
         (submod "private/gui-util.rkt" styles))
(provide (contract-out
          [test/gui
           (->* [procedure?] [#:wait? boolean?] void?)]))

(define (test/gui proc #:wait? [wait? #t])
  (define es (make-eventspace))
  (define controller (new controller% (es es)))
  (send controller call-in-eventspace
        (lambda () (make-view-frame controller)))
  (send controller run proc)
  (when wait? (void (sync es))))


;; ============================================================
;; Controller

(define controller%
  (class object%
    (init-field es) ;; eventspace
    (super-new)

    (define/public (get-eventspace) es)
    (define/public (call-in-eventspace proc #:sync? [sync? #f])
      (parameterize ((current-eventspace es))
        (queue-callback proc)))

    ;; ----------------------------------------
    ;; interface with tests

    ;; run : (-> Any) -> Void
    (define/public (run proc)
      (parameterize ((current-test-context null)
                     (current-test-listeners (list (get-listener))))
        (call-with-continuation-barrier proc))
      (void))

    (define/public (get-listener)
      (lambda (ctx event)
        (call-in-eventspace
         (lambda () (handle-test-event ctx event)))))

    ;; on-model-status-change : Model -> Void
    (define/public (on-model-status-change model)
      (views-update-model model))

    ;; ----------------------------------------
    ;; model management (eventspace thread only)

    ;; handle-test-event : TestContext TestEvent -> Void
    (define/private (handle-test-event ctx event)
      (match event
        ['start
         (match-define (cons frame parent-ctx) ctx)
         (define parent (and (pair? parent-ctx) (get-model parent-ctx)))
         (define model (make-model parent frame))
         (set-model! ctx model)
         (when parent (send parent add-child! model))
         (views-add-model model)]
        ['end
         (define model (get-model ctx))
         (send model finish! 'done)]
        [(? check-failure?)
         (define model (get-model ctx))
         (send model finish! event)]
        ;; --------------------
        [_ (void)]))

    ;; make-model : Model/#f TestFrame -> Model
    (define/private (make-model parent frame)
      (new test-model% (controller this) (parent parent) (frame frame)))

    (define frame=>model (make-hasheq))
    (define/private (get-model ctx)
      (or (hash-ref frame=>model (car ctx) #f)
          (error 'controller:get-model "none registered: ~e" (car ctx))))
    (define/private (set-model! ctx model)
      (hash-set! frame=>model (car ctx) model))

    ;; ----------------------------------------
    ;; view management

    ;; views : (Listof View)
    (define views null)

    ;; add-view : View -> Void
    (define/public (add-view view)
      (set! views (cons view views)))

    ;; remove-view : View -> Void
    (define/public (remove-view view)
      (set! views (remq view views)))

    ;; views-add-model : Model -> Void
    (define/private (views-add-model model)
      (for ([view (in-list views)])
        (send view add-model model)))

    ;; views-update-model : Model -> Void
    (define/private (views-update-model model)
      (for ([view (in-list views)])
        (send view update-model model)))

    ))


;; ============================================================
;; Model

(define test-model%
  (class object%
    (init-field controller
                parent
                frame)
    (super-new)

    ;; children : (GVectorof TestModel)
    (define children (make-gvector))

    ;; endstate : (U #f 'done CheckFailure)
    (define endstate #f)

    (define start-times (get-times-now))
    (define end-times #f)

    (define/public (get-times)
      (and end-times (map - end-times start-times)))

    (define/private (get-times-now) ;; real, cpu, gc
      (list (current-inexact-monotonic-milliseconds)
            (current-process-milliseconds)
            (current-gc-milliseconds)))

    ;; output : (Listof (cons (U 'output 'error) (Listof Bytes))) -- most recent first
    (define output null)

    (define/public (get-controller) controller)
    (define/public (get-parent) parent)
    (define/public (get-children) (gvector->list children))
    (define/public (get-failure-info)
      (match endstate [(check-failure info) info] [_ #f]))

    (define/public (get-name)
      (or (test-frame-short-name frame)
          "(unnamed test)"))

    (define/public (add-child! c)
      (gvector-add! children c))

    (define/public (finish! v)
      (set! end-times (get-times-now))
      (set! endstate v)
      (when parent (send parent on-child-status-change this))
      (send controller on-model-status-change this))

    ;; get-case-status : -> (U 'incomplete 'pass 'fail)
    ;; Status not considering children.
    (define/public (get-case-status)
      (match endstate
        [#f 'incomplete]
        ['done 'pass]
        [_ 'fail]))

    ;; get-full-status : -> (U 'fail 'fail-within 'incomplete 'incomplete-within 'pass)
    (define/public (get-full-status)
      (define case-status (get-case-status))
      (define-values (tests pass fail) (get-summary))
      (cond [(eq? case-status 'fail) 'fail]
            [(> fail 0) 'fail-within]
            [(eq? case-status 'incomplete) 'incomplete]
            [(> tests (+ pass fail)) 'incomplete-within]
            [else 'pass]))

    ;; on-child-status-change : TestModel -> Void
    (define/public (on-child-status-change child)
      (define result (cache-ref children-cache))
      (cache-invalidate! children-cache)
      (define new-result (cache-ref children-cache))
      (unless (equal? new-result result)
        (when parent (send parent on-child-status-change this))
        (send controller on-model-status-change this)))

    ;; get-summary : -> (values Nat Nat Nat)
    ;; Returns (values total pass fail) -- may not agree, if any incomplete
    (define/public (get-summary)
      (define-values (c-total c-pass c-fail)
        (get-children-summary))
      (define-values (this-pass this-fail)
        (case (get-case-status)
          [(pass) (values 1 0)]
          [(fail) (values 0 1)]
          [(incomplete) (values 0 0)]))
      (values (+ c-total 1)
              (+ c-pass this-pass)
              (+ c-fail this-fail)))

    (define/public (get-children-summary)
      (match (cache-ref children-cache)
        [(summary total pass fail)
         (values total pass fail)]))

    (define children-cache (cache (calculate-children-summary)))

    (define/private (calculate-children-summary)
      (define-values (total pass fail)
        (for/fold ([total 0] [pass 0] [fail 0])
                  ([child (in-gvector children)])
          (define-values (child-total child-pass child-fail)
            (send child get-summary))
          (values (+ total child-total) (+ pass child-pass) (+ fail child-fail))))
      (summary total pass fail))

    ))

;; Summary = (summary Nat Nat Nat)
(struct summary (total pass fail) #:prefab)


;; ============================================================
;; View

(define view%
  (class object%
    (init-field parent
                controller)
    (super-new)

    (define es (send (send parent get-top-level-window) get-eventspace))
    (define/private (call-in-eventspace proc)
      (parameterize ((current-eventspace es))
        (queue-callback proc)))

    (define hpane (new panel:horizontal-dragable% (parent parent)))
    (define tree-pane (new vertical-pane% (parent hpane)))
    (define tree-view
      (new model-tree-view% (parent tree-pane) (view this)))
    (define detail-pane (new vertical-pane% (parent hpane)))
    (define detail-text (new text:styled% (style-map basic-styles #;checkers-style-map)))
    (send detail-text lock #t)
    (define detail-canvas
      (new canvas:wide-snip% (parent detail-pane) (editor detail-text)))
    (with-handlers ([exn:fail? void])
      (send hpane set-percentages '(2/5 3/5)))

    (define renderer (new model-renderer% (view this)))

    ;; ----------------------------------------
    ;; from tree-view

    (define selected #f)

    (define/public (get-selected-model)
      selected)

    (define/public (set-selected-model model)
      (unless (eq? model selected)
        (set! selected model)
        (send tree-view set-selected-model model)
        (show-model model)))

    ;; ----------------------------------------
    ;; from controller

    ;; add-model : Model -> Void
    (define/public (add-model model)
      (add/update-model model))

    ;; update-model : Mode -> Void
    (define/public (update-model model)
      (add/update-model model))

    (define lock (make-semaphore 1))    ;; Semaphore
    (define updates null)               ;; (Listof Model)

    ;; add/update-model : Model -> Void
    (define/private (add/update-model model)
      (when (call-with-semaphore lock
              (lambda ()
                (begin0 (null? updates)
                  (unless (memq model updates)
                    (set! updates (cons model updates))))))
        (call-in-eventspace
         (lambda ()
           (let ([updates (call-with-semaphore lock
                            (lambda ()
                              (begin0 updates
                                (set! updates null))))])
             (perform-update-batch (reverse updates)))))))

    ;; perform-update-batch : (Listof Model) -> Void
    (define/private (perform-update-batch models)
      (send tree-view perform-update-batch models)
      (let ([selected (get-selected-model)])
        (when (and selected (memq selected models))
          (show-model selected))))

    ;; show-model : result<%> -> void
    ;; Displays the result in the Details area.
    ;; ** Must be called from eventspace thread.
    (define/private (show-model model)
      (send* detail-text
        (begin-edit-sequence)
        (lock #f)
        (erase))
      (send detail-text render (send renderer render-model/long model))
      (send* detail-text
        (lock #t)
        (end-edit-sequence)
        (scroll-to-position 0)))

    ;; shutdown : -> void
    ;; Notifies the controller that the view has hung up.
    (define/public (shutdown)
      (send controller remove-view this))
    ))

;; ----------------------------------------
;; Tree View

(define model-tree-view%
  (class hierarchical-list%
    (inherit get-editor
             get-items)
    (init-field view)
    (super-new (style '(auto-hscroll)))

    ;; ----------------------------------------
    ;; View Link

    (define model=>view-link (make-hash))
    (define/private (set-view-link model item)
      (hash-set! model=>view-link model item))
    (define/private (get-view-link model)
      (hash-ref model=>view-link model #f))

    ;; ----------------------------------------
    ;; Behavior

    (define/override (on-select item)
      (let [(model (send item user-data))]
        (send view set-selected-model model)))

    (define/override (on-double-select item)
      (when (is-a? item hierarchical-list-compound-item<%>)
        (if (send item is-open?) (send item close) (send item open))))

    (define/private (ensure-tree-visible model)
      (define parent (send model get-parent))
      (define parent-view-link (and parent (get-view-link parent)))
      (when (and parent (not (send parent-view-link is-open?)))
        (send parent-view-link open)
        (ensure-tree-visible parent)))

    ;; ----------------------------------------

    (define/public (perform-update-batch models)
      (send (get-editor) begin-edit-sequence #f)
      (for ([model (in-list models)])
        (define view-link
          (or (get-view-link model)
              (create-view-link model #f)))
        (update-item view-link))
      (send (get-editor) end-edit-sequence))

    (define/public (set-selected-model model)
      (let ([view-link (get-view-link model)])
        (send view-link select #t)))

    ;; ----------------------------------------
    ;; Construction

    ;; create-view-link : Model Boolean -> Item
    (define/public (create-view-link model compound?)
      (define parent (send model get-parent))
      (define parent-link
        (if parent (force-compound (get-view-link parent)) this))
      (define view-link
        (cond [compound? (send parent-link new-list)]
              [else (send parent-link new-item)]))
      (set-view-link model view-link)
      (send view-link user-data model)
      (let ([editor (send view-link get-editor)])
        (send editor insert (send model get-name)))
      (when (and compound? (not parent))
        (send view-link open))
      view-link)

    ;; force-compound : Item -> CompoundItem
    ;; FIXME: if item is not last (eg, concurrent tests), changes order
    (define/private (force-compound item)
      (define model (send item user-data))
      (cond [(is-a? item hierarchical-list-compound-item<%>)
             item]
            [else
             (define parent-item (send item get-parent))
             (send (or parent-item this) delete-item item)
             (define new-item (create-view-link model #t))
             (update-item new-item)
             new-item]))

    ;; update-item : Item -> Void
    (define/public (update-item view-link)
      (define editor (send view-link get-editor))
      (define model (send view-link user-data))
      (define status (send model get-full-status))
      (define styles (cons style:normal (get-status-styles status)))
      (send editor begin-edit-sequence #f)
      (for ([style (in-list styles)])
        (send editor change-style style 0 (send editor last-position) #f))
      (send editor end-edit-sequence))

    ;; get-status-styles : Symbol -> (Listof StyleDelta)
    (define/private (get-status-styles status)
      (case status
        [(pass) (list style:green)]
        [(fail) (list style:red style:bold)]
        [(fail-within) (list style:red)]
        [(incomplete) (list style:gray style:bold)]
        [(incomplete-within) (list style:orange)]))

    ))

;; ----------------------------------------
;; View frame

;; make-view-frame : Controller -> ViewFrame
(define (make-view-frame controller)
  (define frame (new view-frame% (label FRAME-LABEL) (controller controller)))
  (send frame show #t)
  frame)

(define FRAME-LABEL "Test Results")

(define view-frame%
  (class (frame:standard-menus-mixin
          (frame:basic-mixin frame%))
    (inherit get-help-menu
             get-width
             get-height
             get-menu-bar
             get-area-container)
    (init-field controller)
    (init [width (pref:width)]
          [height (pref:height)])
    (super-new (width width) (height height))

    (define view
      (new view%
           (controller controller)
           (parent (get-area-container))))
    (send controller add-view view)

    (define/augment (on-close)
      (pref:width (get-width))
      (pref:height (get-height))
      (send view shutdown)
      (inner (void) on-close))

    (define-syntax override-false
      (syntax-rules ()
        [(override-false name ...)
         (begin (define/override (name . _) #f) ...)]))

    (override-false file-menu:create-new?
                    file-menu:create-open?
                    file-menu:create-open-recent?
                    file-menu:create-revert?
                    file-menu:create-save?
                    file-menu:create-save-as?
                    file-menu:create-print?
                    edit-menu:create-undo?
                    edit-menu:create-redo?
                    edit-menu:create-cut?
                    edit-menu:create-paste?
                    edit-menu:create-clear?
                    edit-menu:create-find?
                    edit-menu:create-preferences?)
    (send (get-help-menu) delete)

    ))

;; ============================================================

(define top-align (make-object style-delta% 'change-alignment 'top))

(define model-renderer%
  (class object%
    (init-field view)
    (super-new)

    ;; ----------------------------------------
    ;; Long form

    (define/public (render-model/long model)
      (define-values (c-total c-pass c-fail) (send model get-children-summary))
      (define c-incomplete (- c-total c-pass c-fail))
      (define full-status (send model get-full-status))
      (define case-status (send model get-case-status))
      (define children (send model get-children))
      (div (span #:style '(large) (send model get-name))
           (span)
           (span "Test status: "
                 (case case-status
                   [(pass) (span #:style '(green) "pass")]
                   [(fail) (span #:style '(red) "fail")]
                   [(incomplete) (span #:style '(gray) "incomplete")]))
           (cond [(> c-total 0)
                  (span "Subtests:"
                        (span #:style '(green)
                              (format " ~s pass" c-pass))
                        (and (> c-fail 0)
                             (span ", " (span #:style '(red)
                                              (format "~s fail" c-fail))))
                        (and (> c-incomplete 0)
                             (span ", " (span #:style '(gray)
                                              (format "~s incomplete" c-incomplete)))))]
                 [else #f])
           (match (send model get-times)
             [(list real-ms cpu-ms gc-ms)
              (span (format "Time: ~s cpu ms; ~s real ms; ~s gc ms"
                            cpu-ms (inexact->exact (round real-ms)) gc-ms))]
             [#f #f])
           (case case-status
             [(fail) (list (span) (render-failure/long model))]
             [else #f])
           (cond [(> c-fail 0)
                  (list (span)
                        (div (span #:style '(bold) "Failing subtests")
                             (for/list ([c (in-list children)]
                                        #:when (eq? (send c get-case-status) 'fail))
                               (render-model/short c))))]
                 [else #f])
           (cond [(> c-incomplete 0)
                  (list (span)
                        (div (span #:style '(bold) "Incomplete subtests")
                             (for/list ([c (in-list children)]
                                        #:when (eq? (send c get-case-status) 'incomplete))
                               (render-model/short c))))]
                 [else #f])
           ))

    (define/public (render-failure/long model)
      (define rparts null)
      (define (printkv label mode value)
        (define part
          (div (span label ":")
               (case mode
                 [(display)
                  (div #:box? #t (format "~a" value))]
                 [(write)
                  (div #:box? #t (span #:style '(value) (format "~s" value)))]
                 [(value)
                  (div #:box? #t (span #:style '(value) (format "~e" value)))])))
        (set! rparts (cons part rparts)))
      (print-fault (send model get-failure-info) printkv)
      (div (span #:style '(bold) "Failure info")
           (reverse rparts)))

    ;; ----------------------------------------
    ;; Short form

    (define/public (render-model/short model)
      (define-values (c-total c-pass c-fail) (send model get-children-summary))
      (define c-incomplete (- c-total c-pass c-fail))
      (define full-status (send model get-full-status))
      (define case-status (send model get-case-status))
      (span (render-model-link model) " "
            (span #:style (case full-status
                            [(fail fail-within incomplete-within) '(red)]
                            [(incomplete) '(gray)]
                            [(pass) '(green)])
                  (case case-status
                    [(pass) "pass"]
                    [(fail) "fail"]
                    [(incomplete) "incomplete"])
                  (cond [(> c-fail 0) " with failing subtests"]
                        [(> c-total (+ c-pass c-fail)) " with incomplete subtests"]
                        [else ""]))))

    (define/private (render-model-link model)
      (span #:style '(clickback)
            #:clickback (lambda _ (send view set-selected-model model))
            (send model get-name)))

    ))
