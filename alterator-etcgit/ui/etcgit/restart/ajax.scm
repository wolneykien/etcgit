(define-module (ui etcgit restart ajax)
    :use-module (alterator ajax)
    :use-module (alterator session)
    :use-module (alterator woo)
    :export (init))

(define (do-reload branch)
  (session-delete (form-cookie "session"))
  (form-update-cookie "session" "")
  (catch/message
    (lambda ()
      (woo-write "/etcgit" 'branch branch))))

(define (init)
  (form-set-timeout (lambda ()
                      (do-reload (form-value "branch")))
                    1))
