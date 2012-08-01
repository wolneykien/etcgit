(define-module (ui etcgit restart ajax)
    :use-module (alterator ajax)
    :use-module (alterator session)
    :use-module (alterator woo)
    :export (init))

(define (do-reload)
  (session-delete (form-cookie "session"))
  (form-update-cookie "session" "")
  (catch/message
    (lambda ()
      (woo-write "/etcgit/head"))))

(define (init)
  (form-set-timeout do-reload 1))
