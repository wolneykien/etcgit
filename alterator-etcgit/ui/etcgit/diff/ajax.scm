(define-module (ui etcgit diff ajax)
  :use-module (alterator ajax)
  :use-module (alterator woo)
  :export (init))

(define (read-diff)
  (catch/message
    (lambda ()
      (form-update-value-list '("diff")
			      (woo-read-first "/etcgit/diff"
					      'file (form-value "file"))))))

(define (init)
  (form-update-value "filename" (form-value "file"))
  (read-diff))