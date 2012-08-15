(define-module (ui etcgit diff ajax)
  :use-module (alterator ajax)
  :use-module (alterator woo)
  :export (init))

(define (read-diff branch filename)
  (catch/message
    (lambda ()
      (form-update-value-list '("diff")
                              (woo-read-first "/etcgit/diff" 'branch branch
                                                             'file filename))
      (js 'sh_highlightDocument))))

(define (init)
  (let* ((file (string-split (form-value "file") #\:))
         (branch (car file))
         (filename (string-join (cdr file) ":")))
    (form-update-value "branch" branch)
    (form-update-value "filename" filename)
    (read-diff branch filename)))
