(define-module (ui etcgit repo ajax)
  :use-module (srfi srfi-1)
  :use-module (alterator ajax)
  :use-module (alterator plist)
  :use-module (alterator woo)
  :export (init))

(define (format-row row proc)
  (if (plist? row)
    (let* ((ns-vs (plist-fold
                    (lambda (n v ns-vs)
                      (cons
                        (append (car ns-vs) (list n))
                        (append (cdr ns-vs) (list v))))
                    (cons '() '())
                    row))
           (res (apply proc (cdr ns-vs)))
           (ns (car ns-vs))
           (vs (list-head res (length ns)))
           (aux (list-tail res (length vs))))
      (append
        (fold (lambda (n v rlist)
                (append rlist (list n v)))
              '()
              ns
              vs)
        aux))
    (cons (car row) (format-row (cdr row) proc))))

(define (format-branch name label remote_head local_head status)
  (append (list name label remote_head local_head)
          (case (string->symbol status)
            ((nw) (list (_ "remote only") 'class "A"))
            ((lo) (list (_ "local only")))
            ((ff) (list ( _ "behind") 'class "A"))
            ((fr) (list (_ "ahead") 'class "M"))
            ((eq) (list (_ "equal")))
            ((br) (list (_ "derived") 'class "M"))
            (else (list (_ "unknown") 'class "D")))))

(define (read-branches)
  (let ((url (form-value "url")))
    (if (and url (not (string-null? url)))
      (catch/message
        (lambda ()
          (form-update-enum "branches" (map (lambda (row)
                                              (format-row row format-branch))
                                            (woo-list "/etcgit/branches" 'url url))))))))

(define (read-branch)
  (catch/message
    (lambda ()
      (let ((data (woo-read-first "/etcgit/branch")))
        (form-update-value "url" (woo-get-option data 'url ""))))))

(define (init)
  (form-bind "fetch" "click" read-branches)
  (read-branch)
  (read-branches))
