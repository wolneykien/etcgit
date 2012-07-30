(define-module (ui etcgit log ajax)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-19)
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

(define (format-commit head timestamp msg)
  (list head
	(date->string
	  (time-utc->date
	    (date->time-utc
	      (string->date timestamp "~Y-~m-~d ~H:~M:~S ~z")))
	  "~d.~m.~Y ~H:~M:~S")
	msg))

(define (read-log)
  (catch/message
    (lambda ()
      (form-update-enum "log"
			(map (lambda (row)
			       (format-row row format-commit))
			     (woo-list "/etcgit/commits" 'head (form-value "branch")
							 'limit 100
							 'branch (form-value "branch")))))))

(define (read-repo)
  (catch/message
    (lambda ()
      (let ((data (woo-read-first "/etcgit"))
            (branch (form-value "branch")))
        (form-update-value "url" (woo-get-option data 'url))
	(form-update-enum "branch"
			  (woo-list "/etcgit/branches" 'url (woo-get-option data 'url)))
	(form-update-value "branch" branch)))))

(define (init)
  (form-bind "branch" "change"
    (lambda ()
      (form-update-value "branch" (car (string-split (form-value "branch") #\;)))
      (read-log)))
  (read-repo)
  (read-log))