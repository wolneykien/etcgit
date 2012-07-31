(define-module (ui etcgit log ajax)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-19)
  :use-module (alterator ajax)
  :use-module (alterator plist)
  :use-module (alterator woo)
  :export (init))

(define *limit* 25)
(define *skip* 0)

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
      (let* ((branch (form-value "branch"))
	     (log (woo-list "/etcgit/commits" 'head branch
			                      'limit (+ *limit* 1)
					      'skip *skip*
					      'branch branch)))
	(if (> (length log) *limit*)
	    (begin
	      (form-update-enum "log"
				(map (lambda (row)
				       (format-row row format-commit))
				     (list-head log *limit*)))
	      #t)
	    (begin
	      (form-update-enum "log"
				(map (lambda (row)
				       (format-row row format-commit))
				     log))
	      #f))))))

(define (read-repo)
  (catch/message
    (lambda ()
      (let ((data (woo-read-first "/etcgit"))
            (branch (form-value "branch")))
        (form-update-value "url" (woo-get-option data 'url))
	(form-update-enum "branch"
			  (woo-list "/etcgit/branches" 'url (woo-get-option data 'url)))
	(form-update-value "branch" branch)))))

(define (init-log)
  (read-repo)
  (set! *skip* 0)
  (form-update-visibility "prev" #f)
  (form-update-visibility "next" (read-log)))

(define (init)
  (form-update-value "branch" (form-value "showbranch"))
  (form-bind "branch" "change" init-log)
  (form-bind "prev" "click"
    (lambda ()
      (if (> *skip* 0)
	  (set! *skip*
		(if (> *skip* *limit*)
		    (- *skip* *limit*)
		    0)))
      (form-update-visibility "prev" (> *skip* 0))
      (form-update-visibility "next" (read-log))))
  (form-bind "next" "click"
    (lambda ()
      (set! *skip* (+ *skip* *limit*))
      (form-update-visibility "prev" #t)
      (form-update-visibility "next" (read-log))))
  (init-log))