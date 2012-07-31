(define-module (ui etcgit ajax)
  :use-module (srfi srfi-1)
  :use-module (alterator ajax)
  :use-module (alterator plist)
  :use-module (alterator woo)
  :export (init))

(define (read-repo)
  (catch/message
    (lambda ()
      (let ((data (woo-read-first "/etcgit")))
        (form-update-value "url" (woo-get-option data 'url))
	(form-update-enum "branch"
			  (woo-list "/etcgit/branches" 'url (woo-get-option data 'url)))
	(form-update-value "branch" (woo-get-option data 'branch))))))

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

(define (format-file filename status)
  (list filename status 'class status))

(define (read-files)
  (catch/message
    (lambda ()
      (form-update-enum "files"
			(map (lambda (row)
			       (format-row row format-file))
			     (woo-list "/etcgit"))))))

(define (fetch-repo)
  (catch/message
    (lambda ()
      (woo-write "/etcgit"
		 'url (form-value "url"))
      (read-repo)
      (read-files))))

(define (reset-to)
  (catch/message
    (lambda ()
      (woo-write "/etcgit"
		 'branch (form-value "branch"))
      (read-files))))

(define (init)
  (form-bind "fetch" "click"
    (lambda ()
      (fetch-repo)))
  (form-bind "url" "enter" (lambda () #f))
  (form-bind "branch" "change"
    (lambda ()
      (reset-to)))
  (form-bind "history" "click"
    (lambda ()
      (form-replace "/etcgit/log"
		    'showbranch (form-value "branch"))))
  (read-repo)
  (read-files))