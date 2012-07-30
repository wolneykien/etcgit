(define-module (ui etcgit ajax)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-19)
  :use-module (ice-9 format)
  :use-module (alterator ajax)
  :use-module (alterator algo)
  :use-module (alterator plist)
  :use-module (alterator woo)
  :use-module (alterator gettext)
  :export (init))

(define _ (lambda (msg) msg))

(define (update-gettext)
  (let ((translate (make-translator "alterator-etcgit" (string-split (form-value "language") #\;))))
    (set! _
          (lambda (msg)
	    (translate msg)))))

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
    (apply fold
           (append
             (list (lambda (n v rlist)
		     (append rlist (list n v)))
		   '())
	     ((lambda (ns-vs)
	        (list (car ns-vs)
		      (apply proc (cdr ns-vs))))
	        (plist-fold
	          (lambda (n v ns-vs)
	            (cons
	              (append (car ns-vs) (list n))
	              (append (cdr ns-vs) (list v))))
	          (cons '() '())
	          row))))
    (cons (car row) (format-row (cdr row) proc))))

(define (format-file filename status)
  (list filename
	(case (string->symbol status)
	  ((M) (_ "modified"))
	  ((A) (_ "new"))
	  ((D) (_ "deleted"))
	  (else status))))

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
      (read-repo))))

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
  (form-bind "branch" "change"
    (lambda ()
      (reset-to)))
  (update-gettext)
  (read-repo)
  (read-files))