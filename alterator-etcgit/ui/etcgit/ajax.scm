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
      (apply proc
	     (plist-fold
	       (lambda (n v vs)
		 (append vs (list v)))
	       '()
	       row))
      (cons (car row) (format-row (cdr row) proc))))

(define (format-file filename status)
  (list 'filename filename 'status status 'class status))

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

(define (start-list)
  (catch/message
    (lambda ()
      (map (lambda (row)
	     (cdr (format-row row (lambda (n l) n))))
	   (woo-list "/etcgit/start")))))

(define (reload-head)
  (catch/message
    (lambda ()
      (woo-write "/etcgit/head")))
  (read-files))

(define (ask-reload-head)
  (if (form-confirm
        (let ((srvs (start-list)))
	  (if (not (null? srvs))
	      (format #f (_ "Are you sure you want to reset the state of the configuration files to that of the <code><b>~a</b></code> profile?<br /><br /><b>The following services will be restarted:</b><br /><br /><code>~a</code>")
		      (form-value "branch")
		      (string-join srvs " "))
	      (format #f (_ "Are you sure you want to reset the state of the configuration files to that of the <code><b>~a</b></code> profile?")
		      (form-value "branch"))))
	(_ "Configuration reset"))
      (reload-head)))

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
  (form-bind "reset-to" "click"
    (lambda ()
      (ask-reload-head)))
  (form-bind "update" "click"
    (lambda ()
      (form-update-value "commit-new" #f)
      (form-update-value "commit-message" "")
      (form-update-value "commit-branch" "")
      (form-update-activity "commit-branch" #f)
      (let* ((res (js-result 'askCommit (_ "Commit changes") (_ "OK") (_ "Cancel")))
             (retcode (assoc 'retcode res))
             (msg (assoc 'msg res)))
        (if (and retcode (cdr retcode))
            (form-warning "Result: ~s" res)))))
  (form-bind "new" "click"
    (lambda ()
      (form-update-value "commit-new" #t)
      (form-update-value "commit-message" "")
      (form-update-value "commit-branch" "")
      (form-update-activity "commit-branch" #t)
      (let* ((res (js-result 'askCommit (_ "Commit changes") (_ "OK") (_ "Cancel")))
             (retcode (assoc 'retcode res))
             (msg (assoc 'msg res)))
        (if (and retcode (cdr retcode))
            (form-warning "Result: ~s" res)))))
  (read-repo)
  (read-files))
