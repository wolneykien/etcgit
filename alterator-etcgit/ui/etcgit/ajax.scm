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
  (form-confirm
    (let ((srvs (start-list)))
      (if (not (null? srvs))
        (format #f (_ "Are you sure you want to reset the state of the configuration files to that of the <code><b>~a</b></code> profile?<br /><br /><b>The following services will be restarted:</b><br /><br /><code>~a</code>")
                (form-value "branch")
                (string-join srvs " "))
        (format #f (_ "Are you sure you want to reset the state of the configuration files to that of the <code><b>~a</b></code> profile?")
                (form-value "branch"))))
    (_ "Configuration reset")))

(define (ask-commit new)
  (form-update-value "commit-new" new)
  (form-update-activity "commit-branch" new)
  (form-update-value "commit-message" "")
  (form-update-value "commit-branch" "")
  (let* ((res (js-result 'askCommit (_ "Commit changes") (_ "OK") (_ "Cancel")))
         (retcode (and (assoc 'retcode res) (car (assoc 'retcode res))))
         (msg (and (assoc 'msg res) (car (assoc 'msg res))))
         (branch (and (assoc 'branch res) (car (assoc 'branch res)) (assoc 'branchName res) (car (assoc 'branchName res)))))
    (and retcode (list msg branch))))

(define (commit msg branch)
  (if (not msg)
    (form-error (_ "Please, specify a comment for the commit"))
    (begin
      (catch/message
        (lambda ()
          (if branch
              (woo-write "/etcgit/head" 'commit #t 'msg msg 'branch branch)
              (woo-write "/etcgit/head" 'commit #t 'msg msg))))
      (read-repo)
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
  (form-bind "reset-to" "click"
    (lambda ()
      (if (ask-reload-head)
          (reload-head))))
  (form-bind "update" "click"
    (lambda ()
      (let ((ret (ask-commit #f)))
        (if ret
            (apply commit ret)))))
  (form-bind "new" "click"
    (lambda ()
      (let ((ret (ask-commit #t)))
        (if ret
            (apply commit ret)))))
  (read-repo)
  (read-files))
