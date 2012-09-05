(define-module (ui etcgit ajax)
  :use-module (srfi srfi-1)
  :use-module (alterator ajax)
  :use-module (alterator plist)
  :use-module (alterator woo)
  :export (init))

(define (format-row row proc)
  (if (plist? row)
    (apply proc
           (plist-fold
             (lambda (n v vs)
               (append vs (list v)))
             '()
             row))
    (cons (car row) (format-row (cdr row) proc))))

(define (format-file branch filename status)
  (list 'filename filename 
        'status status
        'branchpath (string-append branch ":" filename)
        'class status))

(define (format-branch current name label)
  (list 'name name
        'label (or (and (equal? name current)
                        (string-append "* " label))
                   label)))

(define (read-repo)
  (catch/message
    (lambda ()
      (let* ((data (woo-read-first "/etcgit"))
             (branch (woo-get-option data 'branch))
             (modified (woo-get-option data 'modified #f))
             (branches (woo-list "/etcgit/branches"))
             (first-branch (if (not (null? branches))
                             (car (list-head branches 1))
                             '()))
             (first-branch-name (and (not (null? first-branch))
                                     (cdr (plistq 'name (if (plist? first-branch)
                                                          first-branch
                                                          (cdr first-branch)))))))
        (form-update-enum "branch"
                          (map (lambda (row)
                                 (format-row row (lambda (name label . other)
                                                   (format-branch branch name label))))
                               branches))
        (form-update-value "branch" (or branch first-branch-name))
        (js 'updateProfileName (or branch "--") modified)))))

(define (read-files)
  (catch/message
    (lambda ()
      (let* ((files (map (lambda (row)
                           (format-row row (lambda (filename status)
                                             (format-file (form-value "branch") filename status))))
                         (woo-list "/etcgit" 'branch (form-value "branch"))))
             (modified (not (null? files)))
             (repo-data (woo-read-first "/etcgit"))
             (current (woo-get-option repo-data 'branch))
             (local-branches (woo-list "/etcgit/branches"))
             (branch (form-value "branch")))
        (form-update-enum "files" files)
        (form-update-activity "reset-to" (or modified (and (not current) (not (null? local-branches)))))
        (form-update-activity "update" (and branch (not (string-null? branch)) modified))
        (form-update-activity "new" modified)
        (form-update-activity "history" (and branch (not (string-null? branch))))))))

(define (start-list)
  (catch/message
    (lambda ()
      (map (lambda (row)
             (cdr (format-row row (lambda (n l) n))))
           (woo-list "/etcgit/start" 'branch (form-value "branch"))))))

(define (reload-head)
  (let ((srvs (start-list)))
    (if (member "ahttpd" srvs)
      (begin
        (form-warning (_ "The System management center service will be restarted. The current session will be closed now."))
        (form-replace "/etcgit/restart" 'branch (form-value "branch")))
      (do-reload))))

(define (do-reload)
  (catch/message
    (lambda ()
      (woo-write "/etcgit" 'branch (form-value "branch"))))
  (read-repo)
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
  (form-update-value "commit-branch"
                     (if new
                       (string-append (form-value "branch") "1")
                       ""))
  (let* ((res (js-result 'askCommit (_ "Commit changes") (_ "OK") (_ "Cancel")))
         (retcode (and (assoc 'retcode res) (cdr (assoc 'retcode res))))
         (msg (and (assoc 'msg res) (cdr (assoc 'msg res))))
         (branch (and (assoc 'branch res) (cdr (assoc 'branch res)) (assoc 'branchName res) (cdr (assoc 'branchName res)))))
    (and retcode (list msg branch))))

(define (commit msg branch)
  (if (or (not msg) (string-null? msg))
    (form-error (_ "Please, specify a comment for the commit"))
    (if (and branch (string-null? branch))
      (form-error (_ "Profile name should not be empty"))
      (begin
        (catch/message
          (lambda ()
            (if branch
                (woo-write "/etcgit/head" 'commit #t 'msg msg 'branch branch)
                (woo-write "/etcgit/head" 'commit #t 'msg msg 'branch (form-value "branch")))))
        (read-repo)
        (read-files)))))

(define (init)
  (form-bind "branch" "change"
    (lambda ()
      (read-files)))
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
  (let ((branch (form-value "showbranch")))
    (if (and branch (not (string-null? branch)))
      (form-update-value "branch" branch)))
  (read-files))
