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

(define (format-branch name label remote_head local_head status pstatus)
  (append 
    (list name label remote_head local_head
        (case (string->symbol status)
          ((nw) (_ "remote only"))
          ((lo) (_ "local only"))
          ((ff) ( _ "remote outdated"))
          ((fr) (_ "local outdated"))
          ((eq) (_ "equal"))
          ((br) (_ "derived"))
          (else (_ "unknown")))
        (case (string->symbol pstatus)
          ((eq) (_ "published"))
          ((ff) (_ "outdated"))
          ((fr) (_ "derived"))
          ((br) (_ "derived"))
          ((nw) (_ "public only"))
          ((lo) (_ "unpublished"))
          (else (_ "unpublished"))))
    (case (string->symbol status)
      ((nw) (list 'class "A"))
      ((fr) (list 'class "A"))
      ((ff) (list 'class "M"))
      ((br) (list 'class "M"))
      ((lo) (list 'class "D"))
      ((un) (list 'class "D"))
      (else '()))))

(define (do-read-branches url)
    (catch/message
      (lambda ()
        (let* ((branches (woo-list "/etcgit/branches" 'url url))
               (remote-branches (filter (lambda (row)
                                          (let ((remote-head (plistq 'remote_head (if (plist? row) row (cdr row)))))
                                            (and remote-head
                                                 (not (string-null? (cdr remote-head)))
                                                 (not (equal? "0" (cdr remote-head))))))
                                        branches))
               (has-remotes (not (null? remote-branches))))
          (form-update-enum "branches" (map (lambda (row)
                                              (format-row row format-branch))
                                            branches))
          (form-update-activity "update-all" has-remotes)
          (form-update-activity "update-selected" has-remotes)
          (form-update-value "fetch-result" (if has-remotes
                                              (_ "Successfully fetched the profile list")
                                              (_ "No profiles are published at the given URL")))
          (if (and url (not (string-null? url)) (not has-remotes))
            (form-warning (_ "Unable to fetch profiles from the given URL")))
          (form-bind "branches" "change" update-buttons)))))

(define (read-branches)
  (do-read-branches (form-value "url")))

(define (read-publication)
  (catch/message
    (lambda ()
      (let ((publication-data (woo-read-first "/etcgit/publication")))
        (form-update-value "publication-status" (if (woo-get-option publication-data 'status) "on" "off"))
        (form-update-visibility "publication-info" (woo-get-option publication-data 'status))
        (form-update-value "publicurl" (woo-get-option publication-data 'url))))))

(define (read-repo)
  (catch/message
    (lambda ()
      (let ((branch-data (woo-read-first "/etcgit/branch")))
        (form-update-value "url" (woo-get-option branch-data 'url "")))))
  (read-publication))

(define (selected-branches)
  (let ((branches (form-value "branches")))
    (if (and branches (not (string-null? branches)))
      (string-split branches #\;)
      '())))


(define (update-buttons)
  (let ((selected? (not (null? (selected-branches)))))
    (form-update-activity "update-selected" selected?)
    (form-update-activity "delete-selected" selected?)))

(define (write-publication-status)
  (catch/message
    (lambda ()
      (woo-write "/etcgit/publication" 'status (form-value "publication-status"))))
  (read-publication)
  (read-branches))

(define (fetch-branch branch)
  (catch/message
    (lambda ()
      (woo-write "/etcgit/branch" 'branch branch 'url (form-value "url")))))

(define (fetch-all-branches)
  (catch/message
    (lambda () 
      (for-each (lambda (row)
                  (let* ((row (if (plist? row) row (cdr row)))
                         (name (cdr (plistq 'name row)))
                         (status (cdr (plistq 'status row))))
                    (if (or (equal? status "nw")
                            (equal? status "fr"))
                      (fetch-branch name))))
                (woo-list "/etcgit/branches" 'url (form-value "url")))))
  (read-branches))

(define (fetch-selected-branches)
  (for-each fetch-branch (selected-branches))
  (read-branches))

(define (delete-branch branch)
  (catch/message
    (lambda ()
      (woo-write "/etcgit/branch" 'branch branch 'url ""))))

(define (delete-selected-branches)
  (for-each delete-branch (selected-branches))
  (read-branches))

(define (init)
  (form-bind "fetch" "click" read-branches)
  (form-bind "url" "enter" read-branches)
  (form-bind "publication-status" "change" write-publication-status)
  (form-bind "update-all" "click" fetch-all-branches)
  (form-bind "update-selected" "click" fetch-selected-branches)
  (form-bind "delete-selected" "click" delete-selected-branches)
  (read-repo)
  (do-read-branches #f)
  (update-buttons))
