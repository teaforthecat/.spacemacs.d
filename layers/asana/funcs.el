(require 'oauth2)
(require 'json)

(defvar asana-personal-access-token
  (make-oauth2-token
    :access-token
   "0/ceac1a5b5bbf2eb521873f0a5288413e"))

(defvar asana-api-url "https://app.asana.com/api/1.0/")

(defun asana-me () (concat asana-api-url "users/me"))
(defun asana-url (path &optional query)
  (concat asana-api-url
          path
          (if query
              (concat "?"
                      ;; here we rebuild the list of query parameters to ensure
                      ;; that all values are lists, as url-build-query-string
                      ;; insists upon it
                      (url-build-query-string (map 'list (lambda (q) (cons (car q)
                                                                     (list (cdr q))))
                                                   query))))))

;; test
;; (asana-url "projects" '((workspace . 123)))
;; (asana-url "projects")

(defun asana-get (path &optional query)
  (with-current-buffer
      (oauth2-url-retrieve-synchronously asana-personal-access-token
                                         (asana-url path query)
                                         "GET")
    (goto-char url-http-end-of-headers)
    (json-read)))

(defun asana-post (path data)
  (with-current-buffer
      (oauth2-url-retrieve-synchronously asana-personal-access-token (asana-me))
    (goto-char url-http-end-of-headers)
    (json-read)))


(defun asana-data-get (response &optional thing)
  (let ((data (assoc 'data response)))
    (map 'list 'ht<-alist (cdr (if thing (assoc thing data) data)))))


(defun asana-extract-workspace-id (workspaces name)
  (gethash 'id
           (seq-find (lambda (w) (equal name (gethash 'name w)))
                     workspaces)))

;; test
;; (asana-extract-workspace-id
;;  (map 'list 'ht<-alist
;;       [((id . 2844457889638) (name . "Your Company's Name"))
;;        ((id . 498346170860) (name . "Personal Projects"))])
;;  "Your Company's Name")
;; => 2844457889638


(defun asana-choose-workspace-id ()
  (let* ((response (asana-get "workspaces"))
         (workspaces (asana-data-get response))
         (workspace-name (completing-read "Choose Workspace"
                                          (map 'list (lambda (w)
                                                       (gethash 'name w))
                                               workspaces)
                                          )))
    (asana-extract-workspace-id workspaces
                                workspace-name)))

;; test
;; interactive
;; (asana-choose-workspace-id)

(defun asana-list-projects (workspace-id)
  (interactive "P")
  (let ((workspace-id (or workspace-id (asana-choose-workspace-id)))
        (projects (asana-data-get (asana-get "projects" `((workspace . ,workspace-id))))))
    (map 'list (lambda (p) (gethash 'name p))
         projects)))


;; test
;; (asana-list-projects 498346170860)
;; => ("Bones" "Data center design" "Reference" "Meta")

;; (asana-list-projects)

(defvar user-data
  (with-current-buffer
      (oauth2-url-retrieve-synchronously asana-personal-access-token (asana-me))
    (goto-char url-http-end-of-headers)
    (json-read)))

(cdr (assoc 'name (assoc 'data user-data)))

user-data
((data (id . 2844569582960) (email . "teaforthecat@gmail.com") (name . "Chris") (photo) (workspaces . [((id . 2844457889638) (name . "Your Company's Name")) ((id . 101572903613205) (name . "govdelivery.com")) ((id . 498346170860) (name . "Personal Projects"))])))
