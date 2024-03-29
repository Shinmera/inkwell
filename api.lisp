(in-package #:org.shirakumo.inkwell)

(defparameter *base-url* "https://app.splatoon2.nintendo.net")
(defvar *session*)

(define-condition api-request-failed (error)
  ((url :initarg :url :reader url)
   (code :initarg :code :reader code)
   (body :initarg :body :reader body))
  (:report (lambda (c s) (format s "API request to ~s return code ~a~@[:~%  ~a~]"
                                 (url c) (code c) (body c)))))

(defun %request (url parameters form-data session)
  (let ((drakma:*text-content-types* `(("application" . "json"))))
    (drakma:http-request url
                         :accept "application/json;charset=UTF-8"
                         :additional-headers `(("Accept-Charset" . "UTF-8")
                                               ("Cookie" . ,(format NIL "iksm_session=~a" session)))
                         :external-format-in :utf-8
                         :external-format-out :utf-8
                         :want-stream T
                         :decode-content T
                         :form-data form-data
                         :parameters parameters)))

(defun request (endpoint &key urlparts parameters form-data (session *session*) string)
  (let ((url (format NIL "~a/api~a~{/~a~}" *base-url* endpoint urlparts)))
    (multiple-value-bind (stream code) (%request url parameters form-data session)
      (case code
        (200
         (if string
             (alexandria:read-stream-content-into-string stream)
             (yason:parse stream :json-nulls-as-keyword NIL)))
        (T
         (error 'api-request-failed :url url :code code :body (alexandria:read-stream-content-into-string stream)))))))

(defun list-battles ()
  (into 'battle (-> (request "/results") "results")))

(defun battle (battle)
  (let ((id (ensure-id 'battle battle)))
    (into 'battle (request "/results" :urlparts (list id)))))

(defun single-player ()
  (into 'single-player (request "/records/hero")))

(defun schedules ()
  (let ((data (request "/schedules")))
    (list :regular (into 'schedule (gethash "regular" data))
          :ranked (into 'schedule (gethash "gachi" data))
          :league (into 'schedule (gethash "league" data)))))

(defun list-stages ()
  (into 'stage (-> (request "/data/stages") "stages")))

(defun timeline ()
  (into 'timeline (request "/timeline")))

(defun user (user)
  (let ((id (ensure-id 'user user)))
    (into 'user (-> (request "/nickname_and_icon" :parameters `(("id" . ,id))) "nickname_and_icons" 0))))

(defun active-festivals ()
  (into 'festival (-> (request "/festivals/active") "festivals")))

(defun list-festivals ()
  (let* ((data (request "/festivals/pasts"))
         (results (-> data "results")))
    (into 'festival (loop for festival in (-> data "festivals")
                          do (setf (gethash "result" festival)
                                   (find (gethash "festival_id" festival) results
                                         :key (lambda (result) (gethash "festival_id" result))))
                          collect festival))))

(defun votes (festival)
  (let* ((id (ensure-id 'festival festival))
         (data (request "/festivals" :urlparts (list id "votes"))))
    (list (dolist (id (-> data "votes" "alpha"))
            (into 'user (find id (-> data "nickname_and_icons")
                              :key (lambda (user) (gethash "nsa_id" user)) :test #'string=)))
          (dolist (id (-> data "votes" "bravo"))
            (into 'user (find id (-> data "nickname_and_icons")
                              :key (lambda (user) (gethash "nsa_id" user)) :test #'string=))))))

(defun rankings (festival)
  (let* ((id (ensure-id 'festival festival))
         (data (request "/festivals" :urlparts (list id "rankings"))))
    (list (into 'ranking (-> data "rankings" "alpha"))
          (into 'ranking (-> data "rankings" "bravo")))))

(defun shop-info ()
  (let ((data (request "/onlineshop/merchandises")))
    (values (into 'merchandise (-> data "merchandises"))
            (into 'merchandise (-> data "ordered_info")))))

(defun order (merchandise &key override)
  (let ((id (ensure-id 'merchandise merchandise)))
    (into 'merchandise (-> (request "/onlineshop/order" :urlparts (list id)
                                                        :form-data (when override `(("name" . "override"))))
                           "ordered_info"))))
