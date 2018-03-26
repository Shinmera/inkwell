#|
 This file is a part of Inkwell
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.inkwell)

(defparameter *base-url* "https://app.splatoon2.nintendo.net/api")
(defvar *iksm-session*)

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

(defun request (endpoint &key urlparts parameters form-data (session *iksm-session*) string)
  (let ((url (format NIL "~a~a~{/~a~}" *base-url* endpoint urlparts)))
    (multiple-value-bind (stream code) (%request url parameters form-data session)
      (case code
        (200
         (if string
             (alexandria:read-stream-content-into-string stream)
             (yason:parse stream :json-nulls-as-keyword NIL)))
        (T
         (error 'api-request-failed :url url :code code :body (alexandria:read-stream-content-into-string stream)))))))

(defun results ()
  (into 'result (-> (request "/results") "results")))

(defun result (battle-id)
  (into 'result (request "/results" :urlparts (list battle-id))))

(defun single-player ()
  (into 'single-player (request "/records/hero")))

(defun schedules ()
  (into 'schedule (request "/schedules")))

(defun stages ()
  (into 'stage (-> (request "/data/stages") "stages")))

(defun timeline ()
  (into 'timeline (request "/timeline")))

(defun nick-and-icon (user-id)
  (let ((data (-> (request "/nickname_and_icon" :parameters `(("id" . ,user-id))) "nickname_and_icons" 0)))
    (list :nickname (-> data "nickname")
          :nsa-id (-> data "nsa_id")
          :thumbnail (-> data "thumbnail_url"))))

(defun active-festivals ()
  (into 'festival (-> (request "/festivals/active") "festivals")))

(defun festivals ()
  (let* ((data (request "/festivals/pasts"))
         (results (-> data "results")))
    (into 'festival (loop for festival in (-> data "festivals")
                          do (setf (gethash "result" festival)
                                   (find (gethash "festival_id" festival) results
                                         :key (lambda (result) (gethash "festival_id" result))))
                          collect festival))))

;; FIXME
(defun votes (festival-id)
  (request "/festivals/" :urlparts (list festival-id "votes")))

(defun rankings (festival-id)
  (request "/festivals/" :urlparts (list festival-id "rankings")))

(defun merchandise ()
  (let ((data (request "/onlineshop/merchandises")))
    (values (into 'merchandise (-> data "merchandises"))
            (into 'merchandise (-> data "ordered_info")))))

(defun order (item-id &key override)
  (into 'merchandise (-> (request "/onlineshop/order" :urlparts (list item-id)
                                                      :form-data (when override `(("name" . "override"))))
                         "ordered_info")))
