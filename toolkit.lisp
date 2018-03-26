#|
 This file is a part of Inkwell
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.inkwell)

(defun -> (result &rest tree)
  (if (and result tree)
      (let ((key (car tree)))
        (apply #'->
               (etypecase key
                 (integer (nth key result))
                 (string (gethash key result)))
               (cdr tree)))
      result))

(defmacro define-unreadable-printer (class format-string &rest format-args)
  `(defmethod print-object ((,class ,class) stream)
     (print-unreadable-object (,class stream :type T)
       (format stream ,format-string ,@format-args))))

(defmacro with-initargs (instance results &body initargs)
  (let ((result (gensym "RESULT"))
        (name (gensym "NAME")))
    `(let ((,result ,results))
       (flet ((=> (&rest ,name)
                (apply #'-> ,result ,name)))
         (reinitialize-instance ,instance ,@initargs)))))

(defgeneric into (type object))

(defmethod into (type (list list))
  (loop for object in list
        collect (into type object)))

(defmacro define-converter (class &body body)
  (let ((object (gensym "OBJECT")))
    `(progn
       (defmethod into ((,(gensym "CLASS") (eql ',class)) (,object hash-table))
         (into (make-instance ',class) ,object))

       (defmethod into ((,class ,class) (,object hash-table))
         (with-initargs ,class ,object
           ,@body)))))

(defmacro define-class (name superclasses slots &rest options)
  `(defclass ,name ,superclasses
     ,(loop for slot in slots
            collect (if (listp slot)
                        slot
                        (list slot :initform NIL :initarg (intern (string slot) "KEYWORD") :reader slot)))
     ,@options))

(defun ->keyword (string)
  (intern (string string) "KEYWORD"))

(defun ->date (timestamp)
  (local-time:unix-to-timestamp timestamp))

(defun ->mode (string)
  (cond ((string= string "regular") :regular)
        ((string= string "gachi") :ranked)
        ((string= string "league") :league)
        (T :unknown)))

(defun ->rule (string)
  (cond ((string= string "turf_war") :turf-war)
        ((string= string "splat_zones") :splat-zones)
        ((string= string "tower_control") :tower-control)
        ((string= string "clam_blitz") :clam-blitz)
        (T :unknown)))

(defun fmttime (date &key (format :full))
  (let ((format (ecase format
                  (:full '((:year 4) "." (:month 2) "." (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))
                  (:hour '((:year 4) "." (:month 2) "." (:day 2) " " (:hour 2) ":" (:min 2)))
                  (:date '((:year 4) "." (:month 2) "." (:day 2)))
                  (:time '((:year 4) "." (:month 2) "." (:day 2))))))
    (local-time:format-timestring NIL date :format format)))

(defun ->weapon-name (string)
  (case (parse-integer string)
    (0 "Hero Shot")
    (1 "Hero Roller")
    (2 "Hero Charger")
    (3 "Hero Dualies")
    (4 "Hero Brella")
    (5 "Hero Splatling")
    (6 "Hero Blaster")
    (7 "Hero Slosher")
    (8 "Herobrush")))

(defun mkhash (&rest kvs)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k table) v))
    table))

(defun ensure-id (type id-ish)
  (cond ((typep id-ish type)
         (id id-ish))
        ((typep id-ish 'integer)
         id-ish)
        ((typep id-ish 'string)
         id-ish)
        (T (error "~s is not a ~a." id-ish type))))
