;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Resource-related methods

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(defclass access-policy ()
  ((get-policy :initarg :get-policy
               :reader get-policy)
   (post-policy :initarg :post-policy
                :reader post-policy)
   (put-policy :initarg :put-policy
               :reader put-policy)
   (delete-policy :initarg :delete-policy
                  :reader delete-policy))
  (:documentation "Selector object, determining which access policies are applied"))

(defun make-access-policy (&key (get-policy :ALLOW-ALL)
                                (post-policy :ALLOW-ALL)
                                (put-policy :ALLOW-ALL)
                                (delete-policy :ALLOW-ALL))
  "Constructor function for access-policy instances."
  (declare (type keyword get-policy post-policy put-policy delete-policy))
  (let ((vals '(:DENY
                 :ALLOW-ALL
                 :ALLOW-AUTHENTICATED
                 :ALLOW-ADMIN)))
    (mapcar #'(lambda (arg)
                (unless (member arg vals)
                  (error (format nil "All args must be one of ~{~A~^, ~}" vals))))
            (list get-policy post-policy put-policy delete-policy))
    (make-instance 'access-policy
                   :get-policy get-policy
                   :post-policy post-policy
                   :put-policy put-policy
                   :delete-policy delete-policy)))

(defun define-policy (policyname)
  "Create a predefined policy object."
  (declare (type string policyname))
  (cond
    ;; Everything fully open.
    ;; Max convenience, but only do this on your personal computer.
    ((equal "open" policyname) (make-access-policy))
    ;; readonly - good for allowing access while you investigate who broke something.
    ((equal "readonly" policyname)
     (make-access-policy :get-policy :ALLOW-ALL
                         :post-policy :DENY
                         :put-policy :DENY
                         :delete-policy :DENY))
    ;; Fallback: error out.
    (t (error "No such policy."))))


(defun get-creator (policy)
  "Return the People UID to use as the owner, for any changes.
   Dispatches on individual policies, e.g. :deny or :allow-all, not on an 'access-policy instance"
  (declare (type keyword policy))
  (cond
    ((equal :deny policy) nil)
    ((member policy '(:allow-all)) "RgAdmin")
    (t (error "Not implemented"))))
