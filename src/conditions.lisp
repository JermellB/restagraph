(in-package #:restagraph)

(define-condition integrity-error (error)
  ((message :initarg :message
            :reader message))
  (:documentation "Signals an attempt to do something that would violate the integrity model of restagraph and, by implication, the application built on it."))

(define-condition client-error (error)
  ((message :initarg :message
            :reader message))
  (:documentation "The client made an invalid request, e.g. required parameters were missing."))
