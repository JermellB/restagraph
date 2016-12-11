(in-package #:restagraph)

(define-condition integrity-error (error)
  ((message :initarg :message
            :reader message))
  (:documentation "Signals an attempt to do something that would violate the integrity model of restagraph and, by implication, the application built on it."))
