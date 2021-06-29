;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(define-condition integrity-error (error)
  ((message :initarg :message
            :reader message))
  (:documentation "Signals an attempt to do something that would violate the integrity model of restagraph and, by implication, the application built on it."))

(define-condition client-error (error)
  ((message :initarg :message
            :reader message))
  (:documentation "The client made an invalid request, e.g. required parameters were missing."))
