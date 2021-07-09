;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Logging infrastructure
;;; We only need something extremely simple

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


;; Set the threshold logging level
(defparameter *loglevel* :debug)

;; Wrap log-message in a closure containing the log-levels
(let ((loglevels '(:crit 4
                   :critical 4
                   :error 3
                   :warn 2
                   :warning 2
                   :info 1
                   :debug 0)))
  (defun log-message (severity message)
    (declare (type keyword severity)
             (type string message))
    ;; Filter on severity
    (when (>= (getf loglevels severity) (getf loglevels *loglevel*))
      (format cl:*standard-output* "~%~A ~A ~A"
              ;; Generate the timestamp
              (multiple-value-bind (sec minute hour date month year)
                (get-decoded-time)
                (format nil "[~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d]"
                        year month date hour minute sec))
              severity
              message))))
