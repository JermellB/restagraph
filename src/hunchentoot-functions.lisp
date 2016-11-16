;;;; Functions exposed by the REST API server application

(in-package #:restagraph)

(defun four-oh-four ()
  "Fallthrough handler, for anything we haven't already defined."
  (setf (tbnl:content-type*) "text/plain")
  (setf (tbnl:return-code*) tbnl:+http-not-found+)
  "This is not a valid URI")

;; Dispatch them
(setf tbnl:*dispatch-table*
      (list ;(tbnl:create-prefix-dispatcher "/v1/ipv4-addresses/" 'ipv4-address)
            ;; Fallback.
            ;; This must be last, because they're inspected in order,
            ;; first match wins.
            (tbnl:create-prefix-dispatcher "/" 'four-oh-four)))
