;;;; Schema creation functions

(in-package #:restagraph)

(defun inject-schema (db schema-path)
  (let ((schema (cl-yaml:parse (pathname schema-path))))
    ;; Resourcetypes
    (maphash
      #'(lambda (resourcename value)
          (add-resourcetype
          db
          resourcename
          :attrs (gethash "attributes" value)
          :dependent (gethash "dependent" value)
          :notes (gethash "notes" value)))
      (gethash "resourcetypes" schema))
    ;; Relationships between resourcetypes
    (mapcar
      #'(lambda (rel)
          (let ((relparts (cl-ppcre:split "/" (gethash "uri" rel))))
          (add-resource-relationship
            db
            (second relparts)   ; parent-type
            (third relparts)    ; relationship
            (fourth relparts)   ; dependent-type
            :dependent (gethash "dependent" rel)
            :cardinality (gethash "cardinality" rel)
            :notes (gethash "notes" rel))))
      (gethash "relationships" schema))))

