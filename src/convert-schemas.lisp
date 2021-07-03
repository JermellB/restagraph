(defpackage convert-schemas
  (:use #:cl
        #:cl-yaml
        #:cl-change-case
        #:cl-json))

(in-package :convert-schemas)

(defun log-message (severity message)
  (format t "~%~A: ~A" severity message))

(defun enumerate-schemas-in-dir (schemadir)
  "Return a list of pathnames for .yaml files in the specified directory."
  (log-message :info (format nil "Attempting to read schemas in directory ~A" schemadir))
  ;; Safety first: is the directory even there?
  (let ((schemapath (directory-namestring schemadir)))
    (if (probe-file schemapath)
      ;; This is _really_ ugly, but guarantees alphabetical order.
      (mapcar #'pathname
              (sort (mapcar #'namestring
                            (directory (make-pathname
                                         :name :wild
                                         :type "yaml"
                                         :directory schemapath)))
                    #'string<))
      ;; Safety-check failed. Complain loudly.
      (let ((message (format nil "Schema directory ~A doesn't exist!" schemapath)))
        (log-message :fatal message)
        (error message)))))

(defun digest-schema-yaml (filepath)
  "Digest a single YAML file, and return it as a plist with the following keys:
   :name = string. Schema name, as reported in the YAML file.
   :version = string. Schema version, as reported in the YAML file.
   :resourcetypes = list of schema-rtypes structs.
   :relationships = list of dotted-lists: (<source-type name> . <schema-rels struct>)"
  (declare (type pathname filepath))
  (log-message :info (format nil "Attempting to digest schema file ~A" filepath))
  (let ((schema (cl-yaml:parse filepath)))
    (list
      :name (gethash "name" schema)
      :version (gethash "version" schema)
      ;; Don't assume there _are_ resourcetypes defined in this schema.
      ;; Also, `maphash` breaks when you feed it '().
      :resourcetypes
      (if (gethash "resourcetypes" schema)
          (loop for resourcename being the hash-keys
                in (gethash "resourcetypes" schema)
                using (hash-value value)
                collect (list
                          :name resourcename
                          :dependent (when (or (equal (gethash "dependent" value) t)
                                               (equal (gethash "dependent" value) "true"))
                                       t)
                          :notes (or (gethash "notes" value) "")
                          :attributes
                          ;; Don't assume this resourcetype has attributes defined.
                          ;; Again, `maphash` doesn't cope with NIL.
                          (when (gethash "attributes" value)
                            (loop for attrname being the hash-keys
                                  in (gethash "attributes" value)
                                  using (hash-value attrdetails)
                                  collect (list
                                            :name attrname
                                            :description (if (and attrdetails
                                                                  (hash-table-p attrdetails)
                                                                  (gethash "description" attrdetails))
                                                             (gethash "description" attrdetails)
                                                             "")
                                            :values (when (and attrdetails
                                                               (hash-table-p attrdetails)
                                                               (gethash "vals" attrdetails))
                                                      (cl-ppcre:split ","
                                                                      (gethash "vals" attrdetails))))))))
          (log-message :info (format nil "No resourcetypes found in schema '~A'"
                                     (gethash "name" schema))))
      ;; Don't assume there _are_ relationships defined in this schema.
      :relationships
      (if (gethash "relationships" schema)
          (remove-if
                    #'null
                    (mapcar
                      #'(lambda (rel)
                          (let* ((rel-parts
                                   (remove-if #'(lambda (part) (equal "" part)) 
                                              ;; If there's a URI attribute, split it on forward-slash and remove
                                              ;; any remaining empty strings resulting from a leading slash.
                                              ;; If there isn't, do that to an empty string. It's simpler this way.
                                              (cl-ppcre:split "/" (gethash "uri" rel ""))))
                                 ;; Enforce a valid cardinality.
                                 ;; If a valid one was not supplied, make it the default.
                                 (raw-cardinality (gethash "cardinality" rel "many:many"))
                                 (cardinality (if (member raw-cardinality
                                                          '("many:many" "1:many" "many:1" "1:1")
                                                          :test #'equal)
                                                  raw-cardinality
                                                  "many:many")))
                            ;; Warn the admin if the original cardinality was not valid.
                            (when (not (equal raw-cardinality cardinality))
                              (log-message
                                :error
                                (format nil "Relationship /~A/~A/~A had invalid cardinality '~A'. Forcing to default many:many."
                                        (first rel-parts)
                                        (second rel-parts)
                                        (third rel-parts)
                                        raw-cardinality)))
                            ;; If there was a valid path, generate the expected plist.
                            (if (equal (length rel-parts) 3)
                                ;; Create a duple: name of the source-type, and a schema-rels struct
                                (list
                                  :source-type (first rel-parts)
                                  :relationship (second rel-parts)
                                  :target-type (third rel-parts)
                                  :cardinality cardinality
                                  :dependent (when (gethash "dependent" rel) t)
                                  :notes (gethash "notes" rel))
                                ;; If not, log the fact and return a null value for filtering out.
                                (progn
                                  (log-message :warn "No URI in this entry. Skipping.")
                                  '()))))
                      (gethash "relationships" schema)))
          (log-message :info (format nil "No relationships found in schema '~A'"
                                     (gethash "name" schema)))))))

(defun update-hash-from-digest (hash digest)
  "Update the contents of a hash-table (with test #'equal),
   from the output of digest-schema-yaml."
  (log-message :debug (format nil "Attempting to update a hash-table with the contents of digest ~A"
                              (getf digest :name)))
  ;; Add the resourcetypes to the hash
  `((:RESOURCES . ,(append (cdr (assoc :RESOURCES hash))
                           (mapcar #'(lambda (resourcetype)
                                       `(("name" . ,(cl-change-case:pascal-case (getf resourcetype :NAME)))
                                         ("dependent" . ,(if (null (getf resourcetype :DEPENDENT))
                                                             "false"
                                                             (getf resourcetype :DEPENDENT)))
                                         ("notes" . ,(getf resourcetype :NOTES))
                                         ("attributes"
                                          . ,(mapcar #'(lambda (attr)
                                                         `(("name" . ,(getf attr :NAME))
                                                           ("description" . ,(getf attr :DESCRIPTION))
                                                           ("values" . ,(getf attr :VALUES))))
                                                     (getf resourcetype :ATTRIBUTES)))))
                                   (getf digest :resourcetypes))))
    ;; Update the relationships between resourcetypes.
    ;; Do this as a separate step, to allow for back-references.
    (:RELATIONSHIPS . ,(append (cdr (assoc :RELATIONSHIPS hash))
                               (mapcar #'(lambda (rel)
                                           `(("source-type" . ,(cl-change-case:pascal-case (getf rel :SOURCE-TYPE)))
                                             ("name" . ,(string-upcase (cl-change-case:snake-case (getf rel :RELATIONSHIP))))
                                             ("target-type" . ,(cl-change-case:pascal-case (getf rel :TARGET-TYPE)))
                                             ("cardinality" . ,(getf rel :cardinality))
                                             ("dependent" . ,(getf rel :DEPENDENT))
                                             ("notes" . ,(if (getf rel :NOTES) (getf rel :NOTES) nil))))
                                       (getf digest :RELATIONSHIPS))))))

(defun inject-all-schemas (schemas hash)
  "Update the supplied hash-table with the digested contents of the schema directory."
  (if (car schemas)
      (inject-all-schemas
        (cdr schemas)
        (update-hash-from-digest hash (digest-schema-yaml (car schemas))))
      hash))


(defun convert-schemas (schemadir outfile)
  (let ((interim (inject-all-schemas
                   (enumerate-schemas-in-dir schemadir)
                   '((:RESOURCES . ()) (:RELATIONSHIPS . ())))))
    (with-open-file (str outfile :direction :output :if-exists :supersede)
      (cl-json:encode-json-alist
        `(("name" . "webcat")
          (:RESOURCETYPES . ,(stable-sort
                               (cdr (assoc :RESOURCES interim))
                               #'string<
                               :key #'(lambda (record) (cdr (assoc "name" record :test #'equal)))))
          (:RELATIONSHIPS . ,(stable-sort
                               (stable-sort
                                 (cdr (assoc :RELATIONSHIPS interim))
                                 #'string<
                                 :key #'(lambda (record) (cdr (assoc "name" record :test #'equal))))
                               #'string<
                               :key #'(lambda (record) (cdr (assoc "source-type" record :test #'equal))))))
        str))))
