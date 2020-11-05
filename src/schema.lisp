;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Schema creation functions

(in-package #:restagraph)

(defun digest-schema-yaml (filepath)
  "Digest a single YAML file, and return it as a plist with the following keys:
   :name = string. Schema name, as reported in the YAML file.
   :version = string. Schema version, as reported in the YAML file.
   :resourcetypes = list of schema-rtypes structs.
   :relationships = list of dotted-lists: (<source-type name> . <schema-rels struct>)"
  (declare (type pathname filepath))
  (log-message :info "Attempting to digest schema file ~A" filepath)
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
                collect (make-schema-rtypes
                          :name resourcename
                          :dependent (gethash "dependent" value)
                          :notes (or (gethash "notes" value) "")
                          :attributes
                          ;; Don't assume this resourcetype has attributes defined.
                          ;; Again, `maphash` doesn't cope with NIL.
                          (when (gethash "attributes" value)
                            (loop for attrname being the hash-keys
                                  in (gethash "attributes" value)
                                  using (hash-value attrdetails)
                                  collect (make-schema-rtype-attrs
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
          (log-message :info "No resourcetypes found in schema '~A'"
                       (gethash "name" schema)))
      ;; Don't assume there _are_ relationships defined in this schema.
      :relationships
      (if (gethash "relationships" schema)
          (remove-if
                    #'null
                    (mapcar
                      #'(lambda (rel)
                          (log-message :info "Attempting to create a schema-rel struct with URI '~A" (gethash "uri" rel))
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
                              (log-message :error "Relationship /~A/~A/~A had invalid cardinality '~A'. Forcing to default many:many."
                                           (first rel-parts)
                                           (second rel-parts)
                                           (third rel-parts)
                                           raw-cardinality))
                            ;; If there was a valid path, generate the expected plist.
                            (if (equal (length rel-parts) 3)
                                ;; Create a duple: name of the source-type, and a schema-rels struct
                                (cons
                                  (first rel-parts)
                                  (make-schema-rels
                                    :relationship (second rel-parts)
                                    :target-type (third rel-parts)
                                    :cardinality cardinality
                                    :dependent (when (gethash "dependent" rel) t)
                                    :notes (gethash "notes" rel)))
                                ;; If not, log the fact and return a null value for filtering out.
                                (progn
                                  (log-message :warn "No URI in this entry. Skipping.")
                                  '()))))
                      (gethash "relationships" schema)))
          (log-message "No relationships found in schema '~A'"
                       (gethash "name" schema))))))

(defun update-hash-from-digest (hash digest)
  "Update the contents of a hash-table (with test #'equal),
   from the output of digest-schema-yaml."
  (declare (type hash-table hash)
           (list digest))
  (log-message :debug "Attempting to update a hash-table with the contents of digest ~A"
               (getf digest :name))
  ;; Add the resourcetypes to the hash
  (mapcar #'(lambda (rtype)
              ;; Check for duplicates
              (if (gethash (schema-rtypes-name rtype) hash)
                  ;; If it's a dupe, warn and move on.
                  (log-message :error "Refusing to create duplicate entry for resourcetype '~A'"
                               (schema-rtypes-name rtype))
                  ;; Not a dupe; add it to the hash.
                  (progn
                    (log-message :debug "Adding resourcetype '~A' to the hash"
                                 (schema-rtypes-name rtype))
                    (setf (gethash (schema-rtypes-name rtype) hash) rtype))))
          (getf digest :resourcetypes))
  ;; Update the relationships between resourcetypes.
  ;; Do this as a separate step, to allow for back-references.
  (mapcar #'(lambda (rel)
              (log-message
                :debug
                "Attempting to update resourcetype '~A' with relationship '~A' to resourcetype '~A'"
                (car rel)
                (schema-rels-relationship (cdr rel))
                (schema-rels-target-type (cdr rel)))
              ;; Is the source-type in there?
              (let ((sourcetype (gethash (car rel) hash)))
                (if sourcetype
                    ;; If so, go ahead.
                    (add-rel-to-schema-rtype sourcetype (cdr rel))
                    ;; If it's not there, complain and move on.
                    (log-message :error "Doomed attempt to update nonexistent resourcetype '~A' with relationship '~A' to resourcetype '~A'"
                                 (car rel)
                                 (schema-rels-relationship (cdr rel))
                                 (schema-rels-target-type (cdr rel))))))
          (getf digest :relationships)))

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

(defun inject-all-schemas (hash schemadir)
  "Update the supplied hash-table with the digested contents of the schema directory."
  (declare (type hash-table hash)
           (type pathname schemadir))
  (mapcar #'(lambda (schemafile)
              (update-hash-from-digest hash (digest-schema-yaml schemafile)))
          (enumerate-schemas-in-dir schemadir)))
