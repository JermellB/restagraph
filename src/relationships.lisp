;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Relationship-related methods

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(defgeneric create-relationship-by-path (db sourcepath destpath schema)
  (:documentation "Create a relationship between two arbitrary, pre-existing resources. The last element of the sourcepath must be the relationship type."))

(defmethod create-relationship-by-path ((db neo4cl:neo4j-rest-server)
                                        (sourcepath string)
                                        (destpath string)
                                        schema)
  (log-message :debug (format nil "Attempting to create a relationship from ~A to ~A"
                              sourcepath destpath))
  ;; Initial sanity-checks
  (let ((source-part-list (get-uri-parts sourcepath))
        (dest-parts (get-uri-parts destpath)))
    (cond
      ((not (equal (mod (length source-part-list) 3) 0))
       (let ((message (format nil "~A is not a valid path to a relationship" sourcepath)))
         (log-message :debug message)
         (error 'client-error :message message)))
      ((not (equal (mod (length dest-parts) 3) 2))
       (let ((message (format nil "~A is not a valid path to a resource" destpath)))
         (log-message :debug message)
         (error 'client-error :message message)))
      ;; Having made it that far, make checks that call to the database
      (t
       (let* ((relationship (car (last source-part-list)))
              (source-parts (butlast source-part-list)) ; Path to the source resource
              (source-type (nth (- (length source-parts) 2) source-parts))
              (dest-type (nth (- (length dest-parts) 2) dest-parts))
              ;; Note that this enables an "any" relationship to be pre-empted
              ;; by a more specific definition.
              (relationship-attrs
                (or (get-relationship schema source-type relationship dest-type)
                    (get-relationship schema "any" relationship dest-type))))
         (log-message :debug "Basic sanity-checks passed. Starting more in-depth checks.")
         (cond
           ;; No such relationship
           ((not relationship-attrs)
            (let ((message
                    (format nil "'~A' is not a valid relationship from type '~A' to type '~A'"
                            relationship source-type dest-type)))
              (log-message :debug message)
              (error 'integrity-error :message message)))
           ;; 1:1 dependent relationship
           ((and
              (dependent relationship-attrs)
              (or
                (equal (cardinality relationship-attrs) "1:1")
                (equal (cardinality relationship-attrs) "1:many")))
            (let ((message (format nil "~A dependency. Either move the relationship or create a new dependent resource."
                                   (cardinality relationship-attrs))))
              (log-message :debug message)
              (error 'integrity-error :message message)))
           ;; Are we trying to create a duplicate?
           ((check-relationship-by-path
              db (format nil "~{/~A~}" source-parts) relationship destpath)
            (let ((message "Relationship already exists"))
              (log-message :debug message)
              (error 'integrity-error :message message)))
           ;; Do both the source and destination resources actually exist?
           ((null (get-resources db (format nil "/~{~A~^/~}" source-parts)))
            (let ((message (format nil "The source resource /~{~A~^/~} does not exist" source-parts)))
              (log-message :debug message)
              (error 'client-error :message message)))
           ((null (get-resources db destpath))
            (let ((message "The destination resource does not exist"))
              (log-message :debug message)
              (error 'client-error :message message)))
           ;; Many-to-one, and the source already has this relationship with another such target?
           ((and
              (equal (cardinality relationship-attrs) "many:1")
              (>
                (neo4cl:extract-data-from-get-request
                  (neo4cl:neo4j-transaction
                    db
                    `((:STATEMENTS
                        ((:STATEMENT
                           .  ,(format nil "MATCH ~A-[:~A]->(b:~A) RETURN count(b)"
                                       (uri-node-helper source-parts
                                                        :path ""
                                                        :marker "a"
                                                        :directional t)
                                       relationship
                                       dest-type)))))))
                0))
            (let ((message (format nil"~{~A~^/~} already has a many:1 ~A relationship with a resource of type ~A"
                                   source-parts relationship dest-type)))
              (log-message :debug message)
              (error 'integrity-error :message message)))
           ;; Go ahead and create the relationship
           (t
             (neo4cl:neo4j-transaction
               db
               `((:STATEMENTS
                   ((:STATEMENT
                      .  ,(format nil "MATCH ~A, ~A MERGE (a)-[:~A]->(b)"
                                  (uri-node-helper source-parts
                                                   :path ""
                                                   :marker "a"
                                                   :directional t)
                                  (uri-node-helper dest-parts
                                                   :path ""
                                                   :marker "b"
                                                   :directional t)
                                  relationship)))))))))))))


(defgeneric check-relationship-by-path (db sourcepath relationship destpath)
  (:documentation "Confirm whether this relationship exists between these resources.
                   A special-case method for avoiding the ambiguity that can catch out get-resources."))


(defmethod check-relationship-by-path ((db neo4cl:neo4j-rest-server)
                                       (sourcepath string)
                                       (relationship string)
                                       (destpath string))
  (neo4cl:extract-rows-from-get-request
    (neo4cl:neo4j-transaction
      db
      `((:STATEMENTS
          ((:STATEMENT .
            ,(format nil "MATCH ~A-[r:~A]->~A RETURN labels(a), a.uid, r, labels(b), b.uid"
                     (uri-node-helper (get-uri-parts sourcepath)
                                      :path ""
                                      :marker "a"
                                      :directional t)
                     relationship
                     (uri-node-helper (get-uri-parts destpath)
                                      :path ""
                                      :marker "b"
                                      :directional nil)))))))))


(defgeneric delete-relationship-by-path (db schema relationship-uri target-resource)
  (:documentation "Delete a relationship based on its path, and that of its target.
                  Arguments:
                  - relationship-uri = URI of the relationship itself
                  - target-resource = /<type>/<uid> of the resource at the end of the relationship.
                  This form is required to distinguish between deleting the relationship, and the resource itself."))

(defmethod delete-relationship-by-path ((db neo4cl:neo4j-rest-server)
                                        (schema hash-table)
                                        (relationship-uri string)
                                        (target-resource string))
  (log-message :debug (format nil "Attempting to delete the relationship ~A to ~A"
                              relationship-uri target-resource))
  (let* ((rel-parts (get-uri-parts relationship-uri))
         (source-type (car (last (butlast rel-parts 2))))
         (relationship (car (last rel-parts)))
         (dest-parts (get-uri-parts target-resource))
         (dest-type (first dest-parts))
         (dest-uid (second dest-parts))
         (relationship-attrs
           (car (or (get-relationship schema source-type relationship dest-type)
                    (get-relationship schema "any" relationship dest-type)))))
    (log-message :debug (format nil "Source type: ~A" source-type))
    (log-message :debug (format nil "Relationship: ~A" relationship))
    (log-message :debug (format nil "Dest type: ~A" dest-type))
    (log-message :debug (format nil "Dest UID: ~A" dest-uid))
    ;; Sanity checks
    (cond
      ;; Is the relationship URI valid?
      ((not (equal (mod (length rel-parts) 3) 0))
       (error 'client-error :message "This URI does not specify a relationship."))
      ;; Is the target URI valid?
      ((not (equal (mod (length dest-parts) 3) 2))
       (error 'client-error :message "Target path does not specify a resource."))
      ;; Is there a relationship defined between these types?
      ((not relationship-attrs)
       (error
         'client-error
         :message "There is no relationship between these resource-types. Are you sure there's something here to delete?"))
      ;; Would this orphan a dependent resource at the end of the relationship,
      ;; by removing its last parent?
      ((and
         (dependent relationship-attrs)
         ;; Would this be the last parent?
         ;; Test by checking for other incoming dependent relationships.
         ;; If there's one or more, we're good to go.
         (let ((others (neo4cl:extract-rows-from-get-request
                         (neo4cl:neo4j-transaction
                           db
                           `((:STATEMENTS
                               ((:STATEMENT
                                  .
                                  ,(format nil "MATCH ~A<-[r]-(n) RETURN type(r), labels(n);"
                                           (uri-node-helper
                                             (append rel-parts dest-parts)
                                             :path ""
                                             :marker "n"
                                             :directional t))))))))))
           (log-message :debug (format nil "Found ~D other incoming relationships to the target resource"
                                       (length others)))
           ;; Either there are no others to check...
           (or (null others)
               ;; ...or there is at least one, but none of them is a dependent type
               (not (some #'(lambda (inc)
                              ;; Is there a :dependent value of 't in this incoming relationship?
                              (log-message
                                :debug
                                (format nil "Checking for dependencies in incoming relationship ~A from type ~A"
                                        (car inc) (car (second inc))))
                              (dependent
                                (get-relationship schema dest-type (car inc) (car (second inc)))))
                          others)))))
       (error 'integrity-error
              :message "This would leave an orphan dependent resource. Delete the dependent resource instead."))
      ;; Sanity-checks passed; let's try to make it happen
      (t
        (neo4cl:neo4j-transaction
          db
          `((:STATEMENTS
              ((:STATEMENT
                 .
                 ,(format nil "MATCH ~A-[r:~A]->(:~A {uid: '~A'}) DELETE r"
                          (uri-node-helper (butlast rel-parts) :path "" :marker "n" :directional t)
                          relationship
                          dest-type
                          dest-uid))))))))))
