;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Configs for the server to use

(in-package #:restagraph)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4950
    :dbhostname "10.255.0.1"
    :dbport 7474
    :dbname "neo4j"
    :dbusername "neo4j"
    :dbpasswd "wallaby"
    :api-uri-base "/raw/v1"
    :schema-uri-base "/schema/v1"
    :files-uri-base "/files/v1"
    :files-temp-location "/tmp/restagraph-files-tmp/"
    :files-location "/tmp/restagraph-files"))

(setf *loglevel* :info)

;; Define the core schemas, without which RG won't work properly
(defparameter *core-schemas*
  (list (list :name "meta"
              :version 5
              :resourcetypes
              (list (make-incoming-rtypes
                      :name "any"
                      :notes "Special-case meta-resource, representing an instance of any type of resource.")
                    (make-incoming-rtypes
                      :name "tags"
                      :notes "For categorising resources of any type."
                      :attributes (list (make-incoming-rtype-attrs
                                          :name "description"
                                          :description "Clarification of what the tag means.")))
                    (make-incoming-rtypes
                      :name "groups"
                      :notes "For collecting resources into explicit groups."
                      :attributes (list (make-incoming-rtype-attrs
                                          :name "description"
                                          :description "Clarification of what the group means.")))
                    (make-incoming-rtypes
                      :name "comments"
                      :dependent t
                      :notes "Comments on things."
                      :attributes (list (make-incoming-rtype-attrs
                                          :name "fulltext"
                                          :description "The full text of the comment"))))
              :relationships `(("any" . ,(make-incoming-rels :relationship "Tags"
                                                             :target-type "tags"))
                               ("any" . ,(make-incoming-rels :relationship "Groups"
                                                             :target-type "groups"))
                               ("any" . ,(make-incoming-rels :relationship "Comments"
                                                             :target-type "comments"
                                                             :cardinality "1:many"
                                                             :dependent t))))
        (list :name "people"
              :version 1
              :resourcetypes
              (list (make-incoming-rtypes
                      :name "people"
                      :notes "UID should be their login name or some other compact reference."
                      :attributes (list (make-incoming-rtype-attrs
                                          :name "displayname"
                                          :description "The human-friendly version of their name, to be displayed in the UI.")
                                        (make-incoming-rtype-attrs
                                          :name "notes"
                                          :description "Notes about this person.")))
                    (make-incoming-rtypes
                      :name "roles"
                      :notes "Principally for role-based access control, but can also tie into organisational roles."
                      :attributes (list (make-incoming-rtype-attrs
                                          :name "displayname"
                                          :description "The human-friendly version of the role's name, to be displayed in the UI.")
                                        (make-incoming-rtype-attrs
                                          :name "notes"
                                          :description "Clarification of what this role is for.")))))
        (list :name "files"
              :version 1
              :resourcetypes
              (list (make-incoming-rtypes
                      :name "files"
                      :notes "Files uploaded by users."
                      :attributes (list (make-incoming-rtype-attrs
                                          :name "title"
                                          :description "The UID requested by the client")
                                        (make-incoming-rtype-attrs
                                          :name "originalname"
                                          :description "The filename sent by the browser.")
                                        (make-incoming-rtype-attrs
                                          :name "notes"
                                          :description "Notes about this file")
                                        (make-incoming-rtype-attrs
                                          :name "mimetype"
                                          :description "The detected mime-type of this file.")
                                        (make-incoming-rtype-attrs
                                          :name "sha3256sum"
                                          :description "The SHA3-256 checksum of the file. Chosen for resistance against length-extension collisions."))))
              :relationships `(("files" . ,(make-incoming-rels :relationship "Thumbnail"
                                                               :target-type "files"))
                               ("people" . ,(make-incoming-rels :relationship "Roles"
                                                                :target-type "roles"))))))
