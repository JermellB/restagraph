;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Configs for the server to use

(in-package #:restagraph)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


;; Define the core schemas, without which RG won't work properly
(defparameter *core-schema*
  (make-incoming-subschema-version
    :name "core"
    :version 1
    :apply-order 0
    :resourcetypes
    (list (make-incoming-rtypes
            :name "any"
            :notes "Special-case meta-resource, representing an instance of any type of resource."
            :attributes ())
          (make-incoming-rtypes
            :name "Tags"
            :notes "For categorising resources of any type."
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"
                                :description "Clarification of what the tag means.")))
          (make-incoming-rtypes
            :name "Groups"
            :notes "For collecting resources into explicit groups."
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"
                                :description "Clarification of what the group means.")))
          (make-incoming-rtypes
            :name "People"
            :notes "UID should be their login name or some other compact reference."
            :attributes (list (make-incoming-rtype-attrs
                                :name "displayname"
                                :description "The human-friendly version of their name, to be displayed in the UI.")
                              (make-incoming-rtype-attrs
                                :name "notes"
                                :description "Notes about this person.")))
          (make-incoming-rtypes
            :name "Pronouns"
            :notes "The pronouns by which a person prefers to be addressed."
            :attributes ())
          (make-incoming-rtypes
            :name "Files"
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
    :relationships `(("any" . ,(make-incoming-rels :relationship "TAGS"
                                                   :target-type "Tags"))
                     ("any" . ,(make-incoming-rels :relationship "GROUPS"
                                                   :target-type "Groups"))
                     ("any" . ,(make-incoming-rels :relationship "CREATOR"
                                                   :target-type "People"))
                     ("People" . ,(make-incoming-rels :relationship "PRONOUNS"
                                                      :target-type "Pronouns")))))
