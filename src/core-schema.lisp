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
    :resourcetypes
    (list (make-incoming-rtypes
            :name "any"
            :description "Special-case meta-resource, representing an instance of any type of resource. This is used for defining relationships where either the source or target could be, well, any resourcetype. The server refuses to create an instance of this resourcetype."
            :attributes ())
          (make-incoming-rtypes
            :name "Tags"
            :description "For categorising resources of any type. Useful in searches."
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"
                                :description "Clarification of what the tag means.")))
          (make-incoming-rtypes
            :name "Groups"
            :description "For collecting resources into explicit groups."
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"
                                :description "Clarification of what the group means.")))
          (make-incoming-rtypes
            :name "Organisations"
            :description "Any kind of organisation: professional, social or other."
            :dependent nil
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"
                                :description "Notes about this particular organisation.")))
          (make-incoming-rtypes
            :name "People"
            :description "Real people, imaginary people, security roles, members of an external organisation... if they're a person, this is the type."
            :attributes (list (make-incoming-rtype-attrs
                                :name "displayname"
                                :description "The human-friendly version of their name, to be displayed in the UI.")
                              (make-incoming-rtype-attrs
                                :name "notes"
                                :description "Notes about this person.")))
          (make-incoming-rtypes
            :name "Pronouns"
            :description "The pronouns by which a person prefers to be addressed."
            :attributes ())
          (make-incoming-rtypes
            :name "Files"
            :description "Metadata about files uploaded by users. The files themselves are stored separately, using the sha3-256 checksum as the filename."
            :attributes (list (make-incoming-rtype-attrs
                                :name "title"
                                :description "The requested filename, recorded verbatim instead of having to be sanitised for URI-safety.")
                              (make-incoming-rtype-attrs
                                :name "notes"
                                :description "Notes about this file.")
                              (make-incoming-rtype-attrs
                                :name "mimetype"
                                :description "The detected mime-type of this file.")
                              (make-incoming-rtype-attrs
                                :name "sha3256sum"
                                :description "The SHA3-256 checksum of the file. Chosen for resistance against length-extension collisions.")))
          (make-incoming-rtypes
            :name "VrfGroups"
            :description "VRF Groups, as allocated by an organisation."
            :dependent t
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"
                                :description "Helpful notes about what this group is for.")))
          (make-incoming-rtypes
            :name "Ipv4Subnets"
            :description "IPv4 Subnets, as allocated rather than as configured."
            :dependent t
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"
                                :description "Who or what this subnet is allocated for, and possibly why.")
                              (make-incoming-rtype-attrs
                                :name "netaddress"
                                :description "The network address of the subnet.")
                              (make-incoming-rtype-attrs
                                :name "prefixlength"
                                :description "The prefix length of the subnet - an integer between 1 and 32.")))
          (make-incoming-rtypes
            :name "Ipv6Subnets"
            :description "IPv6 Subnets, as allocated rather than as configured."
            :dependent t
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"
                                :description "Who or what this subnet is allocated for, and possibly why.")
                              (make-incoming-rtype-attrs
                                :name "netaddress"
                                :description "The network address of the subnet.")
                              (make-incoming-rtype-attrs
                                :name "prefixlength"
                                :description "The prefix length of the subnet - an integer between 1 and 64.")))
          (make-incoming-rtypes
            :name "Ipv4Addresses"
            :description "IPv4 Addresses. Unqualified, so really only useful for allocating."
            :dependent t
            :attributes (list (make-incoming-rtype-attrs
                                :name "description")))
          (make-incoming-rtypes
            :name "Ipv6Addresses"
            :description "IPv6 Addresses. Unqualified, so really only useful for allocating."
            :dependent t
            :attributes (list (make-incoming-rtype-attrs
                                :name "description"))))
    :relationships (list (make-incoming-rels :name "TAGS"
                                             :source-type "any"
                                             :target-type "Tags"
                                             :cardinality "many:many"
                                             :description "Any resourcetype can be tagged.")
                         (make-incoming-rels :name "GROUPS"
                                             :source-type "any"
                                             :target-type "Groups"
                                             :cardinality "many:many"
                                             :description "Any resourcetype can be assigned to a group.")
                         (make-incoming-rels :name "CREATOR"
                                             :source-type "any"
                                             :target-type "People"
                                             :description "In a coming upgrade, all resources will have a creator as part of the permissions-management system.")
                         (make-incoming-rels :name "PRONOUNS"
                                             :source-type "People"
                                             :target-type "Pronouns"
                                             :description "She/her, they/them, he/him and whatever others you choose to add. These are defined as a separate resourcetype partly because some people accept more than one set, and partly to make it easier to add more as necessary.")
                         (make-incoming-rels :name "MEMBERS"
                                             :source-type "Organisations"
                                             :target-type "People")
                         (make-incoming-rels :name "MEMBER_OF"
                                             :source-type "People"
                                             :target-type "Organisations")
                         (make-incoming-rels :name "VRF_GROUPS"
                                             :source-type "Organisations"
                                             :target-type "VrfGroups"
                                             :cardinality "1:many"
                                             :dependent t)
                         (make-incoming-rels :name "SUBNETS"
                                             :source-type "Organisations"
                                             :target-type "Ipv4Subnets"
                                             :cardinality "1:many"
                                             :dependent t)
                         (make-incoming-rels :name "SUBNETS"
                                             :source-type "VrfGroups"
                                             :target-type "Ipv4Subnets"
                                             :cardinality "1:many"
                                             :dependent t)
                         (make-incoming-rels :name "SUBNETS"
                                             :source-type "Ipv4Subnets"
                                             :target-type "Ipv4Subnets"
                                             :cardinality "1:many"
                                             :dependent t)
                         (make-incoming-rels :name "ADDRESSES"
                                             :source-type "Ipv4Subnets"
                                             :target-type "Ipv4Addresses"
                                             :cardinality "1:many"
                                             :dependent t)
                         (make-incoming-rels :name "SUBNETS"
                                             :source-type "Organisations"
                                             :target-type "Ipv6Subnets"
                                             :cardinality "1:many"
                                             :dependent t)
                         (make-incoming-rels :name "SUBNETS"
                                             :source-type "VrfGroups"
                                             :target-type "Ipv6Subnets"
                                             :cardinality "1:many"
                                             :dependent t)
                         (make-incoming-rels :name "SUBNETS"
                                             :source-type "Ipv6Subnets"
                                             :target-type "Ipv6Subnets"
                                             :cardinality "1:many"
                                             :dependent t)
                         (make-incoming-rels :name "ADDRESSES"
                                             :source-type "Ipv6Subnets"
                                             :target-type "Ipv6Addresses"
                                             :cardinality "1:many"
                                             :dependent t))))
