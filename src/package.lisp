;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


(defpackage restagraph
  (:use
    #:cl)
  (:export
    ;; Operational functions
    startup
    shutdown
    populate-schema
    log-message
    ;; Conditions
    integrity-error
    client-error
    message
    ;; Functions
    sanitise-uid
    ;; Methods
    resourcetype-exists-p
    get-resource-attributes-from-db
    add-resource-relationship
    describe-resource-type
    enforce-db-schema
    dependent-resource-p
    store-resource
    update-resource-attributes
    store-dependent-resource
    move-dependent-resource
    get-resources
    get-dependent-resources
    get-dependent-relationships-for-type
    dependent-relationship-p
    create-relationship-by-path
    check-relationship-by-path
    delete-relationship-by-path
    delete-resource-by-path
    ;; Structures and their constructors and accessors
    make-relationship-attrs
    relationship-attrs-dependent
    relationship-attrs-cardinality
    schema
    ))
