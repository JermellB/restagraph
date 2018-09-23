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
    add-resourcetype
    resourcetype-exists-p
    add-resourcetype-attribute
    resourcetype-attribute-exists-p
    get-resource-attributes-from-db
    update-resourcetype-attribute
    delete-resourcetype-attribute
    delete-resourcetype
    add-resource-relationship
    delete-resource-relationship
    get-resource-types
    describe-resource-type
    relationship-valid-p
    enforce-db-schema
    dependent-resource-p
    store-resource
    update-resource-attributes
    update-resource-attributes
    store-dependent-resource
    move-dependent-resource
    get-resources
    get-dependent-resources
    get-dependent-relationships-for-type
    dependent-relationship-p
    create-relationship-by-path
    get-resources-with-relationship
    check-relationship-by-path
    delete-relationship-by-path
    get-relationship-attrs
    delete-resource-by-path
    ))
