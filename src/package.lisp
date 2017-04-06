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
    ;; Methods
    get-resource-defs-from-db
    get-resource-attributes-from-db
    relationship-valid-p
    enforce-db-schema
    dependent-resource-p
    store-resource
    store-dependent-resource
    move-dependent-resource
    get-resources
    get-dependent-resources
    get-dependen-relationships-for-type
    dependent-relationship-p
    create-relationship-by-path
    get-resources-with-relationship
    check-relationship-by-path
    delete-relationship-by-path
    get-relationship-attrs
    delete-resource-by-path
    ))
