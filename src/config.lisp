;;;; Configs for the server to use

(in-package #:restagraph)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4950
    :datastore ,(make-instance 'neo4cl:neo4j-rest-server
                               :dbpasswd "wallaby")
    ;; Attributes that users should be able to add to all resources
    :default-write-attributes ("comment")
    ;; default-read-attributes should always include "created"
    ;; unless you have a good reason to hide the creation-date of a resource.
    ;; It's probably a good idea to also include all the default-write-attributes.
    :default-read-attributes ("created" "comment")))
