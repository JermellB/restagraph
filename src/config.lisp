;;;; Configs for the server to use

(in-package #:restagraph)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4950
    :datastore ,(make-instance 'neo4cl:neo4j-rest-server
                               :dbpasswd "wallaby")))
