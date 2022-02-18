;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for all of restagraph
;;;;
;;;; Beware: it currently only tests _expected_ cases,
;;;; and does not test edge-cases or wrong input.

(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite main)
(fiveam:in-suite main)


(defparameter *http-server*
  (make-instance 'neo4cl:neo4j-rest-server
                 :hostname (getf restagraph::*config-vars* :dbhostname)
                 :dbname (getf restagraph::*config-vars* :dbname)
                 :dbuser (getf restagraph::*config-vars* :dbusername)
                 :dbpasswd (getf restagraph::*config-vars* :dbpasswd)))

(defparameter *bolt-auth-basic*
  (make-instance 'neo4cl:bolt-auth-basic
                 :username "neo4j"
                 :password "wallaby"))

(defparameter *bolt-auth-none*
  (make-instance 'neo4cl:bolt-auth-none
                 :username "neo4j"))

(defparameter *bolt-server*
  (make-instance 'neo4cl:bolt-server
                 ;:hostname "192.0.2.1"
                 :hostname "localhost"
                 :auth-token *bolt-auth-basic*))

(defparameter *admin-user* "RgAdmin")
