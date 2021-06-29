;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for side-effecting code.

(in-package #:restagraph-test)

(fiveam:def-suite
  side-effecting
  :description "Tests for side-effecting code."
  :in main)

(fiveam:in-suite side-effecting)

(defparameter *server*
  (make-instance 'neo4cl:neo4j-rest-server
                 :hostname (getf restagraph::*config-vars* :dbhostname)
                 :dbname (getf restagraph::*config-vars* :dbname)
                 :dbuser (getf restagraph::*config-vars* :dbusername)
                 :dbpasswd (getf restagraph::*config-vars* :dbpasswd)))

(fiveam:test
  authenticate-to-neo4j
  :depends-on 'validate-attributes
  "Basic checks of authentication to the Neo4j server.
  This checks whether Restagraph can authenticate to Neo4j.
  This is *not* a check of the user-authentication system for session management."
  ;; Success
  (fiveam:is (restagraph::ensure-db-passwd *server*))
  ;; Failure
  (fiveam:is (null (restagraph::ensure-db-passwd
                     (make-instance 'neo4cl:neo4j-rest-server
                                    :hostname (getf restagraph::*config-vars* :dbhostname)
                                    :dbname (getf restagraph::*config-vars* :dbname)
                                    :dbpasswd "This is not the password"
                                    :dbuser (getf restagraph::*config-vars* :dbusername))))))
