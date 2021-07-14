;   Copyright 2021 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for pure code.
;;;; I.e, functions and methods without side-effects.

(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))

(fiveam:def-suite
  utilities-side-effecting
  :description "Tests for side-effecting functions in utilities.lisp. Tests are named for the functions they're testing."
  :in main)

(fiveam:in-suite utilities-side-effecting)

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


;; FIXME: write these tests
#+(or)
(fiveam:test
  hash-file)

#+(or)
(fiveam:test
  get-file-mime-type)

#+(or)
(fiveam:test
  move-file)

#+(or)
(fiveam:test
  confirm-db-is-running)
