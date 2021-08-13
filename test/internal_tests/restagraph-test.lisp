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


(defparameter *server*
  (make-instance 'neo4cl:neo4j-rest-server
                 :hostname (getf restagraph::*config-vars* :dbhostname)
                 :dbname (getf restagraph::*config-vars* :dbname)
                 :dbuser (getf restagraph::*config-vars* :dbusername)
                 :dbpasswd (getf restagraph::*config-vars* :dbpasswd)))
