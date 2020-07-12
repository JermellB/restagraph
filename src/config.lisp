;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory

;;;; Configs for the server to use

(in-package #:restagraph)

(defparameter *config-vars*
  `(:listen-address "localhost"
    :listen-port 4950
    :dbhostname "localhost"
    :dbport 7474
    :dbname "neo4j"
    :dbusername "neo4j"
    :dbpasswd "wallaby"
    :api-uri-base "/raw/v1"
    :schema-uri-base "/schema/v1"
    :files-uri-base "/files/v1"
    :files-temp-location "/tmp/restagraph-files-tmp"
    :files-location "/tmp/restagraph-files"))

(setf *loglevel* :info)
