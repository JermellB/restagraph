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
    :files-temp-location "/tmp/restagraph-files-tmp/"
    :files-location "/tmp/restagraph-files"))

(setf *loglevel* :info)

;; Define the core schema, without which RG won't work properly
(multiple-value-bind (foundp found-system asdf-parent)
  (asdf:locate-system :restagraph)
  ;; The first two values are only defined so we can get at the third one
  (declare (ignore foundp)
           (ignore found-system))
  ;; Force this to be the value of the global/dynamic variable
  (defparameter *core-schemas*
    (mapcar #'(lambda (yaml)
                (cl-yaml:parse (merge-pathnames
                                 yaml
                                 ;; Use ASDF to find out where we are in the filesystem.
                                 (directory-namestring asdf-parent))))
            '(#P"schemas/01_meta.yaml"
              #P"schemas/02_people.yaml"
              #P"schemas/03_files.yaml"))))
