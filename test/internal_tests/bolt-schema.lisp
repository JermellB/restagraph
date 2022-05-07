;   Copyright 2022 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;;; Test suite for schema functions which interact with the database

(in-package #:restagraph-test)

(declaim (optimize (compilation-speed 0)
                   (speed 2)
                   (safety 3)
                   (debug 3)))


(fiveam:def-suite
  schema
  :description "Tests for schema functions which interact with the database."
  :in main)

(fiveam:in-suite schema)

(fiveam:test
  schema-versions
  "Can we create and remove schema versions?"
  (let ((session (neo4cl:establish-bolt-session *bolt-server*)))
    ;; Install a new schema version, and get its ID
    (let ((schema-version (restagraph::create-new-schema-version session)))
      (restagraph::log-message :debug (format nil "New schema version: ~D" schema-version))
      ;; Is the current version an integer?
      (fiveam:is (integerp (restagraph::current-schema-version session)))
      ;; Is it the same as the one we were told we just installed?
      (fiveam:is (equal schema-version (restagraph::current-schema-version session)))
      ;; Check whether list-schema-versions works
      (fiveam:is (every
                   #'integerp
                   (cdr
                     (assoc :VERSIONS (restagraph::list-schema-versions session)))))
      ;; Clean up the schema version we installed.
      ;; This action should return nil, so that's what we're testing for.
      (fiveam:is (null (restagraph::delete-schema-version session schema-version))))
    (neo4cl:disconnect session)))

(fiveam:test
  schema-version-rollbacks
  "Basic operations on schema versions"
  (let* ((session (neo4cl:establish-bolt-session *bolt-server*))
         ;; version-zero is to ensure there _is_ a prior version,
         ;; because otherwise some of these tests break when run against a clean database.
         (version-zero (restagraph::create-new-schema-version session))
         (original-version-list (restagraph::list-schema-versions session))
         (original-version (restagraph::current-schema-version session)))
    (restagraph::log-message :debug (format nil ";DEBUG version-list at start of test: ~A" original-version-list))
    ;; Create a new schema version
    (restagraph::log-message :info ";TEST Add a schema version")
    (sleep 2) ;; Version timestamps have 1-second resolution, so prevent collisions
    (restagraph::create-new-schema-version session)
    ;; Do we have one more version than before?
    (fiveam:is (= (+ (length (assoc :VERSIONS original-version-list)) 1)
                  (length (assoc :VERSIONS (restagraph::list-schema-versions session)))))
    (let ((new-current-version (restagraph::current-schema-version session)))
      ;; Is the new current version newer than the previous one?
      (fiveam:is (> new-current-version original-version))
      (restagraph::log-message :info ";TEST Set schema version back and forward")
      ;; Set the version back to the previous one
      (fiveam:is (null (restagraph::set-current-schema-version session original-version)))
      (fiveam:is (= original-version (restagraph::current-schema-version session)))
      ;; Roll forward to the newer one
      (fiveam:is (null (restagraph::set-current-schema-version session new-current-version)))
      (fiveam:is (= new-current-version (restagraph::current-schema-version session)))
      ;; Delete the new version
      (restagraph::log-message :info ";TEST Delete a schema version")
      (fiveam:is (null (restagraph::delete-schema-version session new-current-version)))
      ;; Are we back to the original current-version?
      (restagraph::log-message :info ";TEST Check whether we´re back to the original version")
      (fiveam:is (equal original-version (restagraph::current-schema-version session)))
      (restagraph::log-message
        :info
        ";TEST Check whether we´re back to the original number of versions")
      ;; Are we back to the original number of versions?
      (fiveam:is (= (length (assoc :VERSIONS original-version-list))
                    (length (assoc :VERSIONS (restagraph::list-schema-versions session))))))
    ;; Tidy up
    (restagraph::log-message :info ";INFO Post-test cleanup")
    (when (member version-zero (cdr (assoc :VERSIONS (restagraph::list-schema-versions session))))
      (restagraph::delete-schema-version session version-zero))
    (neo4cl:disconnect session)))

(fiveam:test
  install-core-schema
  "Annoyingly necessary to make the rest work."
  :depends-on 'schema-version-rollbacks
  (let ((session (neo4cl:establish-bolt-session *bolt-server*)))
    (let ((schema-version (restagraph::create-new-schema-version session)))
      ;; Install the core schema in the new schema-version
      (fiveam:is (every #'null (restagraph::install-subschema
                                 session
                                 restagraph::*core-schema* schema-version)))
      ;; Remember to remove it
      (restagraph::delete-schema-version session schema-version))
    (neo4cl:disconnect session)))

;;; New strategy:
;;; - Create a new schema-version.
;;; - Install the core schema into the new version.
;;; - Perform these tests against that.
;;; - Clean up by removing that entire new schema-version.
(fiveam:test
  process-filter
  "Check the filtering of GET query parameters."
  :depends-on 'install-core-schema
  (let* ((session (neo4cl:establish-bolt-session *bolt-server*))
         ;; Create a new schema-version
         (schema-version (restagraph::create-new-schema-version session)))
    ;; Install the core schema
    (restagraph::install-subschema session restagraph::*core-schema* schema-version)
    ;; Install a test-specific resourcetype
    (restagraph::log-message :debug ";TEST: Install EnumTest resourcetype")
    (restagraph::install-subschema-resourcetype
      session
      (restagraph::make-incoming-rtypes
        :name "EnumTest"
        :description "For testing enum values"
        :attributes (list (restagraph::make-incoming-rtype-attrs
                            (list :type "varchar"
                                  :name "carl"
                                  :maxlength 16
                                  :readonly nil
                                  :description "It needed a name, alright?"
                                  :values '("one" "two" "three")))))
      schema-version)
    ;; Fetch the schema into memory
    (restagraph::log-message :debug ";TEST: Fetch the current schema")
    (let ((schema (restagraph::fetch-current-schema session)))
      ;; Now delete the newly-created schema version, because we don't need it any more
      (restagraph::log-message :debug ";TEST: Delete the schema we created.")
      (restagraph::delete-schema-version session schema-version)
      ;; Now disconnect from the server and release the session resources
      (neo4cl:disconnect session)
      ;; On with the tests
      (restagraph::log-message :DEBUG ";TEST: null filter")
      (fiveam:is (null (restagraph::process-filter '() schema "any")))
      (restagraph::log-message :DEBUG ";TEST: regex filter")
      (fiveam:is (equal "n.displayname =~ '.*foo.*'"
                        (restagraph::process-filter '("displayname" . ".*foo.*") schema "People")))
      (restagraph::log-message :DEBUG ";TEST: negated regex filter")
      (fiveam:is (equal "NOT n.displayname =~ '.*foo.*'"
                        (restagraph::process-filter '("displayname" . "!.*foo.*") schema "People")))
      (restagraph::log-message :DEBUG ";TEST: existence filter")
      (fiveam:is (equal "exists(n.displayname)"
                        (restagraph::process-filter '("displayname" . "exists") schema "People")))
      (restagraph::log-message :DEBUG ";TEST: negated existence filter")
      (fiveam:is (equal "NOT exists(n.displayname)"
                        (restagraph::process-filter '("displayname" . "!exists") schema "People")))
      (restagraph::log-message :DEBUG ";TEST: enum filter")
      (fiveam:is (equal "n.carl IN ['one']"
                        (restagraph::process-filter '("carl" . "one") schema "EnumTest")))
      (fiveam:is (equal "n.carl IN ['one', 'three']"
                        (restagraph::process-filter '("carl" . "one,three") schema "EnumTest")))
      ;; This next one _should_ throw some kind of error, because it's not a valid value.
      ;; However, that will have to wait until I figure out how to usefull report on invalid filters.
      (fiveam:is (equal "n.carl IN ['four']"
                        (restagraph::process-filter '("carl" . "four") schema "EnumTest")))
      (restagraph::log-message :DEBUG ";TEST: text filter")
      (fiveam:is (equal "n.displayname = 'blah'"
                        (restagraph::process-filter '("displayname" . "blah") schema "People")))
      (restagraph::log-message :DEBUG ";TEST: negated text filter")
      (fiveam:is (equal "NOT n.displayname = 'blah'"
                        (restagraph::process-filter '("displayname" . "!blah") schema "People")))
      (restagraph::log-message :DEBUG ";TEST: outbound filters")
      (fiveam:is (equal "(n)-[:TAGS]->(:Tags { uid: 'thisTag' })"
                        (restagraph::process-filter '("RGoutbound" . "/TAGS/Tags/thisTag")
                                                    schema
                                                    "People")))
      (fiveam:is (equal "(n)-[:THINGS]->(:Things { uid: 'this' })-[:RELATES_TO]->(:Things { uid: 'that' })"
                        (restagraph::process-filter
                          '("RGoutbound" . "/THINGS/Things/this/RELATES_TO/Things/that")
                          schema
                          "People")))
      (restagraph::log-message :DEBUG ";TEST: negated outbound filters")
      (fiveam:is (equal "NOT (n)-[:TAGS]->(:Tags { uid: 'thisTag' })"
                        (restagraph::process-filter '("RGoutbound" . "!/TAGS/Tags/thisTag")
                                                    schema
                                                    "People")))
      (fiveam:is (equal "NOT (n)-[:THINGS]->(:Things { uid: 'this' })-[:RELATES_TO]->(:Things { uid: 'that' })"
                        (restagraph::process-filter
                          '("RGoutbound" . "!/THINGS/Things/this/RELATES_TO/Things/that")
                          schema
                          "People")))
      (restagraph::log-message :DEBUG ";TEST: inbound filters")
      (fiveam:is (equal "(:Things { uid: 'this' })-[:RELATES_TO]->(n)"
                        (restagraph::process-filter '("RGinbound" . "/Things/this/RELATES_TO")
                                                    schema
                                                    "People")))
      (restagraph::log-message :DEBUG ";TEST: inbound filters with wildcards")
      (fiveam:is (equal "(:Things)-[:RELATES_TO]->(n)"
                        (restagraph::process-filter '("RGinbound" . "/Things/*/RELATES_TO")
                                                    schema
                                                    "People")))
      (restagraph::log-message :DEBUG ";TEST: inbound filters with misplaced wildcards")
      (fiveam:is (equal "(:Things)-[:]->(n)"
                        (restagraph::process-filter '("RGinbound" . "/Things/*/*")
                                                    schema
                                                    "People")))
      (fiveam:is (equal "(:Things { uid: 'that' })-[:]->(n)"
                        (restagraph::process-filter '("RGinbound" . "/Things/that/*")
                                                    schema
                                                    "People")))
      (fiveam:is (equal "(:Things { uid: 'this' })-[:SUBTHINGS]->(:Things { uid: 'that' })-[:RELATES_TO]->(n)"
                        (restagraph::process-filter
                          '("RGinbound" . "/Things/this/SUBTHINGS/Things/that/RELATES_TO")
                          schema
                          "People")))
      (restagraph::log-message :DEBUG ";TEST: negated inbound filters")
      (fiveam:is (equal "NOT (:Things { uid: 'this' })-[:RELATES_TO]->(n)"
                        (restagraph::process-filter '("RGinbound" . "!/Things/this/RELATES_TO")
                                                    schema
                                                    "People")))
      (fiveam:is (equal "NOT (:Things { uid: 'this' })-[:SUBTHINGS]->(:Things { uid: 'that' })-[:RELATES_TO]->(n)"
                        (restagraph::process-filter
                          '("RGinbound" . "!/Things/this/SUBTHINGS/Things/that/RELATES_TO")
                          schema
                          "People"))))))
