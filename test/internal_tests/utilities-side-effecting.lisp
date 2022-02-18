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
  confirm-db-is-running
  "Can we even log into the thing?"
  (fiveam:is (restagraph::confirm-db-is-running *bolt-server*)))


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
