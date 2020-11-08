;   Copyright 2020 James Fleming <james@electronic-quill.net>
;
;   Licensed under the GNU General Public License
;   - for details, see LICENSE.txt in the top-level directory


;;; Classes that need to be predefined because of the way SBCL's compiler works

(in-package #:restagraph)

(defclass restagraph-acceptor (tbnl:easy-acceptor)
  ;; Class attributes
  ((datastore :initarg :datastore
              :reader datastore
              :initform (error "Datastore object must be supplied.")
              :documentation "An object representing the datastore, on which the generic functions will be dispatched.")
   (uri-base-api :initarg :uri-base-api
                 :reader uri-base-api
                 :initform "/raw/v1"
                 :documentation "Base URI on which the raw API is to be presented.")
   (uri-base-schema :initarg :uri-base-schema
                    :reader uri-base-schema
                    :initform "/schema/v1"
                    :documentation "Base URI on which the schema API is to be presented.")
   (uri-base-files :initarg :uri-base-files
                   :reader uri-base-files
                   :initform "/files/v1"
                   :documentation "Base URI on which the files API is to be presented.")
   (files-location :initarg :files-location
                   :reader files-location
                   :initform (error "files-location is required")
                   :documentation "Parent directory under which file objects are to be stored.")
   (schema :initarg :schema
           :accessor schema
           :initform (make-schema-hash-table)
           :documentation "What to use for schema actions. Valid options are a hash-table or the datastore object, as methods are specialised on both."))
  ;; Class defaults for initalising the superclass
  (:default-initargs :address "127.0.0.1")
  ;; Class documentation
  (:documentation "Customised Hunchentoot acceptor, subclassed from tbnl:easy-acceptor. Carries additional configuration data for the site."))
