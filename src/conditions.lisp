;   Copyright 2017 James Fleming <james@electronic-quill.net>
;
;   Licensed under the Apache License, Version 2.0 (the "License");
;   you may not use this file except in compliance with the License.
;   You may obtain a copy of the License at
;
;       http://www.apache.org/licenses/LICENSE-2.0
;
;   Unless required by applicable law or agreed to in writing, software
;   distributed under the License is distributed on an "AS IS" BASIS,
;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;   See the License for the specific language governing permissions and
;   limitations under the License.

(in-package #:restagraph)

(define-condition integrity-error (error)
  ((message :initarg :message
            :reader message))
  (:documentation "Signals an attempt to do something that would violate the integrity model of restagraph and, by implication, the application built on it."))

(define-condition client-error (error)
  ((message :initarg :message
            :reader message))
  (:documentation "The client made an invalid request, e.g. required parameters were missing."))
