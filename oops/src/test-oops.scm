;;; FILE: "test-oops.scm"
;;; IMPLEMENTS: Unit tests driver for Oops
;;; LANGUAGE: Gambit Scheme (v4 beta12)
;;; AUTHOR: Ken Dickey
;;;
;;; COPYRIGHT (c) 2005 by Kenneth Alan Dickey
;;;
;;;   Licensed under the Apache License, Version 2.0 (the "License");
;;;   you may not use this file except in compliance with the License.
;;;   You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;   Unless required by applicable law or agreed to in writing, software
;;;   distributed under the License is distributed on an "AS IS" BASIS,
;;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;   See the License for the specific language governing permissions and
;;;   limitations under the License.

;; Invoke via:
;;        (include "test-oops.scm")
;==============================================================;
(define oops-dir "../")
(define join string-append)
(load (join oops-dir "src/" "oops"))
;==============================================================;
(load (join oops-dir "test/" "testing"))

;(verbose? #t)
;(break-on-error? #t)

;; Nota Bene: order dependent initializations!
(load (join oops-dir "test/" "test-slots"))
(load (join oops-dir "test/" "test-class"))
(load (join oops-dir "test/" "test-generics")) ;; requires "test-class" be loaded 1st
(load (join oops-dir "test/" "test-collections"))
(load (join oops-dir "test/" "test-types"))

(run-all-tests)

;===========================E=O=F==============================;
