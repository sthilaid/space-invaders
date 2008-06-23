;;; FILE: "oops-files.scm"
;;; IMPLEMENTS: Implementation file list for Oops
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

;==============================================================;
(define %oops-files
  '( 
     "format"
     "mechanics"
     "slots"
     "instance"
     "class"
     "subclass"
     "setget"
     "class-hierarchy"
     "define-class"
      "methods"
      "generics"
      "describe"
      "limited-types"      ;; depends on generics
      "collection-classes" ;; depends on limited types
      "conditions"         ;; depends on collections (<string>
      "collection-methods" ;; depends on conditions
      "ts-table"   ;; thread-safe-table
;;   "ffi"
) )





;===========================E=O=F==============================;
