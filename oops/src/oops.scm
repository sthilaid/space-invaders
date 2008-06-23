;;; FILE: "oops.scm"
;;; IMPLEMENTS: load oops for Oops
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

;; Load this file into Gambit via include, i.e.
;;    (load "oops.scm")
;;    (include "oops-macros.scm")
;; or
;;    (include "oops.scm")

;==============================================================;
(include "common-macros.scm")
;==============================================================;
(define debug-oops #f)

(define oops-info
  '( (name . "Oops")
     (status . pre-alpha/incomplete-prototype)
     (version . -1)
     (compatability . 0) ;; incremented w each incompatable release
     (copyright .
"COPYRIGHT (c) 2005 by Kenneth Alan Dickey

  Licensed under the Apache License, Version 2.0 (the \"License\");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an \"AS IS\" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.")
) )

(include "oops-files.scm")
(include "oops-macros.scm")

(define load-fun
  (if debug-oops
      (lambda (file)
        (newline) (display "Loading: \"")
        (display file)
        (display "\"")
        (display (string-append "\n" "Loaded:  \"" (load file) "\""))
        (newline)
        )
      load))
        
(for-each load-fun %oops-files)

(newline)
(display "Loaded Oops!")
(newline)

'Oops

;===========================E=O=F==============================;
