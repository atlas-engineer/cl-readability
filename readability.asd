;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defsystem readability
  :description ""
  :author "Atlas Engineer LLC"
  :homepage "https://github.com/atlas-engineer/cl-readability"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (cl-ppcre serapeum alexandria)
  :components ((:file "package")
               (:file "globals")
               (:file "conditions")
               (:file "readability")))

(defsystem readability/plump
  :depends-on (readability plump clss quri)
  :components ((:file "readability-plump")))
