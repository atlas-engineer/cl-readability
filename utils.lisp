;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defun smember (string list-of-string)
  "A frequent case: find a STRING in LIST-OF-STRINGS case-insensitively."
  (member string list-of-string :test #'string-equal))
