;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defun smember (string list-of-string)
  "A frequent case: find a STRING in LIST-OF-STRINGS case-insensitively."
  (member string list-of-string :test #'string-equal))

(defun trim (string)
  (string-trim serapeum:whitespace string))

(defun word-count (string)
  ;; FIXME: Use regexps? Split by other markers? We can do better than
  ;; this, better than Readability.js!
  (length (uiop:split-string string :separator serapeum:whitespace)))

(defun test (regex string)
  (ppcre:scan regex string))
