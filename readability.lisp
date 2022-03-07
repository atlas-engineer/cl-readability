;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defvar *min-content-length* 140
  "The minimum node content length used to decide if the document is readerable.")

(defvar *min-score* 20
  "The minumum cumulated 'score' used to determine if the document is readerable.")

(defvar *unlikely-candidate-regex*
  "-ad-|ai2html|banner|breadcrumbs|combx|comment|community|cover-wrap|disqus|extra|footer|gdpr|header|legends|menu|related|remark|replies|rss|shoutbox|sidebar|skyscraper|social|sponsor|supplemental|ad-break|agegate|pagination|pager|popup|yom-remote")

(defvar *maybe-candidate-regex*
  "and|article|body|column|content|main|shadow")

(defvar *visibility-checker* nil
  "The function used to determine if a node is visible.")

(export-always 'is-readerable)
(defgeneric is-readerable (document)
  (:documentation
   "Decides whether or not the document is reader-able without parsing the whole thing.

Variables that influence the checking:
- `*min-content-length*',
- `*min-score*',
- `*unlikely-candidate-regex*',
- `*maybe-candidate-regex*',
- `*visibility-checker*'."))

(defvar *max-elements* nil
  "Max number of nodes supported by the parser.
Defaults to nil (no limit).")

(defvar *max-top-candidates* 5
  "The number of top candidates to consider when analysing how tight
  the competition is among candidates.")

(defvar *tags-to-score*
  '("SECTION" "H2" "H3" "H4" "H5" "H6" "P" "TD" "PRE")
  "Element tags to score by default.")

(defvar *char-threshold* 500
  "The default number of chars an article must have in order to return a result.")

(define-condition too-many-elements-error (error)
  ((possible-maximum
    :initform *max-elements*
    :accessor possible-maximum
    :initarg :possible-maximum)
   (number-of-elements
    :initform nil
    :accessor number-of-elements
    :initarg :number-of-elements))
  (:report
   (lambda (condition stream)
     (format stream
             "Too many elements in the document: found ~d, expected no more than ~d"
             (number-of-elements condition) (possible-maximum condition))))
  (:documentation "A condition to signal when the number of elements to parse is too large.
See `*max-elements*'."))

(export-always 'parse)
(defgeneric parse (document)
  (:documentation "Parse DOCUMENT and return its readability-enabled version.
Non-destructive."))

(export-always 'nparse)
(defgeneric nparse (document)
  (:documentation "Parse DOCUMENT and return its readability-enabled version.
Possibly modifies the original structure."))
