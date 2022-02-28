;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:cl-readability)

(defvar *default-min-content-length* 140)

(defvar *default-min-score* 20)

(export-always 'is-readerable)
(defgeneric is-readerable (document &key min-content-length min-score visibility-checker)
  (:method :around (document
                    &key (min-content-length *default-min-content-length*)
                    min-score visibility-checker)
    (check-type min-content-length (or null integer) "an optional integer")
    (check-type min-score (or null integer) "an optional integer")
    (check-type visibility-checker (or null function))
    (the (values boolean &optional) (call-next-method)))
  (:documentation
   "Decides whether or not the document is reader-able without parsing the whole thing.

Arguments:
- MIN-CONTENT-LENGTH -- The minimum node content length used to decide
  if the document is readerable. Defaults to `*default-min-content-length*'.
- MIN-SCORE -- The minumum cumulated 'score' used to determine if the
  document is readerable. Defaults to `*default-min-score*'.
- VISIBILITY-CHECKER -- The function used to determine if a node is
  visible."))

(defvar *default-max-elements* nil
  "Max number of nodes supported by the parser.
Defaults to nil (no limit).")

(defvar *default-max-top-candidates* 5
  "The number of top candidates to consider when analysing how tight
  the competition is among candidates.")

(defvar *default-tags-to-score*
  '("SECTION" "H2" "H3" "H4" "H5" "H6" "P" "TD" "PRE")
  "Element tags to score by default.")

(defvar *default-char-threshold* 500
  "The default number of chars an article must have in order to return a result.")

(defvar *default-unlikely-candidate-regex*
  "-ad-|ai2html|banner|breadcrumbs|combx|comment|community|cover-wrap|disqus|extra|footer|gdpr|header|legends|menu|related|remark|replies|rss|shoutbox|sidebar|skyscraper|social|sponsor|supplemental|ad-break|agegate|pagination|pager|popup|yom-remote")

(defvar *default-maybe-candidate-regex*
  "and|article|body|column|content|main|shadow")

(export-always 'parse)
(defgeneric parse (document
                   &key max-elements max-top-candidates
                     tags-to-score char-threshold
                     unlikely-candidate-regex
                     maybe-candidate-regex)
  (:method :around (document
                    &key (max-elements *default-max-elements*)
                      (max-top-candidates *default-max-top-candidates*)
                      (tags-to-score *default-tags-to-score*)
                      (char-threshold *default-char-threshold*)
                      (unlikely-candidate-regex *default-unlikely-candidate-regex*)
                      (maybe-candidate-regex *default-maybe-candidate-regex*))
    (check-type max-elements (or null integer) "an optional integer")
    (check-type max-top-candidates integer)
    (check-type tags-to-score list "a list of strings")
    (check-type char-threshold integer)
    (check-type unlikely-candidate-regex string)
    (check-type maybe-candidate-regex string)
    (call-next-method))
  (:documentation "Parse DOCUMENT and return its readability-enabled version.

Arguments are:

- MAX-ELEMENTS -- maximum number of elements to even try parsing. If
  the page has more elements, give up on making it readable. An optional integer.
- MAX-TOP-CANDIDATES -- The number of top candidates to consider when
  analysing how tight the competition is among candidates. An integer.
- TAGS-TO-SCORE -- Element tags to score by default. A list of uppercase strings.
- CHAR-THRESHOLD -- The default number of chars an article must have
  in order to return a result. An integer.
- UNLIKELY-CANDIDATE-REGEX -- A regex string for what is not likely to
  be article content.
- MAYBE-CANDIDATE-REGEX -- a regex string for possible article content tags."))

(export-always 'nparse)
(defgeneric nparse (document
                    &key max-elements max-top-candidates
                      tags-to-score char-threshold
                      unlikely-candidate-regex
                      maybe-candidate-regex)
  (:method :around (document
                    &key (max-elements *default-max-elements*)
                      (max-top-candidates *default-max-top-candidates*)
                      (tags-to-score *default-tags-to-score*)
                      (char-threshold *default-char-threshold*)
                      (unlikely-candidate-regex *default-unlikely-candidate-regex*)
                      (maybe-candidate-regex *default-maybe-candidate-regex*))
    (check-type max-elements (or null integer) "an optional integer")
    (check-type max-top-candidates integer)
    (check-type tags-to-score list "a list of strings")
    (check-type char-threshold integer)
    (check-type unlikely-candidate-regex string)
    (check-type maybe-candidate-regex string)
    (call-next-method))
  (:documentation "A destructive version of `parse'.
See `parse' for the argument description."))
