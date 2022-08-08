;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defgeneric qs (root css-selector)
  (:documentation "Select the first element in the ROOT matching the CSS-SELECTOR.

Return a single element node."))
(defgeneric qsa (root css-selector)
  (:documentation "Select all the elements in the ROOT matching the CSS-SELECTOR.

Return a list."))
(defgeneric attr (element attribute)
  ;; TODO: add a condition class for attribute not applicable error.
  (:documentation "Get an attribute value from the element.

If there's none, return NIL.

MUST have a setf-method."))
(defgeneric matches (element css-selector)
  (:documentation "Whether the ELEMENT matches the CSS-SELECTOR."))
(defgeneric inner-text (element)
  (:documentation "Return the inner text of ELEMENT as a plain non-HTML string."))
(defgeneric remove-child (child)
  (:documentation "Remove CHILD from its parent element, effectively removing it from DOM.
In case there's no parent, do nothing."))

(export-always 'is-readerable)
(defgeneric is-readerable (document)
  (:method ((root t))
    "Default method doing the most sensible readability checking possible.

Relies on the `qsa', `attr', `matches', and `inner-text' generics
being defined for a certain back-end."
    (let ((nodes (qsa root "p, pre, article, div > br")))
      (loop with max-score = 0
            for node in nodes
            for score
              = (serapeum:and-let*
                    ((match-string (uiop:strcat (attr node "class") " " (attr node "id")))
                     (visible-p (funcall *visibility-checker* node))
                     (likely-candidate (or (not (cl-ppcre:scan *unlikely-candidate-regex* match-string))
                                           (cl-ppcre:scan *maybe-candidate-regex* match-string)))
                     (not-a-li (not (matches node "li p")))
                     (text-content (string-trim serapeum:whitespace (inner-text node)))
                     (text-content-length (length text-content))
                     (length-sufficient (>= text-content-length *min-content-length*)))
                  (sqrt (- text-content-length *min-content-length*)))
            when score
              do (incf max-score score)
            when (> max-score *min-score*)
              do (return t)
            finally (return nil))))
  (:documentation
   "Decides whether or not the document is reader-able without parsing the whole thing.

Variables that influence the checking:
- `*min-content-length*',
- `*min-score*',
- `*unlikely-candidate-regex*',
- `*maybe-candidate-regex*',
- `*visibility-checker*'."))

(export-always 'parse)
(defgeneric parse (document url)
  (:documentation "Parse DOCUMENT (from URL) and return its readability-enabled version.
Non-destructive."))

(export-always 'nparse)
(defgeneric nparse (document url)
  (:documentation "Parse DOCUMENT (from URL) and return its readability-enabled version.
Possibly modifies the original structure."))
