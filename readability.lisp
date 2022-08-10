;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

;;; The API to implement on the side of a certain backend.

(defgeneric qs (root css-selector)
  (:documentation "Select the first element in the ROOT matching the CSS-SELECTOR.

Return a single element node."))
(defgeneric qsa (root css-selector)
  (:documentation "Select all the elements in the ROOT matching the CSS-SELECTOR.

Return a list."))
(defgeneric matches (element css-selector)
  (:documentation "Whether the ELEMENT matches the CSS-SELECTOR."))
(defgeneric name (element)
  (:documentation "Tag name of the element."))
(defgeneric attr (element attribute)
  (:documentation "Get an ATTRIBUTE value from the ELEMENT.

If there's none, return NIL.

If the ELEMENT has no way to get attributes (which usually means it's
not an HTML element), throw `no-attributes-error'.

MUST have a setf-method. Accept any type of data and convert it to
string before setf-ing the value. If the value is NIL, remove the
attribute."))
(defgeneric attrs (element)
  (:documentation "Get a list of attribute names."))
(defgeneric inner-text (element)
  (:documentation "Return the inner text of ELEMENT as a plain non-HTML string."))
(defgeneric parent (element)
  (:documentation "Get a parent of the ELEMENT or NIL."))
(defgeneric children (element)
  (:documentation "Get a list of ELEMENT children.
Only elements are listed."))
(defgeneric next-sibling (element)
  (:documentation "Get next sibling for an ELEMENT or NIL if there's none."))
(defgeneric remove-child (child)
  (:documentation "Remove CHILD from its parent element, effectively removing it from DOM.
In case there's no parent, do nothing."))
(defgeneric replace-child (child replacement)
  (:documentation "Replace the CHILD with REPLACEMENT in its parent.
In case there's no parent and/or REPLACEMENT, do nothing."))
(defgeneric set-tag-name (element tag-name)
  (:documentation "Change the ELEMENT tag name to TAG-NAME.

This usually means replacing the ELEMENT with a newly-created element with TAG-NAME.

Owes a terrible name to Readability._setTagName() method."))
(defgeneric make-text-node (text)
  (:documentation "Make a text node with TEXT content."))

;; Methods that depend on the API and that `is-readerable' and `parse'
;; depend on. Those should work just fine given proper implementation
;; of the API methods above:

(defgeneric without-content-p (element)
  (:method ((element t))
    (and (zerop (length (string-trim serapeum:whitespace (inner-text element))))
         (or (zerop (length (children element)))
             (= (length (children element))
                (+ (length (qsa element "br"))
                   (length (qsa element "hr")))))))
  (:documentation "Whether the element is empty.
Default method checks `inner-text' and element `children'.

A copy of Readability._isElementWithoutContent()."))

(defgeneric post-process-content (element)
  (:method ((element t))
    (fix-relative-urls element)
    (simplify-nested-elements element)
    (unless *keep-classes*
      (clean-classes element)))
  (:documentation "Run any post-process modifications to article content (ELEMENT) as necessary.

Readability._PostProcessContent()."))

;; TODO: Meaningless? Replace with mapcar?
(defgeneric remove-nodes (nodes &optional filter)
  (:method ((nodes t) &optional filter)
    (dolist (node nodes)
      (cond
        ((and filter (funcall filter node))
         (remove-child node))
        (filter nil)
        (t (remove-child node)))))
  (:documentation "Iterate over NODES, call FILTER for each node and remove it if FILTER returned true.

If function is not passed, remove all the nodes in NODES.

Readability._removeNodes()."))

;; TODO: Meaningless? Replace with mapcar?
(defgeneric replace-node-tags (nodes new-tag-name)
  (:method ((nodes t) (new-tag-name string))
    (dolist (node nodes)
      (set-tag-name node new-tag-name)))
  (:documentation "Iterates over NODES, and calls `set-node-tag' for each node with NEW-TAG-NAME.

Readability._replaceNodeTags()."))

(defgeneric get-all-nodes-with-tag (node &rest tag-names)
  (:method ((node t) &rest tag-names)
    (alexandria:mappend (alexandria:curry #'qsa node) tag-names))
  (:documentation "Readability._getAllNodesWithTag()."))

(defgeneric clean-classes (element)
  (:method ((element t))
    (let* ((preserved-classes
             (remove-if (lambda (class) (smember class *preserved-classes*))
                        (uiop:split-string (attr element "class")
                                           :separator serapeum:whitespace)))
           (class (when preserved-classes
                    (format nil "~{~a~^ ~}" preserved-classes))))
      (setf (attr element "class") class)
      (mapc #'clean-classes (children element))))
  (:documentation "Removes the class attribute from every element in the given ELEMENT subtree.

Ignores classes those that match `*preserved-classes*'.

Readability._cleanClasses()."))

(defgeneric fix-relative-urls (element)
  (:method ((element t))
    (flet ((relative->absolute (uri) ;; toAbsoluteURI
             (when (and uri
                        (not (uiop:emptyp (ignore-errors (quri:render-uri (quri:uri uri)))))
                        (not (uiop:string-prefix-p "#" uri)))
               (quri:render-uri
                (quri:merge-uris (quri:uri uri)
                                 (quri:uri *document-url*))))))
      (dolist (node (get-all-nodes-with-tag element "a"))
        (cond
          ;; Remove links with javascript: URIs, since they won't work
          ;; after scripts have been removed from the page.
          ((and (attr node "href")
                (uiop:string-prefix-p "javascript:" (attr node "href")))
           (if (zerop (length (children node)))
               ;; If the link only contains simple text content, it can be converted to a text node.
               (replace-child node (make-text-node (inner-text node)))
               ;; If the link has multiple children, they should all be preserved.
               (set-tag-name node "span")))
          ((attr node "href")
           (setf (attr node "href") (relative->absolute (attr node "href"))))
          (t nil)))
      (dolist (node (get-all-nodes-with-tag
                     element "img" "picture" "figure" "video" "audio" "source"))
        (when (attr node "src")
          (setf (attr node "src") (relative->absolute (attr node "src"))))
        (when (attr node "poster")
          (setf (attr node "poster") (relative->absolute (attr node "poster"))))
        ;; TODO: srcset
        ;;   if (srcset) {
        ;;   var newSrcset = srcset.replace(this.REGEXPS.srcsetUrl, function(_, p1, p2, p3) {
        ;;     return toAbsoluteURI(p1) + (p2 || "") + p3;
        ;;   });

        ;;   media.setAttribute("srcset", newSrcset);
        ;; }
        )))
  (:documentation "Converts each <a> and <img> uri in the given element to an absolute URI, ignoring #ref URIs.

Readability._fixRelativeUris()"))

(defgeneric simplify-nested-elements (element)
  (:method ((element t))
    (when
        (and (parent element)
             (smember (name element) '("div" "section"))
             (not (and (attr element "id")
                       (uiop:string-prefix-p "readability" (attr element "id")))))
      (cond
        ((without-content-p element)
         (remove-child element))
        ((and (children element)
              (serapeum:single (children element))
              (matches "div, section" (first (children element))))
         (replace-child
          element
          (serapeum:lret ((child (first (children element))))
            (dolist (attr (attrs element))
              (setf (attr child attr) (attr element attr)))
            (simplify-nested-elements (first (children element))))))))
    (mapc #'simplify-nested-elements (children element)))
  (:documentation "Readability._simplifyNestedElements()."))

(defgeneric get-article-title (element)
  (:method ((element t))
    (let* ((original-title
             (trim (inner-text (qs "title" element))))
           (hierarchical-p nil)
           (current-title
             (cond
               ((cl-ppcre:scan " [\\|\\-\\\\\\/>»] " original-title)
                (setf hierarchical-p
                      (cl-ppcre:scan " [\\\\\\/>»] " original-title))
                (let* ((parts (cl-ppcre:split " [\\|\\-\\\\\\/>»] " original-title))
                       (first-part (first parts))
                       (second-part (second parts)))
                  (cond
                    ((>= (word-count first-part) 3)
                     first-part)
                    (second-part second-part)
                    (t original-title))))
               ((search ": " original-title)
                (let ((headings (qsa "h1, h2" element)))
                  (or (find-if (lambda (heading)
                                 (string-equal (trim (inner-text heading)) original-title))
                               headings)
                      (let* ((parts (uiop:split-string original-title :separator '(#\:)))
                             (last-part (alexandria:lastcar parts))
                             (first-part (first parts)))
                        (cond
                          ((>= (word-count last-part) 3)
                           last-part)
                          ((<= (word-count first-part) 5)
                           first-part)
                          (t original-title))))))
               ((and (not (< 15 (length original-title) 150))
                     (serapeum:single (qsa "h1" element)))
                (inner-text (qs "h1" element)))
               (t original-title)))
           (current-title
             (cl-ppcre:regex-replace-all *normalize-regex* (trim current-title) " "))
           (title-word-count (word-count current-title)))
      (if (and (<= title-word-count 4)
               (or (not hierarchical-p)
                   (/= title-word-count
                       (1- (word-count (cl-ppcre:regex-replace-all
                                        "[\\|\\-\\\\\\/>»]+" original-title ""))))))
          original-title
          current-title)))
  (:documentation "Get the article title as an H1.

Readability._getArticleTitle()."))

;; The toplevel API.

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
