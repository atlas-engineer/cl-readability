;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

;;; The API to implement on the side of a certain backend.

(defun element-p (maybe-element)
  ;; The minimal set of methods to implement for a working API:
  (and
   (not (text-node-p maybe-element))
   (find-method #'qsa nil (list (class-of maybe-element)) nil)
   (find-method #'name nil (list (class-of maybe-element)) nil)
   (find-method #'attr nil (list (class-of maybe-element)) nil)
   (find-method #'attrs nil (list (class-of maybe-element)) nil)
   (find-method #'parent nil (list (class-of maybe-element)) nil)
   (find-method #'children nil (list (class-of maybe-element)) nil)))

(defgeneric text-node-p (node)
  (:documentation "Whether the NODE is a text node."))
;; CSS-SELECTOR is optional to exclude it from specializable methods.
(defgeneric qs (root &optional css-selector)
  (:method ((root t) &optional css-selector)
    (first (qsa root css-selector)))
  (:documentation "Select the first element in the ROOT matching the CSS-SELECTOR.

Return a single element node. If CSS-SELECTOR is NIL, return the root itself."))
(defgeneric qsa (root &rest css-selectors)
  (:documentation "Select all the elements in the ROOT matching the CSS-SELECTORS.

Return a list of matching elements. If CSS-SELECTORS list is empty, return all
the sub-elements of the ROOT."))
(defgeneric matches (element &rest css-selectors)
  (:method ((element t) &rest css-selectors)
    (when element
      (find element (apply #'qsa (parent element) css-selectors))))
  (:documentation "Whether the ELEMENT matches the CSS-SELECTORS."))
(defgeneric name (element)
  (:documentation "Tag name of the element.
Should return NIL is there's no name or the element is malformed."))
(defgeneric attr (element &optional attribute)
  (:documentation "Get an ATTRIBUTE value from the ELEMENT.

If there's none or if ATTRIBUTE is NIL, return NIL.

If the ELEMENT has no way to get attributes (which usually means it's
not an HTML element), throw `no-attributes-error'.

MUST have a setf-method. Accept any type of data and convert it to
string before setf-ing the value. If the value is NIL, remove the
attribute."))
(defgeneric attrs (element)
  (:documentation "Get a list of attribute names."))
(defgeneric inner-text (element)
  (:documentation "Return the inner text of ELEMENT as a plain non-HTML string."))
(defgeneric inner-html (element)
  (:documentation "Return the inner HTML of ELEMENT as a plain string."))
(defgeneric parent (element)
  (:documentation "Get a parent of the ELEMENT or NIL."))
(defgeneric children (element)
  (:documentation "Get a list of ELEMENT children."))
(defgeneric children-elements (element)
  (:method ((element t))
    (remove-if-not #'element-p (children element)))
  (:documentation "Get a list of ELEMENT children elements."))
(defgeneric next-sibling (node)
  (:documentation "Get next sibling node for a NODE or NIL if there's none.
SHOULD return NIL is NODE is NIL."))
(defgeneric next-node (node)
  (:method ((node t))
    (loop for next = node then (next-sibling next)
          while (and next
                     (not (element-p next))
                     (every #'serapeum:whitespacep (inner-text next)))
          finally (return next)))
  (:documentation "Finds the next node, starting from the given NODE.
Ignores whitespace in between.
If the given node is an element, the same node is returned."))
(defgeneric next-sibling-element (element)
  (:documentation "Get next sibling element for an ELEMENT or NIL if there's none.
SHOULD return NIL is ELEMENT is NIL."))
(defgeneric append-child (parent child)
  (:documentation "Append CHILD to the end of PARENT children list."))
(defgeneric remove-child (child)
  (:documentation "Remove CHILD from its parent element, effectively removing it from DOM.
In case there's no parent, do nothing."))
(defgeneric replace-child (child replacement)
  (:documentation "Replace the CHILD with REPLACEMENT in its parent.
In case there's no parent and/or REPLACEMENT, do nothing."))
(defgeneric set-tag-name (element tag-name)
  (:documentation "Change the ELEMENT tag name to TAG-NAME.

This usually means replacing the ELEMENT with a newly-created element with TAG-NAME.

Return the element with a new name, even if new.

Owes a terrible name to Readability._setTagName() method."))
(defgeneric make-text-node (text)
  (:documentation "Make a text node with TEXT content."))

;; Methods that depend on the API and that `is-readerable' and `parse'
;; depend on. Those should work just fine given proper implementation
;; of the API methods above:

(defgeneric without-content-p (element)
  (:method ((element t))
    (and (zerop (length (string-trim serapeum:whitespace (inner-text element))))
         (or (zerop (length (children-elements element)))
             (= (length (children-elements element))
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

(defgeneric clean-classes (element)
  (:method ((element t))
    (let* ((preserved-classes
             (remove-if (lambda (class) (smember class *preserved-classes*))
                        (uiop:split-string (attr element "class")
                                           :separator serapeum:whitespace)))
           (class (when preserved-classes
                    (format nil "~{~a~^ ~}" preserved-classes))))
      (setf (attr element "class") class)
      (mapc #'clean-classes (children-elements element))))
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
      (dolist (node (qsa element "a"))
        (cond
          ;; Remove links with javascript: URIs, since they won't work
          ;; after scripts have been removed from the page.
          ((and (attr node "href")
                (uiop:string-prefix-p "javascript:" (attr node "href")))
           (if (zerop (length (children-elements node)))
               ;; If the link only contains simple text content, it can be converted to a text node.
               (replace-child node (make-text-node (inner-text node)))
               ;; If the link has multiple children, they should all be preserved.
               (set-tag-name node "span")))
          ((attr node "href")
           (setf (attr node "href") (relative->absolute (attr node "href"))))
          (t nil)))
      (dolist (node (qsa element "img" "picture" "figure" "video" "audio" "source"))
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
        ((and (children-elements element)
              (serapeum:single (children-elements element))
              (matches "div, section" (first (children-elements element))))
         (replace-child
          element
          (serapeum:lret ((child (first (children-elements element))))
            (dolist (attr (attrs element))
              (setf (attr child attr) (attr element attr)))
            (simplify-nested-elements (first (children element))))))))
    (mapc #'simplify-nested-elements (children element)))
  (:documentation "Readability._simplifyNestedElements()."))

(defgeneric get-article-title (element)
  (:method ((element t))
    (let* ((original-title
             (trim (inner-text (qs element "title"))))
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
                (let ((headings (qsa element "h1, h2")))
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
                     (serapeum:single (qsa element "h1")))
                (inner-text (qs element "h1")))
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

(defgeneric phrasing-context-p (node)
  (:method ((node t))
    (or
     (text-node-p node)
     (smember (name node) *phrasing-elements*)
     (and (smember (name node) '("a" "del" "ins"))
          ;; FIXME: node.childNodes
          (every #'phrasing-context-p (children node)))))
  (:documentation "Determine if a node qualifies as phrasing content.
https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Content_categories#Phrasing_content

Readability._isPhrasingContent()."))

(defgeneric whitespace-p (node)
  (:method ((node t))
    (or (and (text-node-p node) (every #'serapeum:whitespacep (inner-text node)))
        (and (element-p node) (matches node "br"))))
  (:documentation "Readability._isWhitespace()."))

(defgeneric replace-brs (element)
  (:method ((element t))
    (dolist (br (qsa element "br"))
      (when (parent br)
        (let ((replaced-p nil))
          (loop for next = (next-node (next-sibling br))
                  then (next-node br-sibling)
                for br-sibling = (next-sibling next)
                while (and next (matches next "br"))
                do (remove-child next)
                do (setf replaced-p t))
          (when replaced-p
            (let ((p (set-tag-name br "p")))
              (loop for next = (next-sibling p) then next-sibling
                    for next-sibling = (next-sibling next)
                    while (and next (phrasing-context-p next))
                    do (remove-child next)
                    do (append-child p next)
                    when (and (matches next "br")
                              (matches (next-node (next-sibling next)) "br"))
                      do (return))
              (loop for last-child = (alexandria:lastcar (children p))
                    while (and last-child (whitespace-p last-child))
                    do (remove-child last-child))
              (when (matches (parent p) "p")
                (set-tag-name ))))))))
  (:documentation "Replaces 2 or more successive <br> elements with a single <p>.
Whitespace between <br> elements are ignored. For example:

 <div>foo<br>bar<br> <br><br>abc</div>

will become:

 <div>foo<br>bar<p>abc</p></div>

Readability._replaceBrs()."))

(defgeneric prepare-document (element)
  (:method ((element t))
    (mapc #'remove-child (qsa element "style"))
    (replace-brs element)
    (dolist (font (qsa element "font" ))
      (set-tag-name font "span")))
  (:documentation "Prepare the HTML document for readability to scrape it.
This includes things like stripping javascript, CSS, and handling terrible markup."))

(defgeneric clean-styles (element)
  (:method ((element t))
    (unless (or (null element)
                (matches element "svg"))
      (dolist (attr *presentation-attributes*)
        (setf (attr element attr) nil))
      (when (smember (name element) *deprecated-size-attribute-names*)
        (setf (attr element "width") nil
              (attr element "height") nil))
      (mapc #'clean-styles (children-elements element))))
  (:documentation "

Readability._cleanStyles()."))

(defgeneric clean (element &rest tags)
  (:method ((element t) &rest tags)
    (dolist (tag tags)
      (dolist (e (qsa element tag))
        (unless (or (and (matches e "object, embed, iframe")
                         (some (lambda (val)
                                 (cl-ppcre:scan *videos-regex* val))
                               (mapcar (alexandria:curry #'attr e) (attrs e))))
                    (and (matches e "object")
                         (cl-ppcre:scan *videos-regex* (inner-html e))))
          (remove-child e)))))
  (:documentation "Clean an ELEMENT of all elements of type TAG.
(Unless it's a youtube/vimeo video. People love movies.)

Readability._clean()."))

(defgeneric prepare-article (element)
  (:method ((element t))
    (clean-styles element)
    ;; TODO:
    ;; this._markDataTables(articleContent);
    ;; this._fixLazyImages(articleContent);
    ;; this._cleanConditionally(articleContent, "form");
    ;; this._cleanConditionally(articleContent, "fieldset");
    (clean element "object" "embed" "footer" "link" "aside")
    ;; TODO:
    ;;     // Clean out elements with little content that have "share" in their id/class combinations from final top candidates,
    ;; // which means we don't remove the top candidates even they have "share".
    ;; var shareElementThreshold = this.DEFAULT_CHAR_THRESHOLD;
    ;; this._forEachNode(articleContent.children, function (topCandidate) {
    ;;   this._cleanMatchedNodes(topCandidate, function (node, matchString) {
    ;;     return this.REGEXPS.shareElements.test(matchString) && node.textContent.length < shareElementThreshold;
    ;;   });
    ;; });
    (clean element "iframe" "input" "textarea" "select" "button")
    ;; All the rest of it
    )
  (:documentation "Prepare the article ELEMENT for display.
Clean out any inline styles, iframes, forms, strip extraneous <p> tags, etc.

Readability._prepArticle()."))

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
