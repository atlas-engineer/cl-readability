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
   (find-method #'child-nodes nil (list (class-of maybe-element)) nil)))

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
  (:documentation "Whether the ELEMENT matches the CSS-SELECTORS.
If element is not `element-p', return NIL."))
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
(defgeneric get-inner-text (node &key normalize-spaces-p)
  (:method ((node t) &key (normalize-spaces-p t))
    (let ((content (trim (inner-text node))))
      (if normalize-spaces-p
          (ppcre:regex-replace-all *normalize-regex* content " ")
          content)))
  (:documentation "Get the inner text of a node.
This also strips out any excess whitespace to be found."))
(defun text-length (element)
  "Small helper to get the content length of ELEMENT."
  (length (get-inner-text element)))
(defgeneric inner-html (element)
  (:documentation "Return the inner HTML of ELEMENT as a plain string."))
(defgeneric parent (element)
  (:documentation "Get a parent of the ELEMENT or NIL."))
(defgeneric child-nodes (element)
  (:documentation "Get a list of ELEMENT children."))
(defgeneric children (element)
  (:method ((element t))
    (remove-if-not #'element-p (child-nodes element)))
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
      (dolist (node (qsa element "a"))
        (cond
          ;; Remove links with javascript: URIs, since they won't work
          ;; after scripts have been removed from the page.
          ((and (attr node "href")
                (uiop:string-prefix-p "javascript:" (attr node "href")))
           (if (zerop (length (child-nodes node)))
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
        ((and (children element)
              (serapeum:single (children element))
              (matches "div, section" (first (children element))))
         (replace-child
          element
          (serapeum:lret ((child (first (children element))))
            (dolist (attr (attrs element))
              (setf (attr child attr) (attr element attr)))
            (simplify-nested-elements (first (child-nodes element))))))))
    (mapc #'simplify-nested-elements (children element)))
  (:documentation "Readability._simplifyNestedElements()."))

(defgeneric get-article-title (element)
  (:method ((element t))
    (let* ((original-title
             (trim (inner-text (qs element "title"))))
           (hierarchical-p nil)
           (current-title
             (cond
               ((test " [\\|\\-\\\\\\/>»] " original-title)
                (setf hierarchical-p
                      (test " [\\\\\\/>»] " original-title))
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

(defgeneric phrasing-content-p (node)
  (:method ((node t))
    (or
     (text-node-p node)
     (smember (name node) *phrasing-elements*)
     (and (smember (name node) '("a" "del" "ins"))
          ;; FIXME: node.childNodes
          (every #'phrasing-content-p (children node)))))
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
                    while (and next (phrasing-content-p next))
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
      (mapc #'clean-styles (children element))))
  (:documentation "

Readability._cleanStyles()."))

(defun video-embed-p (element)
  (or (and (matches element "object, embed, iframe")
           (some (lambda (val)
                   (test *videos-regex* val))
                 (mapcar (alexandria:curry #'attr element) (attrs element))))
      (and (matches element "object")
           (test *videos-regex* (inner-html element)))))

(defgeneric clean (element &rest tags)
  (:method ((element t) &rest tags)
    (dolist (tag tags)
      (dolist (e (qsa element tag))
        (unless (video-embed-p e)
          (remove-child e)))))
  (:documentation "Clean an ELEMENT of all elements of type TAG.
(Unless it's a youtube/vimeo video. People love movies.)

Readability._clean()."))

;; TODO: Refactor?
(defgeneric has-ancestor-tag (node tag-name &key max-depth filter)
  (:method ((node t) tag-name &key (max-depth 3) filter)
    (loop for parent = (parent node) then (parent parent)
          for depth from 1
          until (or (null parent)
                    (and max-depth (> depth max-depth)))
          when (and (string-equal (name parent) tag-name)
                    (or (null filter) (funcall filter parent)))
            do (return t)
          finally (return nil)))
  (:documentation "Check if a given NODE has one of its ancestor TAG-NAME matching the provided one.

Readability._hasAncestorTag()."))

(defgeneric link-density (element)
  (:method ((element t))
    (/ (reduce (lambda (link-length link)
                 (+ link-length
                    (let* ((href (attr link "href"))
                           (hash-p (when href (eql #\# (elt href 0)))))
                      (* (text-length link) (if hash-p 0.3 1)))))
               (qsa element "a") :initial-value 0)
       (text-length element)))
  (:documentation "Get the density of links as a percentage of the content.
This is the amount of text that is inside a link divided by the total
text in the node.

Readability._getLinkDensity()"))

(defgeneric class-weight (element)
  (:method ((element t))
    (if (not *weight-classes*)
        0
        (flet ((bool-mul (boolean &optional (multiplier 1))
                 (if boolean multiplier 0)))
          (+ (bool-mul (not (uiop:emptyp (attr element "class")))
                       (+ (bool-mul (test *positive-regex* (attr element "class")) 25)
                          (bool-mul (test *negative-regex* (attr element "class")) -25)))
             (bool-mul (not (uiop:emptyp (attr element "id")))
                       (+ (bool-mul (test *positive-regex* (attr element "id")) 25)
                          (bool-mul (test *negative-regex* (attr element "id")) -25)))))))
  (:documentation "Get an elements class/id weight.
Uses regular expressions to tell if this element looks good or bad.

Readability._getClassWeight()"))

(defgeneric clean-conditionally (element &rest tags)
  (:method ((element t) &rest tags)
    (when *clean-conditionally*
      (dolist (tag tags)
        (dolist (node (reverse (qsa element tag)))
          (let* ((is-list (or (smember tag '("ul" "ol"))
                              (> (/ (reduce #'+ (mapcar #'text-length (qsa node "ul" "ol")))
                                    (length (inner-text node)))
                                 0.9)))
                 (img (length (qsa node "img")))
                 (p (length (qsa node "p")))
                 (li (- (length (qsa node "li")) 100))
                 (input (length (qsa node "input")))
                 (content-length (text-length node))
                 (heading-density (if (zerop content-length)
                                      0
                                      (/ (reduce
                                          #'+ (mapcar #'text-length (qsa node "h1,h2,h3,h4,h5,h6")))
                                         content-length)))
                 (embeds (qsa node "object, embed, iframe"))
                 (embed-count (count-if-not #'video-embed-p embeds))
                 (video-embeds-p (some #'video-embed-p embeds))
                 (link-density (link-density node))
                 (weight (class-weight node))
                 (have-to-remove (when (< (count #\, (inner-text node)) 10)
                                   (or
                                    (and (> img 1) (< (/ p img) 1/2)
                                         (has-ancestor-tag node "figure"))
                                    (and (not is-list) (> li p))
                                    (> input (floor (/ p 3)))
                                    (and (not is-list)
                                         (< heading-density 0.9)
                                         (< content-length 25)
                                         (or (zerop img) (> img 2))
                                         (has-ancestor-tag node "figure"))
                                    (and (not is-list)
                                         (< weight 25)
                                         (> link-density 0.2))
                                    (and (>= weight 25)
                                         (> link-density 0.5))
                                    (or (> embed-count 1)
                                        (and (= embed-count 1)
                                             (< content-length 75)))))))
            ;; TODO:
            ;; var isDataTable = function(t) {
            ;;   return t._readabilityDataTable;
            ;; };
            ;; if (tag === "table" && isDataTable(node)) {
            ;;   return false;
            ;; }
            ;; if (this._hasAncestorTag(node, "table", -1, isDataTable)) {
            ;;   return false;
            ;; }
            (unless (or (has-ancestor-tag node "code")
                        (not (minusp weight))
                        video-embeds-p
                        (not have-to-remove))
              (remove-child node)))))))
  (:documentation "Clean an ELEMENT of all tags of types TAGS if they look fishy.
\"Fishy\" is an algorithm based on content length, class names, link
density, number of images & embeds, etc.

Readability._cleanConditionally()."))

(defgeneric clean-headers (element)
  (:method ((element t))
    (dolist (heading (qsa element "h1, h2"))
      (when (minusp (class-weight heading))
        (remove-child heading))))
  (:documentation "Clean out spurious headers from an Element.

Readability._cleanHeaders()"))

(defgeneric single-tag-inside-p (element tag)
  (:method ((element t) tag)
    (and (= (length (children element)) 1)
         (string-equal tag (name (first (children element))))
         (not (some (lambda (c)
                      (and (text-node-p c)
                           ;; Readability.REGEXPS.hasContent
                           (test "\\S$" (inner-text c))))
                    (child-nodes element)))))
  (:documentation "Check if this node has only whitespace and a single element with given tag.

Returns false if the DIV node contains non-empty text nodes or if it
contains no element with given tag or more than 1 element.

Readability._hasSingleTagInside()."))

(defgeneric prepare-article (element)
  (:method ((element t))
    (clean-styles element)
    ;; TODO:
    ;; this._markDataTables(articleContent);
    ;; this._fixLazyImages(articleContent);
    (clean-conditionally element "form" "fieldset")
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
    (clean-headers element)
    (clean-conditionally element "table" "ul" "div")
    (dolist (h1 (qsa element "h1"))
      (set-tag-name h1 "h2"))
    (dolist (p (qsa element "p"))
      (when (and (zerop (length (qsa p "img, embed, object, iframe")))
                 (serapeum:blankp (inner-text p)))
        (remove-child p)))
    (dolist (br (qsa element "br"))
      (unless (matches (next-node (next-sibling br)) "p")
        (remove-child br)))
    (dolist (table (qsa element "table"))
      (serapeum:and-let* ((single-tbody (single-tag-inside-p table "tbody"))
                          (tbody (qs table "tbody"))
                          (single-tr (single-tag-inside-p tbody "tr"))
                          (row (qs tbody "tr"))
                          (single-td (single-tag-inside-p row "td"))
                          (cell (qs row "td"))
                          (new-cell (set-tag-name
                                     cell (if (every #'phrasing-content-p (child-nodes cell))
                                              "p" "div"))))
        (replace-child table new-cell))))
  (:documentation "Prepare the article ELEMENT for display.
Clean out any inline styles, iframes, forms, strip extraneous <p> tags, etc.

Readability._prepArticle()."))

(defgeneric unwrap-noscript-images (document)
  (:method ((document t))
    (dolist (image (qsa document "img"))
      (loop named attr-checking
            for attr in (attrs image)
            when (or (smember (attr image attr) '("src" "srcset" "data-src" "data-srcset"))
                     (test *image-regex* (attr image attr)))
              do (return-from attr-checking)
            finally (remove-child image)))
    (dolist (noscript (qsa document "noscript"))
      (labels ((single-image-p (node)
                 "Check if NODE is image.
Or if NODE contains exactly only one image whether as a direct child
or as its descendants.

Readability._isSingleImage()"
                 (or (matches node "img")
                     (and (= 1 (length (children node)))
                          (not (uiop:emptyp (get-inner-text node)))
                          (single-image-p (first (children node)))))))
        (serapeum:and-let* ((_ (single-image-p noscript))
                            (family (children (parent noscript)))
                            (pos (position noscript family))
                            (prev (unless (zerop pos)
                                    (elt family (1- pos))))
                            (_ (single-image-p prev))
                            (prev-img (if (matches prev "img")
                                          prev
                                          (qs prev "img")))
                            (new-img (qs noscript "img")))
          (loop for attr in (attrs prev-img)
                for val = (attr prev-img attr)
                when (and (not (uiop:emptyp val))
                          (or (smember attr '("src" "srcset"))
                              (test *image-regex* val))
                          (not (string-equal val (attr new-img attr))))
                  do (setf (attr new-img
                                 (uiop:strcat (when (attr new-img attr) "data-old-") attr))
                           val)
                finally (progn
                          (replace-child prev (first (children noscript)))))))))
  (:documentation "Find all <noscript> that are located after <img> nodes, and which contain only one <img> element.

Replace the first image with the image from inside the <noscript> tag,
and remove the <noscript> tag. This improves the quality of the images
we use on some sites (e.g. Medium).

Readability._unwrapNoscriptImages()."))

(defgeneric remove-scripts (document)
  (:method ((document t))
    (mapcar #'remove-child (qsa document "script"))
    (mapcar #'remove-child (qsa document "noscript")))
  (:documentation "Removes script tags from the document."))

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
                     (likely-candidate (or (not (test *unlikely-candidate-regex* match-string))
                                           (test *maybe-candidate-regex* match-string)))
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
