;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defun smember (string list-of-string)
  "A frequent case: find a STRING in LIST-OF-STRINGS case-insensitively."
  (member string list-of-string :test #'string-equal))

(defmethod node-visible-p ((node plump:element))
  ;; TODO: (!node.style || node.style.display != "none")
  (and (not (plump:has-attribute node "hidden"))
       ;; TODO: (node.className && node.className.indexOf &&
       ;; node.className.indexOf("fallback-image") !== -1
       (or (not (plump:has-attribute node "aria-hidden"))
           (not (string-equal (plump:get-attribute node "aria-hidden") "true")))))

(defmethod remove-child* ((node plump:child-node))
  (when (plump:parent node)
    (plump:remove-child node)))

(defmethod is-readerable ((document plump:root))
  (let* ((*visibility-checker* #'node-visible-p)
         (nodes (clss:select "p, pre, article, div > br" document)))
    (loop with max-score = 0
          for node across nodes
          for score
            = (serapeum:and-let*
                  ((match-string (uiop:strcat (plump:get-attribute node "class") " "
                                              (plump:get-attribute node "id")))
                   (visible-p (funcall *visibility-checker* node))
                   (likely-candidate (or (not (cl-ppcre:scan *unlikely-candidate-regex* match-string))
                                         (cl-ppcre:scan *maybe-candidate-regex* match-string)))
                   (not-a-li (not (clss:node-matches-p "li p" node)))
                   (text-content (string-trim serapeum:whitespace (plump:text node)))
                   (text-content-length (length text-content))
                   (length-sufficient (>= text-content-length *min-content-length*)))
                (sqrt (- text-content-length *min-content-length*)))
          when score
            do (incf max-score score)
          when (> max-score *min-score*)
            do (return t)
          finally (return nil))))

;;; XXX Readability._unwrapNoscriptImages()
(defmethod unwrap-noscript-images ((document plump:nesting-node))
  (loop for image across (clss:select "img" document)
        do (loop named attr-checking
                 for attr being the hash-key of (plump:attributes image)
                   using (hash-value attr-value)
                 when (or (smember attr '("src" "srcset" "data-src" "data-srcset"))
                          (cl-ppcre:scan "\\.(jpg|jpeg|png|webp)" attr-value))
                   do (return-from attr-checking)
                 finally (plump:remove-child image)))
  ;; TODO: noscript cleaning.
  )

;; XXX: Readability._setTagName()
(defmethod replace-with-tag ((node plump:node) tag-name)
  (plump:replace-child
   node (apply #'make-instance
               (type-of node)
               :tag-name tag-name
               :parent nil
               (when (plump:nesting-node-p node)
                 (list :children (plump:children node))))))

;; XXX: A free rewrite of Readability._cleanClasses()
(defmethod normalize-classes ((node plump:node))
  ;; TODO: Classes to preserve.
  (when (plump:element-p node)
    (plump:remove-attribute node "class")
    (loop for child across (plump:children node)
          do (normalize-classes child))))

;;; XXX: Readability._fixRelativeUris
(defmethod fix-relative-urls ((node plump:nesting-node) absolute-uri)
  (flet ((relative->absolute (uri)
           (when (and uri (not (uiop:emptyp (ignore-errors (quri:render-uri (quri:uri uri))))))
             (quri:render-uri (quri:merge-uris (quri:uri uri) (quri:uri absolute-uri))))))
    (loop for elem across (clss:select "a, img, picture, figure, video, audio, source" node)
          for href =  (plump:get-attribute elem "href")
          for src = (plump:get-attribute elem "src")
          for poster = (plump:get-attribute elem "poster")
          ;; TODO: srcset
          unless (uiop:emptyp src)
            do (plump:set-attribute elem "src" (relative->absolute src))
          unless (uiop:emptyp poster)
            do (plump:set-attribute elem "poster" (relative->absolute poster))
          when (and href (string= "javascript:" (quri:uri-scheme (quri:uri href))))
            ;; TODO: if the link only contains simple text content, it
            ;; can be converted to a text node
            ;;
            ;; Replace the link with a span to preserve children.
            do (replace-with-tag elem "span")
          else unless (uiop:emptyp href)
                 do (plump:set-attribute elem "href" (relative->absolute href)))))

;;; XXX: Readability._getNextNode()
(defmethod get-next-node ((node plump:node) &optional ignore-self-and-kids)
  (declare (ignore ignore-self-and-kids))
  (plump:next-element node))

;;; XXX: Readability._getNextNode()
(defmethod get-next-node ((element plump:nesting-node) &optional ignore-self-and-kids)
  (if ignore-self-and-kids
      (loop for node = (plump:parent element) then (plump:parent node)
            until (and node (not (plump:next-element node)))
            finally (return (and node (plump:next-element node))))
      (loop for child across (plump:children element)
            when (plump:element-p child)
              do (return child))))

;;; XXX: Readability._checkByline()
(defmethod get-byline ((element plump:element) match-string)
  (let ((rel (plump:get-attribute element "rel"))
        (itemprop (plump:get-attribute element "itemprop")))
    (if (or (equal rel "author")
            (and itemprop (search "author" itemprop))
            (and (cl-ppcre:scan "byline|author|dateline|writtenby|p-author" match-string)
                 (> 100 (length (string-trim serapeum:whitespace (plump:text element))) 0)))
        (string-trim serapeum:whitespace (plump:text element)))))

;;; XXX: Readability._cleanStyles()
(defmethod clean-styles ((element plump:element))
  ;; Readability.REGEXPS.PRESENTATIONAL_ATTRIBUTES
  (dolist (attr (list "align" "background" "bgcolor" "border" "cellpadding" "cellspacing" "frame"
                      "hspace" "rules" "style" "valign" "vspace" "width" "height"))
    (plump:remove-attribute element attr))
  (loop for child across (plump:child-elements element)
        do (clean-styles child)))

;;; XXX: Readability._getClassWeight()
(defmethod get-class-weight ((element plump:element))
  ;; Readability.REGEXPS.positive
  (let ((positive "article|body|content|entry|hentry|h-entry|main|page|pagination|post|text|blog|story")
        ;; Readability.REGEXPS.positive
        (negative "-ad-|hidden|^hid$| hid$| hid |^hid |banner|combx|comment|com-|contact|foot|footer|footnote|gdpr|masthead|media|meta|outbrain|promo|related|scroll|share|shoutbox|sidebar|skyscraper|sponsor|shopping|tags|tool|widget"))
    (flet ((bool-mul (boolean &optional (multiplier 1))
             (if boolean multiplier 0)))
      (+ (bool-mul
          (unless (uiop:emptyp (plump:get-attribute element "class"))
            (+ (bool-mul (cl-ppcre:scan positive (plump:get-attribute element "class")) 25)
               (bool-mul (cl-ppcre:scan negative (plump:get-attribute element "class")) -25))))
         (bool-mul
          (unless (uiop:emptyp (plump:get-attribute element "id"))
            (+ (bool-mul (cl-ppcre:scan positive (plump:get-attribute element "id")) 25)
               (bool-mul (cl-ppcre:scan negative (plump:get-attribute element "id")) -25))))))))

;;; XXX: Readability._isElementWithoutContent()
(defmethod without-content-p ((element plump:element))
  (and (zerop (length (string-trim serapeum:whitespace (plump:text element))))
       (or (zerop (length (plump:children element)))
           (= (length (plump:children element))
              (+ (length (clss:select "br" element))
                 (length (clss:select "hr" element)))))))

;;; XXX: Readability._simplifyNestedElements
(defmethod simplify-nested-elements ((element plump:nesting-node))
  (loop for node = element then (get-next-node node)
        for parent = (ignore-errors (plump:parent node))
        while node
        when (and (clss:node-matches-p "div, section" node)
                  (without-content-p node))
          do (plump:remove-child node)
        else when (and (serapeum:single (plump:children node))
                       (plump:element-p (elt (plump:children node) 0)))
               do (plump:replace-child
                   node (serapeum:lret ((child (elt (plump:children node) 0)))
                          (setf (slot-value child 'plump-dom::%attributes)
                                (plump:attributes node))))))

;;; XXX: Readability._getArticleTitle()
(defmethod get-article-title ((element plump:element))
  (let* ((original-title (elt (clss:select "title" element) 0))
         (title-parts (cl-ppcre:split " [\\|\\-\\\\\\/>»:] " original-title)))
    ;; TODO: Parse colon-delimited titles properly. Requires some refactoring.
    (cond
      ((> 5 (length (serapeum:tokens (alexandria:lastcar title-parts))) 3)
       (alexandria:lastcar title-parts))
      ((> 5 (length (serapeum:tokens (first title-parts))) 3)
       (first title-parts))
      ((serapeum:single (clss:select "h1" element))
       (plump:text (elt (clss:select "h1" element) 0)))
      (t original-title))))

;; TODO: Replace with the cleaning loops and BR cleaning call?
(defmethod prepare-document ((element plump:nesting-node))
  (loop for style across (clss:select "style" element)
        do (plump:remove-child style))
  ;; Remove BRs
  (loop for font across (clss:select "font" element)
        do (replace-with-tag font "span")))

(defmethod recursive-parents ((node plump:child-node) &key (max-depth 3))
  (labels ((parents (node depth)
             (when (and (plusp depth)
                        (< depth max-depth)
                        (plump:child-node-p node)
                        (plump:parent node))
               (cons node (parents (plump:parent node) (1+ depth))))))
    (parents node 0)))

(defmethod has-ancestor-tag ((node plump:child-node) (tag-name string)
                             &key (max-depth 3) filter-fn)
  (some (lambda (ancestor)
          (and (string-equal tag-name (plump:tag-name ancestor))
               (or (not filter-fn)
                   (funcall filter-fn ancestor))))
        (recursive-parents node :max-depth max-depth)))

(defmethod phrasing-content-p ((node plump:element))
  (or (smember (plump:tag-name node) *phrasing-elements*)
      (and (smember (plump:tag-name node) '("a" "del" "ins"))
           (every #'phrasing-content-p (plump:children node)))))

(defmethod phrasing-content-p ((node plump:text-node))
  t)

(defmethod phrasing-content-p ((node plump:comment))
  (remove-child* node)
  nil)

(defmethod whitespace-node-p ((node plump:text-node))
  (zerop (length (string-trim serapeum:whitespace (plump:text node)))))

(defmethod whitespace-node-p ((node plump:comment))
  (remove-child* node)
  t)

(defmethod whitespace-node-p ((node plump:element))
  (string-equal "br" (plump:tag-name node)))

(defmethod single-tag-inside-p ((node plump:element) (tag-name string))
  (serapeum:and-let* ((children (serapeum:filter #'plump:element-p (plump:children node)))
                      (one-child (= 1 (length children)))
                      (tag (plump:tag-name (elt children 0)))
                      (matching-tag (string-equal tag tag-name)))
    (every #'whitespace-node-p
           (remove-if #'plump:element-p (plump:children node)))))

;; Readability._hasChildBlockElement()
(defmethod has-block-children-p ((node plump:element))
  (some (lambda (child)
          (or (not (plump:nesting-node-p child))
              (smember (plump:tag-name child)
                       '("BLOCKQUOTE" "DL" "DIV" "IMG" "OL" "P" "PRE" "TABLE" "UL"))
              (has-block-children-p child)))
        (plump:children node)))

(defmethod has-block-children-p ((node plump:node))
  nil)

(defmethod get-inner-text ((node plump:node))
  ;; TODO: normalize spaces
  (serapeum:trim-whitespace (plump:text node)))

;; XXX: Readability._getLinkDensity()
(defmethod link-density ((element plump:element))
  (/ (reduce (lambda (link-length link)
               (+ link-length
                  (let* ((href (plump:get-attribute link "href"))
                         (hash-p (when href (eql #\# (elt href 0)))))
                    (* (length (get-inner-text link)) (if hash-p 0.3 1)))))
             (clss:select "a" element) :initial-value 0)
     (length (get-inner-text element))))

(defvar *score-table* (make-hash-table)
  "The table for the already computed nodes score to cache the existing results.")

(defmethod calculate-score ((element plump:element))
  (alexandria:ensure-gethash
   element *score-table*
   (let* ((name (plump:tag-name element))
          (initial-score
            (1+ (cond
                  ((equalp "div" name) 5)
                  ((smember name '("td" "pre" "blockquote")) 3)
                  ((smember name '("address" "ol" "ul" "dl" "dd" "dt" "li" "form"))
                   -3)
                  ((smember name '("h1" "h2" "h3" "h4" "h5" "h6" "th")) -5)
                  (t 0))))
          (inner-text (get-inner-text element))
          (comma-score (1+ (count #\, inner-text)))
          (length-score (min (round (/ (length inner-text) 100)) 3))
          (class-score (get-class-weight element))
          (link-density (link-density element))
          (progeny-score (labels ((calc (node &optional (level 0))
                                    (if (scoreable-p node)
                                        (+ (/ (calculate-score node)
                                              (case level
                                                (0 1)
                                                (1 2)
                                                (otherwise (* level 3))))
                                           (reduce #'+ (map 'list (alexandria:rcurry #'calc (1+ level))
                                                            (plump:children node))))
                                        0)))
                           (reduce #'+ (map 'list #'calc (plump:children element))))))
     (* (- 1 link-density)
        (+ initial-score comma-score length-score class-score progeny-score)))))

(defmethod scoreable-p ((node plump:element))
  (and (smember (plump:tag-name node) *tags-to-score*)
       (has-block-children-p node)
       (plump:parent node)
       (plump:element-p (plump:parent node))
       (> (length (get-inner-text node)) 25)))

(defmethod scoreable-p ((node plump:node))
  nil)

(defmethod grab-article ((document plump:nesting-node))
  (alexandria:when-let* ((body (clss:select "body" document))
                         (body (elt body 0)))
    (loop for node across (clss:select "*" body)
          for match-string = (uiop:strcat (plump:get-attribute node "class") " "
                                          (plump:get-attribute node "id"))
          when (and *visibility-checker*
                    (not (funcall *visibility-checker* node)))
            do (remove-child* node)
               ;; TODO: this._checkByline(node, matchString)
          when (and (ppcre:scan *unlikely-candidate-regex* match-string)
                    (not (ppcre:scan *maybe-candidate-regex* match-string))
                    (not (has-ancestor-tag node "table"))
                    (not (has-ancestor-tag node "code"))
                    (not (clss:node-matches-p "body,a" node)))
            do (remove-child* node)
          when (and (plump:get-attribute node "role")
                    (smember (plump:get-attribute node "role") *unlikely-roles* ))
            do (remove-child* node)
          when (plump:comment-p node)
            do (remove-child* node)
          when (and (smember (plump:tag-name node)
                             (list "div" "section" "header" "h1" "h2" "h3" "h4" "h5" "h6"))
                    (without-content-p node))
            do (remove-child* node)
          when (and (plump:element-p node)
                    (plump:parent node)
                    (string-equal "div" (plump:tag-name node)))
            do (reduce
                (lambda (c1 c2)
                  (if (and (phrasing-content-p c2)
                           (not (whitespace-node-p c2)))
                      (progn
                        (remove-child* c2)
                        (plump:append-child c1 c2)
                        c1)
                      (if (zerop (length (plump:children c1)))
                          c1
                          (plump:make-element node "p"))))
                (plump:children node)
                :initial-value (plump:make-element node "p"))
          when (single-tag-inside-p node "p")
            do (plump:replace-child node (elt (clss:select "p" node) 0))
               ;; TODO: L1005-1319
          when (scoreable-p node)
            maximize (calculate-score node) into max-score
          ;; TODO: 1057-1319
          )
    body))

(defmethod post-process-content ((element plump:nesting-node) url)
  (fix-relative-urls element url)
  (simplify-nested-elements element)
  (normalize-classes element)
  element)

(defmethod nparse ((document plump:nesting-node) url)
  (alexandria:when-let* ((max *max-elements*)
                         (len (length (clss:select "*" document)))
                         (long (> len max)))
    (signal 'too-many-elements-error :number-of-elements len))
  ;; TODO: this._unwrapNoscriptImages(this._doc);
  ;; TODO: var jsonLd = this._disableJSONLD ? {} : this._getJSONLD(this._doc);
  ;; XXX: this._removeScripts(this._doc);
  (loop for node across (clss:select "script,noscript" document)
        when (equalp "script" (plump:tag-name node))
          do (plump:remove-attribute node "src")
             ;; FIXME: Can <script> have >1 children? What for?
          and do (plump:remove-child (elt (plump:children node) 0))
        else do (plump:remove-child node))
  (prepare-document document)
  (let* ((doc document)
         (lang (alexandria:when-let ((html (clss:select "html" doc)))
                 (plump:get-attribute (elt html 0) "lang")))
         ;; TODO: var metadata = this._getArticleMetadata(jsonLd);
         (metadata nil)
         ;; TODO: this._articleTitle = metadata.title;
         (title nil)
         ;; TODO: var articleContent = this._grabArticle();
         (doc (grab-article doc))
         ;; XXX: this._postProcessContent(articleContent);
         (doc (post-process-content doc url)))
    ;; TODO: Find excerpt.
    (values doc
            (plump:serialize doc nil)
            (plump:text doc)
            (length (plump:text doc))
            ;; TODO: Return:
            ;; - title
            ;; - byline
            ;; - dir (?)
            lang
            ;; - excerpt
            ;; - site name
            ))
  )
