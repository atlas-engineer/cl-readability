;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defmethod qs ((root plump:nesting-node) selector)
  (elt (clss:select selector root) 0))
(defmethod qsa ((root plump:nesting-node) selector)
  (coerce (clss:select selector root) 'list))
(defmethod name ((element plump:element))
  (plump:tag-name element))
(defmethod name ((element plump:node))
  nil)
(defmethod attr ((element plump:element) (attribute string))
  (plump:get-attribute element attribute))
(defmethod (setf attr) (value (element plump:element) (attribute string))
  (typecase value
    (null (plump:remove-attribute element attribute))
    (string (plump:set-attribute element attribute value))
    (t (plump:set-attribute element attribute (princ-to-string value)))))
(defmethod (setf attr) ((value null) (element plump:element) (attribute string))
  (plump:remove-attribute element attribute))
(defmethod (setf attr) ((value string) (element plump:element) (attribute string))
  (plump:set-attribute element attribute value))
(defmethod (setf attr) ((value t) (element plump:element) (attribute string))
  (plump:set-attribute element attribute (princ-to-string value)))
(defmethod attrs ((element plump:element))
  (alexandria:hash-table-keys (plump:attributes element)))
(defmethod matches ((element plump:element) selector)
  (clss:node-matches-p selector element))
(defmethod inner-text ((node plump:node))
  ;; FIXME: This often returns meaningless text for <style> and
  ;; <script> elements. Maybe re-write the logic somehow?
  (plump:text node))
(defmethod remove-child ((node plump:child-node))
  (when (plump:parent node)
    (plump:remove-child node)))
(defmethod replace-child ((child plump:child-node) (replacement plump:child-node))
  (plump:replace-child child replacement))
(defmethod set-tag-name ((element plump:element) tag-name)
  (plump:replace-child
   element (apply #'make-instance
                  'plump:element
                  :tag-name tag-name
                  :children (plump:children element)
                  nil)))
(defmethod parent ((element plump:child-node))
  (plump:parent element))
(defmethod parent ((element plump:node))
  nil)
(defmethod children ((element plump:element))
  (coerce (plump:child-elements element) 'list))
(defmethod make-text-node ((text string))
  ;; TODO: Somehow avoid `plump:make-root'.
  (plump:make-text-node (plump:make-root) text))

(defmethod node-visible-p ((node plump:element))
  ;; TODO: (!node.style || node.style.display != "none")
  (and (not (plump:has-attribute node "hidden"))
       ;; TODO: (node.className && node.className.indexOf &&
       ;; node.className.indexOf("fallback-image") !== -1
       (or (not (plump:has-attribute node "aria-hidden"))
           (not (string-equal (plump:get-attribute node "aria-hidden") "true")))))

(defmethod is-readerable :around ((document plump:root))
  "An :around method for the default `is-readerable' to override the `*visibility-checker*'."
  (let ((*visibility-checker* #'node-visible-p))
    (call-next-method)))

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

;; TODO: Replace with the cleaning loops and BR cleaning call?
(defmethod prepare-document ((element plump:nesting-node))
  (loop for style across (clss:select "style" element)
        do (plump:remove-child style))
  ;; Remove BRs
  (loop for font across (clss:select "font" element)
        do (set-tag-name font "span")))

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
  (remove-child node)
  nil)

(defmethod whitespace-node-p ((node plump:text-node))
  (zerop (length (string-trim serapeum:whitespace (plump:text node)))))

(defmethod whitespace-node-p ((node plump:comment))
  (remove-child node)
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
            do (remove-child node)
               ;; TODO: this._checkByline(node, matchString)
          when (and (ppcre:scan *unlikely-candidate-regex* match-string)
                    (not (ppcre:scan *maybe-candidate-regex* match-string))
                    (not (has-ancestor-tag node "table"))
                    (not (has-ancestor-tag node "code"))
                    (not (clss:node-matches-p "body,a" node)))
            do (remove-child node)
          when (and (plump:get-attribute node "role")
                    (smember (plump:get-attribute node "role") *unlikely-roles* ))
            do (remove-child node)
          when (plump:comment-p node)
            do (remove-child node)
          when (and (smember (plump:tag-name node)
                             (list "div" "section" "header" "h1" "h2" "h3" "h4" "h5" "h6"))
                    (without-content-p node))
            do (remove-child node)
          when (and (plump:element-p node)
                    (plump:parent node)
                    (string-equal "div" (plump:tag-name node)))
            do (reduce
                (lambda (c1 c2)
                  (if (and (phrasing-content-p c2)
                           (not (whitespace-node-p c2)))
                      (progn
                        (remove-child c2)
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

(defun remove-non-elements (node)
  (cond
    ;; We can't remove those in `grab-article', because CLSS doesn't
    ;; grab non-element nodes :/
    ;;
    ;; FIXME: We need to remove those because Plump somewhy considers
    ;; comment and CDATA nodes as textual in `plump:text'.
    ((or (plump:cdata-p node)
         (plump:comment-p node)
         (plump:xml-header-p node)
         (and (plump:text-node-p node)
              (uiop:emptyp (get-inner-text node))))
     (remove-child node))
    ((plump:nesting-node-p node)
     (loop for child across (plump:children node)
           do (remove-non-elements child)))))

(defmethod post-process-content :after ((node plump:node))
  (remove-non-elements node))

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
         (*document-url* url)
         (lang (alexandria:when-let ((html (clss:select "html" doc)))
                 (plump:get-attribute (elt html 0) "lang")))
         ;; TODO: var metadata = this._getArticleMetadata(jsonLd);
         (metadata nil)
         ;; TODO: this._articleTitle = metadata.title;
         (title nil)
         ;; TODO: var articleContent = this._grabArticle();
         (doc (grab-article doc)))
    ;; XXX: this._postProcessContent(articleContent);
    (post-process-content doc)
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
