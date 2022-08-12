;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defmethod qs ((root plump:element) &optional selector)
  (elt (clss:select selector root) 0))
(defmethod qsa ((root plump:nesting-node) &rest selectors)
  (if (null selectors)
      (coerce (clss:select "*" root) 'list)
      (alexandria:mappend
       (lambda (selector)
         (coerce (clss:select selector root) 'list))
       selectors)))
(defmethod qsa ((root plump:element) &rest selectors)
  (if (null selectors)
      (coerce (clss:select "*" root) 'list)
      (alexandria:mappend
       (lambda (selector)
         (coerce (clss:select selector root) 'list))
       selectors)))
(defmethod name ((element t))
  nil)
(defmethod name ((element plump:element))
  (plump:tag-name element))
(defmethod name ((element plump:node))
  nil)
(defmethod attr ((element plump:element) &optional attribute)
  (plump:get-attribute element attribute))
(defmethod (setf attr) (value (element plump:element) &optional attribute)
  (typecase value
    (null (plump:remove-attribute element attribute))
    (string (plump:set-attribute element attribute value))
    (t (plump:set-attribute element attribute (princ-to-string value)))))
(defmethod (setf attr) ((value null) (element plump:element) &optional attribute)
  (plump:remove-attribute element attribute))
(defmethod (setf attr) ((value string) (element plump:element) &optional attribute)
  (plump:set-attribute element attribute value))
(defmethod (setf attr) ((value t) (element plump:element) &optional attribute)
  (plump:set-attribute element attribute (princ-to-string value)))
(defmethod attrs ((element plump:element))
  (alexandria:hash-table-keys (plump:attributes element)))
(defmethod matches ((element t) &rest selectors)
  (declare (ignore selectors))
  nil)
(defmethod matches ((element plump:element) &rest selectors)
  (some (alexandria:rcurry #'clss:node-matches-p element) selectors))
(defmethod inner-text ((node plump:node))
  ;; FIXME: This often returns meaningless text for <style> and
  ;; <script> elements. Maybe re-write the logic somehow?
  (plump:text node))
;; To conform to element-p.
(defmethod inner-text ((node plump:element))
  (plump:text node))
(defmethod inner-html ((node t))
  "")
(defmethod inner-html ((node plump:nesting-node))
  (serapeum:mapconcat
   (alexandria:rcurry #'plump:serialize nil)
   (children node)
   ""))
(defmethod append-child ((parent plump:nesting-node) (child plump:child-node))
  (plump:append-child parent child))
;; To conform to element-p.
(defmethod append-child ((parent plump:element) (child plump:child-node))
  (plump:append-child parent child))
(defmethod remove-child ((node plump:child-node))
  (when (plump:parent node)
    (plump:remove-child node)))
(defmethod replace-child ((child plump:child-node) (replacement plump:child-node))
  (plump:replace-child child replacement))
(defmethod set-tag-name ((element plump:element) tag-name)
  (serapeum:lret ((new (apply #'make-instance 'plump:element
                              :attributes (plump:attributes element)
                              :parent (plump:parent element)
                              :tag-name tag-name
                              :children (plump:children element)
                              nil)))
    (plump:replace-child element new)))
;; To conform to element-p.
(defmethod parent ((element plump:element))
  (plump:parent element))
(defmethod parent ((element plump:child-node))
  (plump:parent element))
(defmethod parent ((element plump:node))
  nil)
(defmethod child-nodes ((element t))
  nil)
(defmethod child-nodes ((element plump:nesting-node))
  (coerce (plump:children element) 'list))
(defmethod child-nodes ((element plump:element))
  (coerce (plump:children element) 'list))
(defmethod children ((element t))
  nil)
(defmethod children ((element plump:nesting-node))
  (coerce (plump:children element) 'list))
(defmethod children ((element plump:element))
  (coerce (plump:child-elements element) 'list))
(defmethod next-sibling ((node plump:node))
  (unless (or (null node)
              (null (parent node))
              (zerop (length (plump:family node)))
              (eq node (plump:last-child (parent node))))
    (plump:next-sibling node)))
(defmethod next-sibling ((node null))
  nil)
(defmethod text-node-p ((node t))
  (plump:text-node-p node))
(defmethod make-text-node ((text string))
  ;; TODO: Somehow avoid `plump:make-root'.
  (plump:make-text-node (plump:make-root) text))

(defmethod node-visible-p ((node plump:element))
  (and (or (not (plump:has-attribute node "style"))
           (not (search "display: none" (plump:get-attribute node "style"))))
       (not (plump:has-attribute node "hidden"))
       (or (not (plump:has-attribute node "class"))
           (search "fallback-image" (plump:get-attribute node "class")))
       (or (not (plump:has-attribute node "aria-hidden"))
           (not (string-equal (plump:get-attribute node "aria-hidden") "true")))))

(defmethod is-readerable :around ((document plump:root))
  "An :around method for the default `is-readerable' to override the `*visibility-checker*'."
  (let ((*visibility-checker* #'node-visible-p))
    (call-next-method)))

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

(defmethod recursive-parents ((node plump:child-node) &key (max-depth 3))
  (labels ((parents (node depth)
             (when (and (plusp depth)
                        (< depth max-depth)
                        (plump:child-node-p node)
                        (plump:parent node))
               (cons node (parents (plump:parent node) (1+ depth))))))
    (parents node 0)))

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
          (class-score (class-weight element))
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
                           (not (whitespace-p c2)))
                      (progn
                        (remove-child c2)
                        (plump:append-child c1 c2)
                        c1)
                      (if (zerop (length (plump:children c1)))
                          c1
                          (plump:make-element node "p"))))
                (plump:children node)
                :initial-value (plump:make-element node "p"))
          when (and (parent node) (single-tag-inside-p node "p"))
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
  (unwrap-noscript-images document)
  ;; TODO: var jsonLd = this._disableJSONLD ? {} : this._getJSONLD(this._doc);
  (remove-scripts document)
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
