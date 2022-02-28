;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(defmethod node-visible-p ((node plump:element))
  ;; TODO: (!node.style || node.style.display != "none")
  (and (not (plump:has-attribute node "hidden"))
       ;; TODO: (node.className && node.className.indexOf &&
       ;; node.className.indexOf("fallback-image") !== -1
       (or (not (plump:has-attribute node "aria-hidden"))
           (not (string-equal (plump:get-attribute node "aria-hidden") "true")))))

(defmethod is-readerable ((document plump:root) &key min-content-length min-score
                                                  unlikely-candidate-regex maybe-candidate-regex
                                                  (visibility-checker #'node-visible-p))
  (let* ((nodes (clss:select "p, pre, article, div > br" document)))
    (loop with max-score = 0
          for node across nodes
          for score
            = (serapeum:and-let*
                  ((match-string (uiop:strcat (plump:get-attribute node "class") " "
                                              (plump:get-attribute node "id")))
                   (visible-p (if visibility-checker
                                  (funcall visibility-checker node)
                                  t))
                   (likely-candidate (or (not (cl-ppcre:scan unlikely-candidate-regex match-string))
                                         (cl-ppcre:scan maybe-candidate-regex match-string)))
                   (not-a-li (not (clss:node-matches-p "li p" node)))
                   (text-content (string-trim serapeum:whitespace (plump:text node)))
                   (text-content-length (length text-content))
                   (length-sufficient (>= text-content-length min-content-length)))
                (sqrt (- text-content-length min-content-length)))
          when score
            do (incf max-score score)
          when (> max-score min-score)
            do (return t)
          finally (return nil))))

;; XXX: A free rewrite of Readability._cleanClasses()
(defmethod normalize-classes ((node plump:node))
  ;; TODO: Classes to preserve.
  (when (plump:element-p node)
    (plump:remove-attribute node "class")
    (loop for child across (plump:children node)
          do (normalize-classes child))))

;;; XXX: Readability._fixRelativeUris
(defmethod fix-relative-urls ((node plump:element) absolute-uri)
  (flet ((relative->absolute (uri)
           (quri:merge-uris (quri:uri uri) (quri:uri absolute-uri))))
    (loop for elem across (clss:select "a, img, picture, figure, video, audio, source" node)
          for href = (plump:get-attribute elem "href")
          for src = (plump:get-attribute elem "src")
          for poster = (plump:get-attribute elem "poster")
          ;; TODO: srcset
          unless (uiop:emptyp src)
            do (plump:set-attribute elem "src" (relative->absolute src))
          unless (uiop:emptyp poster)
            do (plump:set-attribute elem "poster" (relative->absolute poster))
          when (and href (uiop:string-prefix-p "JavaScript:" href))
            ;; TODO: if the link only contains simple text content, it
            ;; can be converted to a text node
            ;;
            ;; Replace the link with a span to preserve children.
            do (plump:replace-child
                elem (make-instance
                      'plump:element :tag-name "span" :parent nil :children (plump:children elem)))
          else do (plump:set-attribute elem "href" (relative->absolute href)))))

;;; XXX: Readability._simplifyNestedElements
(defmethod simplify-nested-elements ((element plump:element))
  (loop for node = element then (get-next-node node)
        for parent = (plump:parent node)
        when (and (clss:node-matches-p "div, section" node)
                  (without-content-p node))
          do (plump:remove-child node)
        else when (serapeum:single (plump:children node))
               do (plump:replace-child
                   node (serapeum:lret ((child (elt (plump:children node) 0)))
                          (setf (plump:attributes child) (plump:attributes node))))))
