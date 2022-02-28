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
                   (likely-candidate (or (not (cl-ppcre:scan
                                               unlikely-candidate-regex match-string))
                                         (cl-ppcre:scan
                                          maybe-candidate-regex match-string)))
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
