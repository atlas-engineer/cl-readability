;;;; SPDX-FileCopyrightText: Atlas Engineer LCC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:readability)

(define-condition too-many-elements-error (error)
  ((possible-maximum
    :initform *max-elements*
    :accessor possible-maximum
    :initarg :possible-maximum)
   (number-of-elements
    :initform nil
    :accessor number-of-elements
    :initarg :number-of-elements))
  (:report
   (lambda (condition stream)
     (format stream
             "Too many elements in the document: found ~d, expected no more than ~d"
             (number-of-elements condition) (possible-maximum condition))))
  (:documentation "A condition to signal when the number of elements to parse is too large.
See `*max-elements*'."))
