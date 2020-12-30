;;; microdata.el --- Structured data extraction -*- lexical-binding: t -*-

;; Copyright 2020 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/microdata.el
;; Version: 0.0.1
;; Package-Requires: ((dash "2.0.0") (emacs "27.0"))
;; Keywords: html5, microdata, email

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package contains a set of helper functions for extracting structured
;; data from HTML and emails. It's primarily useful for executing email
;; "actions".

;;; Code:
(require 'dash)
(require 'dom)
(require 'mm-decode)

(defun microdata-from-html ()
  "Extracts microdata from an HTML buffer."
  (microdata--from-dom (libxml-parse-html-region (point-min) (point-max))))

(defun microdata-from-email ()
  "Extracts microdata from a buffer containing an MIME encoded email."
  (mm-with-part (mm-find-part-by-type (list (mm-dissect-buffer)) "text/html" nil t)
    (microdata-from-html)))

(defun microdata-email-actions ()
  "Extracts email actions from a buffer containing an MIME encoded email.

Returns an alist mapping action names to properties alists (containing a `url' and a `type').
an alist containing a `url' and a `type'.
"
  (->>
   (microdata-from-email)
   (--filter (and
              (string= "http://schema.org" (gethash "@context" it))
              (string= "EmailMessage" (gethash "@type" it))))
   (--keep (or (gethash "potentialAction" it) (gethash "action" it))) 
   (--map (cons
           (gethash "name" it)
           `((url . ,(or (gethash "url" it) (gethash "target" it)))
             (type . ,(gethash "@type" it)))))))

(defun microdata-email-actions-by-type (type)
  "Extracts all email actions with the given type (usually zero or one).

Returns an alist mapping action names to URLs.
"
  (->>
   (microdata-email-actions)
   (--filter (string= type (alist-get 'type (cdr it))))
   (--map (cons (car it) (alist-get 'url (cdr it))))))

(defun microdata--parse-item (root)
  "Parses a microdata item."
  (let ((stack (list (dom-non-text-children root)))
        (item (make-hash-table :test 'equal)))
    ;; Extract the context/type.
    (when-let ((type (dom-attr root 'itemtype)))
      (if-let ((idx (--find-last-index (eq it ?/) (string-to-list type)))
               (context (substring type 0 idx))
               (type (substring type (1+ idx))))
          (progn (puthash "@context" context item)
                 (puthash "@type" type item))
        (puthash "@type" type item)))
    ;; Extract the properties.
    (while stack 
      (dolist (el (pop stack))
        (if-let ((prop (dom-attr el 'itemprop))
                 (value (pcase (dom-tag el)
                          ((guard (dom-attr el 'itemtype)) (microdata--parse-item el))
                          ((or 'link 'a) (dom-attr el 'href))
                          ('meta (dom-attr el 'content))
                          ('time (dom-attr el 'datetime))
                          (_ (dom-text el)))))
            (puthash prop value item)
          (push (dom-non-text-children el) stack))))
    item))

(defun microdata--from-dom (root)
  "Extracts microdata from a parsed DOM."
  (cond
   ;; First, look for json-ld
   ((and (eq (dom-tag root) 'script)
         (string= (dom-attr root 'type) "application/ld+json"))
    (-list (json-parse-string (caddr root)
                              :array-type 'list
                              :null-object nil
                              :false-object nil)))
   ;; then, look for microdata.
   ;; NOTE: we look for itemtype because libxml omits empty "boolean" html
   ;; attributes. That means:
   ;;     <div itemscope itemtype="foobar">
   ;; loses the "itemscope" tag.
   ((dom-attr root 'itemtype)
    (list (microdata--parse-item root)))
   ;; finally, search the children.
   (t (->>
       (dom-non-text-children root)
       (-map #'microdata--from-dom)
       (-flatten)))))

(provide 'microdata)
