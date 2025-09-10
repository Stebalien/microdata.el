;;; microdata.el --- Structured data extraction -*- lexical-binding: t -*-

;; Copyright 2020-2025 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/microdata.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (seq "2.22") notmuch)
;; Keywords: microdata, mail

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
(require 'seq)
(require 'dom)
(require 'subr-x)
(require 'mm-decode)

(defalias 'microdata--dom-inner-text
  (if (< emacs-major-version 31) #'dom-text #'dom-inner-text))

(defun microdata-from-html ()
  "Extracts microdata from an HTML buffer."
  (cdr (microdata--parse (libxml-parse-html-region (point-min) (point-max)))))

(defun microdata-from-email ()
  "Extracts microdata from a buffer containing an MIME encoded email."
  (mm-with-part (mm-find-part-by-type (list (mm-dissect-buffer)) "text/html" nil t)
    (pcase-let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                 (`(,properties . ,items) (microdata--parse dom)))
      ;; Ok, so, Google doesn't always remember to wrap their emails in an
      ;; "EmailMessage" item.
      ;;
      ;; If we see unattached properties, attach them to an email object.
      (when properties
        (let ((item (make-hash-table :test 'equal)))
          (puthash "@context" "http://schema.org" item)
          (puthash "@type" "EmailMessage" item)
          (pcase-dolist (`(,k . ,v) properties)
            (puthash k v item))
          (push item items)))
      items)))

(defun microdata-email-actions ()
  "Extract email actions from a buffer containing an MIME encoded email.

Returns an alist mapping action names to properties alists (containing a `url'
and a `type'). an alist containing a `url' and a `type'."
  (thread-last
    (microdata-from-email)
    (seq-filter
     (lambda (it) (and (string= "http://schema.org" (gethash "@context" it))
                  (string= "EmailMessage" (gethash "@type" it)))))
    (seq-keep
     (lambda (it) (or (gethash "potentialAction" it) (gethash "action" it))))
    (seq-map
     (lambda (it) (let* ((type (gethash "@type" it))
                    (url (or (gethash "url" it) (gethash "target" it)))
                    (name (or (gethash "name" it)
                              (format "%s: %s" (string-remove-suffix "Action" type) url))))
               `(,name . ((url . ,url) (type . ,type))))))))

(defun microdata-email-actions-by-type (type)
  "Extract all email actions with the given TYPE (usually zero or one).

Returns an alist mapping action names to URLs."
  (thread-last
    (microdata-email-actions)
    (seq-filter
     (lambda (it) (string= type (alist-get 'type (cdr it)))))
    (seq-map
     (lambda (it) (cons (car it) (alist-get 'url (cdr it)))))))

(defun microdata--parse (element)
  "Extract microdata from a parsed dom ELEMENT."
  (if (eq (dom-tag element) 'script)
      ;; If we're looking at a script tag, check for json-ld, otherwise ignore.
      (when (string= (dom-attr element 'type) "application/ld+json")
        ;; Parse it, then return an alist mapping `nil' to each item.
        ;; Otherwise, they'll be treated as properties.
        (thread-last
          (json-parse-string (caddr element)
                             :array-type 'list
                             :null-object nil
                             :false-object nil)
          ensure-list
          (cons nil)))
    (let* ((type (dom-attr element 'itemtype))
           (prop (dom-attr element 'itemprop))
           (scope (or type (dom-attr element 'itemscope)))
           properties items item)
      ;; First, parse the children. Children can contain unattached properties
      ;; and standalone items.
      (thread-last
        element
        (dom-non-text-children)
        (seq-map 'microdata--parse)
        (seq-do (pcase-lambda (`(,p . ,i))
          (setq properties (append p properties)
                items (append i items)))))
      ;; If this element defines an item scope, create a new item from the
      ;; unattached properties.
      (when scope
        (setq item (make-hash-table :test 'equal))
        ;; if there is a type defined, add it to the object.
        (pcase type
          ((rx (let context (+ nonl)) "/" (let type (+ nonl)))
           (puthash "@context" context item)
           (puthash "@type" type item))
          ((pred (not null))
           (puthash "@type" type item)))
        ;; now add the properties
        (pcase-dolist (`(,k . ,v) properties)
          (puthash k v item))
        ;; and "claim" them.
        (setq properties nil))
      (if (not prop)
          ;; If we're not defining a property and we have an item, save the item.
          (when item (push item items))
        ;; If we have a property and don't have an item, extract an item.
        (unless item
          (setq item (pcase (dom-tag element)
                       ((or 'link 'a) (dom-attr element 'href))
                       ('meta (dom-attr element 'content))
                       ('time (dom-attr element 'datetime))
                       (_ (microdata--dom-inner-text element)))))
        ;; then assign the item to the property.
        (push (cons prop item) properties))
      (cons properties items))))

(provide 'microdata)
;;; microdata.el ends here
