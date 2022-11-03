;;; notmuch-microdata.el --- Notmuch email actions -*- lexical-binding: t -*-

;; Copyright 2021 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/microdata.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.0") (microdata "0.0.1") notmuch)
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

;; This package contains three functions for acting on email actions.

;;; Code:
(require 'microdata)
(require 'notmuch)
(require 'browse-url)

;;;###autoload
(defun notmuch-microdata-search-action-view ()
  "Execute the `view' action."
  (interactive)
  (let ((id (car (split-string (car (notmuch-search-find-stable-query)) " ")))
        (coding-system-for-read 'no-conversion))
    (with-temp-buffer
      (call-process notmuch-command nil t nil "show" "--format=raw" id)
      (when-let ((action (car (microdata-email-actions-by-type "ViewAction"))))
        (message "%s: %s" (car action) (cdr action))
        (browse-url (cdr action))))))

(defun notmuch-microdata-show--get-actions ()
  (let (actions)
    (with-current-notmuch-show-message (setq actions (microdata-email-actions)))
    actions))

;;;###autoload
(defun notmuch-microdata-show-action ()
  "Pick an action to perform on the email."
  (interactive)
  (if-let ((actions (notmuch-microdata-show--get-actions)))
      (when-let* ((selected (completing-read "Action: " actions nil t))
                  (action (alist-get selected actions nil nil 'equal))
                  (url (alist-get 'url action)))
        (browse-url url))
    (user-error "No actions defined!")))

;;;###autoload
(defun notmuch-microdata-show-action-view ()
  "Execute the `view' action."
  (interactive)
  (with-current-notmuch-show-message 
   (when-let ((action (car (microdata-email-actions-by-type "ViewAction"))))
     (message "%s: %s" (car action) (cdr action))
     (browse-url (cdr action)))))

(provide 'notmuch-microdata)
