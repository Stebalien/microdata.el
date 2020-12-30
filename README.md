# microdata.el

An emacs package for extracting microdata from HTML and emails. Useful for
executing email actions from Emacs.

Personally, I use this package to open Discourse and GitHub PR/issue
notifications from notmuch.

## Notmuch Example

Commands for `notmuch-show` mode.

```elisp
(require 'microdata)
(require 'ivy)
(require 'notmuch)

(defun notmuch-show--get-actions ()
  (let (html)
    (with-current-notmuch-show-message (setq html (microdata-email-actions)))
    html))

(defun notmuch-show-action ()
  "Pick an action to perform on the email."
  (interactive)
  (if-let ((actions (notmuch-show--get-actions)))
      (ivy-read "Action: " actions
                :require-match t
                :caller #'notmuch-show-action
                :action (lambda (res) (browse-url (alist-get 'url (cdr res)))))
    (user-error "No actions defined!")))

;; This can be used to quickly open an email's "view target" in a web browser.
(defun notmuch-show-action-view ()
  "Execute the `view' action."
  (interactive)
  (-some->> (notmuch-show--get-actions)
    (-map #'cdr)
    (--find (string= "ViewAction" (alist-get 'type it)))
    (alist-get 'url)
    (browse-url)))
```

Commands for `notmuch-search` mode.

```elisp
(require 'microdata)
(require 'notmuch)

(defun notmuch-search-action-view ()
  "Execute the `view' action."
  (interactive)
  (let ((id (car (split-string (car (notmuch-search-find-stable-query)) " ")))
        (coding-system-for-read 'no-conversion))
    (-some->> (with-temp-buffer
                (call-process notmuch-command nil t nil "show" "--format=raw" id)
                (microdata-email-actions))
      (-map #'cdr)
      (--find (string= "ViewAction" (alist-get 'type it)))
      (alist-get 'url)
      (browse-url))))
```
