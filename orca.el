;;; orca.el --- Org Capture -*- lexical-binding: t -*-

;; Copyright (C) 2017 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/orca
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: org, convenience

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `org-capture' is powerful but also complex.  This package provides
;; several convenient recipes for configuring `org-capture'.

;;; Code:
(require 'org-protocol)
(require 'org-capture)

;;* Org config
(add-to-list 'org-capture-templates
             '("l" "Link" entry (function orca-handle-link)
               "* TODO %(orca-wash-link)\nAdded: %T\n%?"))
(setq org-protocol-default-template-key "l")

;;* Orca config
(defgroup orca nil
  "Org capture"
  :group 'org)

(defcustom orca-wash-list
  '((" - Emacs Stack Exchange" "")
    (" - Stack Overflow" ""))
  "A list of (REGEX REP) to be applied on link title.")

(defcustom orca-handler-list
  (let ((emacs "~/Dropbox/org/wiki/emacs.org")
        (entor "~/Dropbox/org/ent.org"))
    `((orca-handler-match-url "https://www.reddit.com/r/emacs" ,emacs "Reddit")
      (orca-handler-match-url "https://emacs.stackexchange.com/" ,emacs "\\* Questions")
      (orca-handler-current-buffer "\\* Tasks")
      (orca-handler-file ,entor "\\* Articles")))
  "List of handlers by priority.

Each item is a function of zero arguments that opens an
appropiriate file and returns non-nil on match."
  :type '(repeat
          (choice
           (list
            :tag "Current buffer"
            (const orca-handler-current-buffer)
            (string :tag "Heading"))
           (list
            :tag "URL matching regex"
            (const orca-handler-match-url)
            (string :tag "URL")
            (string :tag "File")
            (string :tag "Heading"))
           (list
            :tag "Default"
            (const orca-handler-file)
            (string :tag "File")
            (string :tag "Heading")))))

;;* Functions
(defun orca-wash-link ()
  "Return a pretty-printed top of `org-stored-links'.
Try to remove superfluous information, like the website title."
  (let ((link (caar org-stored-links))
        (title (cl-cadar org-stored-links)))
    (dolist (repl orca-wash-list)
      (setq title (replace-regexp-in-string
                   (nth 0 repl) (nth 1 repl) title)))
    (org-make-link-string link title)))

(defun orca-require-program (program)
  "Check system for PROGRAM, printing error if unfound."
  (or (and (stringp program)
           (not (string= program ""))
           (executable-find program))
      (user-error "Required program \"%s\" not found in your path" program)))

(defun orca-raise-frame ()
  "Put Emacs frame into focus."
  (if (eq system-type 'gnu/linux)
      (progn
        (orca-require-program "wmctrl")
        (call-process
         "wmctrl" nil nil nil "-i" "-R"
         (frame-parameter (selected-frame) 'outer-window-id)))
    (raise-frame)))

;;* Handlers
(defvar orca-dbg-buf nil)

(defun orca-handler-current-buffer (heading)
  "Select the current `org-mode' buffer with HEADING."
  ;; We are in the server buffer; the actual current buffer is first
  ;; on `buffer-list'.
  (let ((orig-buffer (nth 0 (buffer-list))))
    (setq orca-dbg-buf orig-buffer)
    (when (with-current-buffer orig-buffer
            (and (eq major-mode 'org-mode)
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward heading nil t))))
      (switch-to-buffer orig-buffer)
      (goto-char (match-end 0)))))

(defun orca-handler-file (file heading)
  "Select FILE at HEADING."
  (when (file-exists-p file)
    (find-file file)
    (goto-char (point-min))
    (re-search-forward heading nil t)))

(defun orca-handler-match-url (url-regex file heading)
  "For link matching URL-REGEX select FILE at HEADING."
  (when (string-match url-regex (caar org-stored-links))
    (orca-handler-file file heading)))

(defun orca-handle-link ()
  "Select a location to store the current link."
  (orca-raise-frame)
  (let ((hands orca-handler-list)
        hand)
    (while (and (setq hand (pop hands))
                (null
                 (apply (car hand) (cdr hand)))))))

(provide 'orca)

;;; orca.el ends here
