;;; mastAPI.el --- functions for interfacing with Mastodon's API
;; Copyright (C) 2018
;;        Free Software Foundation, Inc.

;; Author: Jonathan Schmeling <jaft@outlook.com>
;; Keywords: api, mastodon
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; string-suffix-p added in 24.4 so that's the cutoff, so far.

;;; Code:
(require 'url)
(require 'json)

(defconst mastAPI-REQUEST_GET    "GET"
  "")
(defconst mastAPI-REQUEST_PUT    "PUT"
  "")
(defconst mastAPI-REQUEST_POST   "POST"
  "")
(defconst mastAPI-REQUEST_PATCH  "PATCH"
  "")
(defconst mastAPI-REQUEST_DELETE "DELETE"
  "")
(defconst mastAPI-NO_REDIRECT   "urn:ietf:wg:oauth:2.0:oob"
  "")



;; Utility
(defun mastAPI-convert-to-string (obj)
  ""
  (if (numberp obj) (number-to-string obj) obj))
(defun mastAPI-create-URI (domain &rest rest)
  ""
  (apply 'concat (cons domain (cons (unless (string-suffix-p "/" domain) "/") rest))))
(defun mastAPI-prepare-image (imagePath)
  ""
  (with-temp-buffer
    (insert-file-contents imagePath)
    (buffer-substring-no-properties (point-min) (point-max))))
(defun mastAPI-concat-amps (args)
  ""
  (let ((temp (lambda (init final)
                (let ((fst (car init)))
                  (cond
                   ((not fst)                           final)
                   ((cdr fst) (funcall temp (cdr init) (concat
                                                         (if (cdr init) "&" "")
                                                         (url-hexify-string (car fst))
                                                         "="
                                                         (url-hexify-string
                                                           (mastAPI-convert-to-string
                                                             (cdr fst)))
                                                         final)))
                   (t         (funcall temp (cdr init) final)))))))
    (funcall temp args "")))

(defun mastAPI-process (buffer)
  ""
  (with-current-buffer buffer
    (let ((json+ (buffer-string)))
      (json-read-from-string (substring json+ (or
                                                (string-match-p "\n\n{"   json+)
                                                (string-match-p "\n\n\\[" json+)))))))
(defun mastAPI-request (reqMeth finalDomain headers data async-p)
  ""
  (let ((url-request-method                           reqMeth)
        (url-request-extra-headers                    headers)
        (url-request-data          (mastAPI-concat-amps data)))
    (if async-p
        (url-retrieve finalDomain `(lambda (status)
                                     (funcall ,async-p (mastAPI-process
                                                         (current-buffer)))))
      (mastAPI-process (url-retrieve-synchronously finalDomain)))))



;; App.s
(defun mastAPI-register-app (domain clientName website &optional redirectURI)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/apps")
    '()
    `(("client_name"   . ,clientName)
      ("redirect_uris" . ,(or redirectURI mastAPI-NO_REDIRECT))
      ("scopes"        . "read write follow")
      ("website"       . ,website))
    async-p))



;; Auth.
(defun mastAPI-generate-user-auth-URI (domain clientID &optional redirectURI scopes)
  ""
  (concat
    domain
    (unless (string-suffix-p "/" domain) "/")
    "oauth/authorize?"
    (mastAPI-concat-amps `(("scope"         . ,(or scopes "read write follow"))
                           ("response_type" . "code")
                           ("redirect_uri"  . ,(or redirectURI mastAPI-NO_REDIRECT))
                           ("client_id"     . ,clientID)))))

(defun mastAPI-get-token-via-auth-code (domain       clientID
                                        clientSecret authCode
                                        &optional    redirectURI async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "oauth/token")
    '()
    `(("client_id"     . ,clientID)
      ("client_secret" . ,clientSecret)
      ("grant_type"    . "authorization_code")
      ("code"          . ,authCode)
      ("redirect_uri"  . ,(or redirectURI mastAPI-NO_REDIRECT)))
    async-p))

(defun mastAPI-get-token-via-user-pass (domain       clientID
                                        clientSecret username
                                        password     &optional scopes async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "oauth/token")
    '()
    `(("client_id"     . ,clientID)
      ("client_secret" . ,clientSecret)
      ("scope"         . ,(or scopes "read write follow"))
      ("grant_type"    . "password")
      ("username"      . ,username)
      ("password"      . ,password))
    async-p))



;; Accounts
(defun mastAPI-account-get (domain token id &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_GET
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id))
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-current-user (domain token &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_GET
    (mastAPI-create-URI domain "api/v1/accounts/verify_credentials")
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-update-user (domain
                                    token  &optional displayName      note
                                                     avatar           header
                                                     locked           source
                                                     fieldsAttributes async-p)
  ""
  (mastAPI-request
    "PATCH"
    (mastAPI-create-URI domain "api/v1/accounts/update_credentials")
    (cons
      `("Authorization" . ,(concat "Bearer " token))
      (if (or avatar header) '(("Content-Type" . "multipart/form-data")) '()))
    `(("display_name"     . ,displayName)
      ("note"             . ,note)
      ("avatar"           . ,avatar)
      ("header"           . ,header)
      ("locked"           . ,locked)
      ("source"           . ,source)
      ("fieldsAttributes" . ,fieldsAttributes))
    async-p))

(defun mastAPI-account-get-followers (domain token id &optional maxID sinceID
                                                                limit async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_GET
    (mastAPI-create-URI domain                "api/v1/accounts/"
                        (number-to-string id) "/followers?"
                        (mastAPI-concat-amps  `(("max_id"   .   ,maxID)
                                                ("since_id" . ,sinceID)
                                                ("limit"    .   ,limit))))
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-get-following (domain token id &optional maxID sinceID
                                                                limit async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_GET
    (mastAPI-create-URI domain                "api/v1/accounts/"
                        (number-to-string id) "/following?"
                        (mastAPI-concat-amps  `(("max_id"   .   ,maxID)
                                                ("since_id" . ,sinceID)
                                                ("limit"    .   ,limit))))
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-get-statuses (domain token
                                     id     &optional onlyMedia      pinned
                                                      excludeReplies maxID
                                                      sinceID        limit  async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_GET
    (mastAPI-create-URI domain                "api/v1/accounts/"
                        (number-to-string id) "/statuses?"
                        (mastAPI-concat-amps  `(("only_media"      .      ,onlyMedia)
                                                ("pinned"          .         ,pinned)
                                                ("exclude_replies" . ,excludeReplies)
                                                ("max_id"          .          ,maxID)
                                                ("since_id"        .        ,sinceID)
                                                ("limit"           .          ,limit))))
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-follow (domain token id &optional reblogs async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id) "/follow")
    `(("Authorization" . ,(concat "Bearer " token)))
    (list (cons "reblogs" reblogs))
    async-p))

(defun mastAPI-account-unfollow (domain token id &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id) "/unfollow")
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-block (domain token id &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id) "/block")
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-unblock (domain token id &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id) "/unblock")
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-mute (domain token id &optional notifications async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id) "/mute")
    `(("Authorization" . ,(concat "Bearer " token)))
    (list (cons "notifications" notifications))
    async-p))

(defun mastAPI-account-unmute (domain token id &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id) "/unmute")
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-endorse (domain token id &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id) "/pin")
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-unendorse (domain token id &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_POST
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id) "/unpin")
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-get-relationships (domain token id &optional async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_GET
    (mastAPI-create-URI domain "api/v1/accounts/relationships?id=" (number-to-string id))
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-account-search (domain token q &optional limit following async-p)
  ""
  (mastAPI-request
    mastAPI-REQUEST_GET
    (mastAPI-create-URI domain               "api/v1/accounts/search?"
                        (mastAPI-concat-amps `(("q"         .         ,q)
                                               ("limit"     .     ,limit)
                                               ("following" . ,following))))
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))
