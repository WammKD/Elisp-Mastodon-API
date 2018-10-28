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

(defconst mastAPI-NO_REDIRECT "urn:ietf:wg:oauth:2.0:oob"
  "")



;; Utility
(defun mastAPI-create-URI (domain &rest rest)
  ""
  (apply 'concat (cons domain (cons (unless (string-suffix-p "/" domain) "/") rest))))
(defun mastAPI-concat-amps (args)
  (mapconcat
    (lambda (arg)
      (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg))))
    args
    "&"))
(defun mastAPI-prepare-image (imagePath)
  ""
  (with-temp-buffer
    (insert-file-contents imagePath)
    (buffer-substring-no-properties (point-min) (point-max))))
(defun mastAPI-process (buffer)
  ""
  (with-current-buffer buffer
    (let ((json+ (buffer-string)))
      (json-read-from-string (substring json+ (string-match-p "{" json+))))))
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
    "POST"
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
    "POST"
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
    "POST"
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
(defun mastAPI-get-account (domain token id &optional async-p)
  ""
  (mastAPI-request
    "GET"
    (mastAPI-create-URI domain "api/v1/accounts/" (number-to-string id))
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))

(defun mastAPI-get-user-account (domain token &optional async-p)
  ""
  (mastAPI-request
    "GET"
    (mastAPI-create-URI domain "api/v1/accounts/verify_credentials")
    `(("Authorization" . ,(concat "Bearer " token)))
    '()
    async-p))
