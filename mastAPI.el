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



;; (defmacro mastAPI-defun (name para docOrBody &rest body)
;;   )



;; App.s
(defun mastAPI-register-app (domain clientName website &optional redirectURI)
  ""
  (let ((url-request-method "POST")
        (url-request-data   (concat "client_name="    clientName
                                    "&redirect_uris=" (or
                                                        redirectURI
                                                        mastAPI-NO_REDIRECT)
                                    "&scopes="        "read write follow"
                                    "&website="       website)))
    (with-current-buffer (url-retrieve-synchronously
                           (concat
                             domain
                             (unless (string-suffix-p "/" domain) "/")
                             "api/v1/apps"))
      (let ((json+ (buffer-string)))
        (json-read-from-string (substring json+ (string-match-p "{" json+)))))))



;; Auth.
(defun mastAPI-generate-user-auth-URI (domain clientID &optional redirectURI scopes)
  ""
  (concat
    domain
    (unless (string-suffix-p "/" domain) "/")
    "oauth/authorize"
    "?scope="         (or scopes "read%20write%20follow")
    "&response_type=" "code"
    "&redirect_uri="  (or redirectURI mastAPI-NO_REDIRECT)
    "&client_id="     clientID))

(defun mastAPI-get-token-via-auth-code (domain       clientID
                                        clientSecret authCode &optional redirectURI)
  ""
  (let ((url-request-method "POST")
        (url-request-data   (concat "client_id="      clientID
                                    "&client_secret=" clientSecret
                                    "&grant_type="    "authorization_code"
                                    "&code="          authCode
                                    "&redirect_uri="  (or
                                                        redirectURI
                                                        mastAPI-NO_REDIRECT))))
    (with-current-buffer (url-retrieve-synchronously
                           (concat
                             domain
                             (unless (string-suffix-p "/" domain) "/")
                             "oauth/token"))
      (let ((json+ (buffer-string)))
        (json-read-from-string (substring json+ (string-match-p "{" json+)))))))

(defun mastAPI-get-token-via-user-pass (domain       clientID
                                        clientSecret username
                                        password     &optional scopes)
  ""
  (let ((url-request-method "POST")
        (url-request-data   (concat "client_id="      clientID
                                    "&client_secret=" clientSecret
                                    "&scope="         (or
                                                        scopes
                                                        "read write follow")
                                    "&grant_type="    "password"
                                    "&username="      username
                                    "&password="      password)))
    (with-current-buffer (url-retrieve-synchronously
                           (concat
                             domain
                             (unless (string-suffix-p "/" domain) "/")
                             "oauth/token"))
      (let ((json+ (buffer-string)))
        (json-read-from-string (substring json+ (string-match-p "{" json+)))))))



;; Accounts
(defun mastAPI-get-account (domain token id)
  ""
  (let ((url-request-method        "GET")
        (url-request-extra-headers `(("Authorization" . ,(concat
                                                           "Bearer "
                                                           token)))))
    (with-current-buffer (url-retrieve-synchronously
                           (concat
                             domain
                             (unless (string-suffix-p "/" domain) "/")
                             "api/v1/accounts/"
                             (number-to-string id)))
      (let ((json+ (buffer-string)))
        (json-read-from-string (substring json+ (string-match-p "{" json+)))))))

(defun mastAPI-get-user-account (domain token)
  ""
  (let ((url-request-method        "GET")
        (url-request-extra-headers `(("Authorization" . ,(concat
                                                           "Bearer "
                                                           token)))))
    (with-current-buffer (url-retrieve-synchronously
                           (concat
                             domain
                             (unless (string-suffix-p "/" domain) "/")
                             "api/v1/accounts/verify_credentials"))
      (let ((json+ (buffer-string)))
        (json-read-from-string (substring json+ (string-match-p "{" json+)))))))
