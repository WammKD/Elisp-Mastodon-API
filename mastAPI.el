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

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; string-suffix-p added in 24.4 so that's the cutoff, so far.

;;; Code:
(require 'url)

;; (defmacro mastAPI-defun (name para docOrBody &rest body)
;;   )

(defun mastAPI-register-app (domain client-name website &optional redirect-url)
  ""
  (let ((url-request-method "POST")
        (url-request-data   (concat "client_name="    client-name
                                    "&redirect_uris=" (or
                                                        redirect-url
                                                        "urn:ietf:wg:oauth:2.0:oob")
                                    "&scopes="        "write read"
                                    "&website="       website)))
    (url-retrieve
      (concat domain (unless (string-suffix-p "/" domain) "/") "api/v1/apps")
      '(lambda (status)
         (switch-to-buffer (current-buffer))))))
