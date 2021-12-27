;;; tzc.el --- Converts time between different time zones  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/tzc
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Convert time between different time zones.

;;; Code:
(require 'timezone)

(defun tzc--get-timeshift-between-zones (from-zone to-zone)
  "Get the shift in time between FROM-ZONE and TO-ZONE."
  (let* ((from-zone-offset (format-time-string "%z" nil from-zone))
	 (to-zone-offset (format-time-string "%z" nil to-zone)))
    (- (timezone-zone-to-minute to-zone-offset) (timezone-zone-to-minute from-zone-offset))))

(tzc--get-timeshift-between-zones "America/New_York" "Asia/Kolkata")

(provide 'tzc)
;;; tzc.el ends here
