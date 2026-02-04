;;; tzc-org.el --- Org mode integration for tzc  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/tzc
;; Keywords: convenience, timezone, org

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

;; Integration between tzc (timezone converter) and Org mode.
;; Provides `tzc-org-schedule' for scheduling org items with timezone conversion.
;; Provides `tzc-org-deadline' for scheduling org items with timezone conversion.

;;; Code:
(require 'tzc)
(require 'org-element)

(defun tzc-org--get-planning-ts (schedule/deadline)
  "Get the timestamp for SCHEDULE/DEADLINE.
Return org timestamp as (STRING BEGIN END)."
  (let* ((ctx (org-element-context))
         (ts (org-element-property schedule/deadline ctx)))
    (list
     (org-element-property :raw-value ts)
     (org-element-property :begin ts)
     (org-element-property :end ts))))

(defun tzc-org--schedule-or-deadline (schedule/deadline)
  "SCHEDULE/DEADLINE with timezone conversion on the fly.
SCHEDULE/DEADLINE can be `SCHEDULED' or `DEADLINE'."
  ;; Get date and time using org-read-date (which returns both date and time)
  (let* ((from-datetime (org-read-date nil t nil "Enter scheduled date and time: "))
	 ;; Parse the datetime to get all components
	 (org-time-stamp (format-time-string "<%Y-%m-%d %a %H:%M>" from-datetime))
	 ;; Get from-zone
	 (from-zone (completing-read "Enter From Zone: " (delete-dups (append (tzc--favourite-time-zones) (tzc--get-time-zones)))))
	 ;; Get to-zone
	 (to-zone (completing-read (format "Convert to zone: ") (delete-dups (append (tzc--favourite-time-zones) (tzc--get-time-zones)))))
	 ;; Add zoneinfo to the time-stamp
	 (org-time-stamp-with-zoneinfo (concat (string-replace ">" (concat " " from-zone) org-time-stamp) ">"))
	 ;; Convert the time-stamp using tzc
	 (converted-time-stamp (tzc-convert-org-time-stamp org-time-stamp-with-zoneinfo to-zone))
	 (ts))
    
    (setq ts (cond ((string-equal schedule/deadline "SCHEDULED") (tzc-org--get-planning-ts :scheduled))
		   ((string-equal schedule/deadline "DEADLINE") (tzc-org--get-planning-ts :deadline))))
    (if (nth 0 ts)
	(progn
	  (goto-char (nth 1 ts))
	  (delete-region (nth 1 ts) (nth 2 ts))
	  (insert converted-time-stamp " "))
      (org-back-to-heading t)
      (forward-line 1)
      (insert (format "%s: " schedule/deadline) converted-time-stamp " "))))

;;;###autoload
(defun tzc-org-schedule (&optional arg)
  "Schedule an org item with timezone conversion.
Similar to `org-schedule', but prompts for timezone conversion.
Prompts for date and time first, then asks for from-zone and to-zone,
converts the time, and inserts the result with the to-zone in the timestamp.
Optional argument ARG."
  (interactive "P")
  (tzc-org--schedule-or-deadline "SCHEDULED"))

(defun tzc-org-deadline (&optional arg)
  "Schedule an org item with timezone conversion.
Similar to `org-deadline', but prompts for timezone conversion.
Prompts for date and time first, then asks for from-zone and to-zone,
converts the time, and inserts the result with the to-zone in the timestamp.
Optional argument ARG."
  (interactive "P")
  (tzc-org--schedule-or-deadline "DEADLINE"))

(provide 'tzc-org)
;;; tzc-org.el ends here


