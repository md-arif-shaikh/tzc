;;; tzc.el --- Converts time between different time zones  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/tzc
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
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
;;
;; `tzc-convert-time` to convert a given time from one time-zone to another
;; `tzc-convert-time-to-favourite-time-zones` to convert a given time from one
;; time-zone to a list of favourite time-zones.
;;
;; A list of favourite time zones could be set using like following
;; (setq tzc-favourite-time-zones '("Asia/Kolkata" "America/New_York" "Europe/Berlin"))

;;; Code:
(require 'timezone)

(defcustom tzc-favourite-time-zones '("Asia/Kolkata"
				      "America/New_York"
				      "UK/London"
				      "Europe/Berlin")
  "List of favourite time zones."
  :type 'list
  :group 'tzc)

(defcustom tzc-time-zones '("Asia/Kolkata"
			    "America/New_York"
			    "UK/London"
			    "Europe/Berlin")
  "List of time zones."
  :type 'list
  :group 'tzc)

(defun tzc--get-timeshift-between-zones (from-zone to-zone)
  "Get the shift in time between FROM-ZONE and TO-ZONE."
  (let* ((from-zone-offset (format-time-string "%z" nil from-zone))
	 (to-zone-offset (format-time-string "%z" nil to-zone)))
    (- (timezone-zone-to-minute to-zone-offset) (timezone-zone-to-minute from-zone-offset))))

(defun tzc--get-hour (time-string)
  "Get the hour from TIME-STRING."
  (let* ((hour (decoded-time-hour (parse-time-string time-string))))
    (if (string-search "PM" (upcase time-string))
	(+ hour 12)
      hour)))

(defun tzc--get-hour-shift (from-zone to-zone)
  "Get the shift in hour between FROM-ZONE and TO-ZONE."
  (/ (tzc--get-timeshift-between-zones from-zone to-zone) 60))

(defun tzc--get-minute-shift (from-zone to-zone)
  "Get the shift in minute between FROM-ZONE and TO-ZONE."
  (% (tzc--get-timeshift-between-zones from-zone to-zone) 60))

(defun tzc--get-converted-time (time-string from-zone to-zone)
  "Convert a given time as given in TIME-STRING from FROM-ZONE to TO-ZONE.
Returns a list of the form `(min hour day)`."
  (let* ((from-zone-hour (tzc--get-hour time-string))
	 (from-zone-minute (decoded-time-minute (parse-time-string time-string)))
	 (hour-shift (tzc--get-hour-shift from-zone to-zone))
	 (minute-shift (tzc--get-minute-shift from-zone to-zone))
	 (to-zone-hour (+ from-zone-hour hour-shift))
	 (to-zone-minute (+ from-zone-minute minute-shift))
	 (to-zone-day "+0d"))
    (cond ((< to-zone-minute 0) (setq to-zone-minute (+ to-zone-minute 60)
				      to-zone-hour (1- to-zone-hour)))
	  ((>= to-zone-minute 60) (setq to-zone-minute (- to-zone-minute 60)
					to-zone-hour (1+ to-zone-hour))))
    (cond ((< to-zone-hour 0) (setq to-zone-hour (+ to-zone-hour 24)
				    to-zone-day "-1d"))
	  ((>= to-zone-hour 24) (setq to-zone-hour (- to-zone-hour 24)
				      to-zone-day "+1d")))
    (list to-zone-minute to-zone-hour to-zone-day)))

(defun tzc--get-converted-timestring (time-string from-zone to-zone)
  "Convert a given time as given in TIME-STRING from FROM-ZONE to TO-ZONE."
  (unless (string-search ":" time-string)
    (error "Seems like the time is not specified in HH:MM format.  This might lead to
erroneous calculation.  Please use correct format for time!"))
  (let* ((to-zone-list (tzc--get-converted-time time-string from-zone to-zone))
	 (minute (nth 0 to-zone-list))
	 (hour (nth 1 to-zone-list))
	 (day (nth 2 to-zone-list))
	 (to-time-string (format "%02d:%02d" hour minute))
	 (to-day-string))
    (unless (string-equal day "+0d")
      (setq to-day-string (format " %s" day)))
    (concat time-string " " from-zone " = " to-time-string to-day-string " " to-zone)))

(defun tzc--time-list (time-zone)
  "A list of times to display for completion based on TIME-ZONE."
  (let* ((time-now (format-time-string "%H:%M" nil time-zone))
	 (hour-now (string-to-number (format-time-string "%H" nil time-zone)))
	 (time-list-after (cl-loop for time in (number-sequence (1+ hour-now) 23)
				   collect (format "%02d:00" time)))
	 (time-list-before (cl-loop for time in (number-sequence 0 (1- hour-now))
				   collect (format "%02d:00" time))))
    (append (cons time-now time-list-after) time-list-before)))

(defun tzc-convert-time (time-string from-zone to-zone)
  "Convert a given time as given in TIME-STRING from FROM-ZONE to TO-ZONE."
  (interactive
   (let* ((from-zone (completing-read "Enter From Zone: " tzc-time-zones))
	  (to-zone (completing-read "Enter To Zone: " tzc-time-zones))
	  (time-string (completing-read "Enter time to covert: " (tzc--time-list from-zone))))
   (list time-string from-zone to-zone)))
  (message (tzc--get-converted-timestring time-string from-zone to-zone)))

(defun tzc-convert-time-to-favourite-time-zones (time-string from-zone)
  "Convert time in TIME-STRING from FROM-ZONE to `tzc-favourite-time-zones`."
  (interactive
   (let* ((from-zone (completing-read "Enter From Zone: " tzc-time-zones))
	  (time-string (completing-read "Enter time to covert: " (tzc--time-list from-zone))))
   (list time-string from-zone)))
  (with-current-buffer (generate-new-buffer "*tzc-times*")
    (dolist (to-zone tzc-favourite-time-zones)
      (insert (concat (tzc--get-converted-timestring time-string from-zone to-zone) "\n")))
    (switch-to-buffer-other-window "*tzc-times*")))

(provide 'tzc)
;;; tzc.el ends here
