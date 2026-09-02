;;; tzc.el --- Converts time between different time zones  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/tzc
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (org "9.5") (transient "0.3.7"))
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
;; (setq tzc-favourite-time-zones-alist '(("Asia/Kolkata" "Kolkata")
;; ("America/New_York" "New York") ("Europe/Berlin" "Berlin")))

;;; Code:
(require 'cl-lib)
(require 'timezone)
(require 'subr-x)
(require 'org)
(require 'org-element)
(require 'transient)

(defcustom tzc-color--time-zone-label "#98C379"
  "Color to indicate a time zone label."
  :type 'color
  :group 'tzc)
(defcustom tzc-color--time-string "#56B6C2"
  "Color to indicate a time string."
  :type 'color
  :group 'tzc)
(defcustom tzc-color--date-string "#C678DD"
  "Color to indicate a date string."
  :type 'color
  :group 'tzc)
(defcustom tzc-color--offset-string "#E5C07B"
  "Color to indicate an offset string."
  :type 'color
  :group 'tzc)

(defface tzc-face-time-zone-label
  `((t :foreground ,tzc-color--time-zone-label
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for time zone label."
  :group 'tzc-face)

(defface tzc-face-time-string
  `((t :foreground ,tzc-color--time-string
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for time string."
  :group 'tzc-face)

(defface tzc-face-date-string
  `((t :foreground ,tzc-color--date-string
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for date string."
  :group 'tzc-face)

(defface tzc-face-offset-string
  `((t :foreground ,tzc-color--offset-string
       :weight extra-bold
       :box nil
       :underline nil))
  "Face for offset string."
  :group 'tzc-face)

(defcustom tzc-use-date-in-world-clock t
  "Whether to use full date in world clock buffer."
  :type 'boolean
  :group 'tzc)

(defcustom tzc-use-offset-in-world-clock t
  "Whether to display offset in world clock buffer."
  :type 'boolean
  :group 'tzc)

(defcustom tzc-use-date-in-convert-time nil
  "Whether to use full date in when converting time."
  :type 'boolean
  :group 'tzc)

(defcustom tzc-favourite-time-zones-alist '(("Asia/Kolkata" "Kolkata")
					    ("UTC+0000" "UTC")
					    ("America/New_York" "New_York")
					    ("Europe/London" "London")
					    ("Europe/Berlin" "Berlin")
					    ("Asia/Shanghai" "Shanghai")
					    ("Asia/Tokyo" "Tokyo"))
  "Alist for favourite time zones containing time zone and label."
  :type '(repeat (list string string))
  :group 'tzc)

(defcustom tzc-home-time-zone (car (car tzc-favourite-time-zones-alist))
  "Home time zone to use as default when no time zone provided.
Can be also used to highlight it in the `tzc-world-clock'."
  :type 'string
  :group 'tzc)

(defun tzc--favourite-time-zones ()
  "Get the list of favourite time zones."
  (mapcar #'car tzc-favourite-time-zones-alist))

(defun tzc--closest-string (target candidates)
  "Return closest string in CANDIDATES to TARGET."
  (car
   (sort candidates
         (lambda (a b)
           (< (string-distance target a)
              (string-distance target b))))))


(defcustom tzc-main-dir (seq-find #'file-directory-p
				  '("/usr/share/zoneinfo.default/"
				    "/usr/share/zoneinfo/"
				    "/usr/lib/zoneinfo/"
				    "/etc/zoneinfo/")
				  "/usr/share/zoneinfo/")
  "Main directory to look for the zoneinfo data on your system."
  :type 'string
  :group 'tzc)

(defcustom tzc-areas '("Africa"
		       "America"
		       "Antarctica"
		       "Arctic"
		       "Asia"
		       "Atlantic"
		       "Australia"
		       "Brazil"
		       "Canada"
		       "Chile"
		       "Europe"
		       "Indian"
		       "Mexico"
		       "Pacific"
		       "US")
  "Areas to look for the time zone info."
  :type '(repeat string)
  :group 'tzc)

(defun tzc--get-time-zones ()
  "Get list of time zones from system."
  (let* ((zones '()))
    (dolist (area (and (stringp tzc-main-dir) tzc-areas))
      (let ((dir-path (expand-file-name area tzc-main-dir)))
	(when (file-directory-p dir-path)
	  (setq zones (append zones (mapcar (lambda (zone) (concat area "/" zone)) (directory-files dir-path nil directory-files-no-dot-files-regexp)))))))
    zones))

(defcustom tzc-time-zones nil
  "List of time zones.
When nil, the list is computed on first use from the zones in
`tzc-favourite-time-zones-alist' and the zoneinfo data found under
`tzc-main-dir'.  Use `tzc--time-zones' to read it rather than this
variable directly."
  :type '(choice (const :tag "Detect automatically" nil)
		 (repeat string))
  :group 'tzc)

(defvar tzc--time-zones-cache nil
  "Cached value of the detected time zones.  See `tzc--time-zones'.")

(defun tzc--time-zones ()
  "Return the list of known time zones.
Honours `tzc-time-zones' when set, otherwise detects them once and caches
the result."
  (or tzc-time-zones
      tzc--time-zones-cache
      (setq tzc--time-zones-cache
	    (delete-dups (append (tzc--favourite-time-zones)
				 (tzc--get-time-zones))))))

(defcustom tzc-world-clock-buffer-name "*tzc-wclock*"
  "Name of the `tzc-world-clock' buffer."
  :type 'string
  :group 'tzc)

(defun tzc--get-time-zone-label (time-zone)
  "Get the label for the TIME-ZONE."
  (cond
   ((null time-zone) "Local Time")
   ((member time-zone (tzc--favourite-time-zones))
    (nth 1 (assoc time-zone tzc-favourite-time-zones-alist)))
   ((string-match-p "\\`[A-Za-z]+\\'" time-zone)
    (user-error "%s is not a valid time zone.  Should be in the format Area/City!" time-zone))
   ((string-match-p "/" time-zone)
    (if (member time-zone (tzc--time-zones))
	(string-replace "_" " " (nth 1 (split-string time-zone "/")))
      (user-error "%s is not a recognized time zone.  Perhaps looking for %s!" time-zone
		  (tzc--closest-string time-zone (tzc--time-zones)))))
   (t time-zone)))

(defun tzc--+-position (timeshift)
  "Position of +- in a TIMESHIFT string."
  (or (string-match "+" timeshift) (string-match "-" timeshift)))

(defun tzc--format-time-shift (timeshift)
  "Convert a TIMESHIFT to proper format of +-HHMM."
  (let ((timeshiftstring (substring timeshift (tzc--+-position timeshift))))
    (cond ((= (length timeshiftstring) 3) (concat timeshiftstring "00"))
	  ((= (length timeshiftstring) 4) (concat timeshiftstring "0"))
	  (t timeshiftstring))))

(defun tzc--+-p (timeshift)
  "Check if the TIMESHIFT in contain +- string."
  (when (stringp timeshift)
    (string-match-p "\\`\\(?:[A-Z]+\\)?[-+][0-9]\\{2,4\\}\\'" timeshift)))

(defun tzc--get-offset (time-zone &optional date)
  "Get the time offset for TIME-ZONE on a given DATE."
  (if (tzc--+-p time-zone)
      (tzc--format-time-shift time-zone)
    (format-time-string "%z" (org-read-date nil t (or date (format-time-string "%F"))) time-zone)))

(defun tzc--get-time-shift-between-zones (from-zone to-zone &optional from-date)
  "Get the shift in time between FROM-ZONE and TO-ZONE.
Optionally provide FROM-DATE."
  (let* ((from-zone-offset (tzc--get-offset from-zone from-date))
	 (to-zone-offset (tzc--get-offset to-zone from-date)))
    (- (timezone-zone-to-minute to-zone-offset) (timezone-zone-to-minute from-zone-offset))))

(defun tzc--get-hour (time-string)
  "Get the hour from TIME-STRING.
Handle the 12-hour clock, where 12 AM is hour 0 and 12 PM is hour 12."
  (let* ((upcased (upcase time-string))
	 (hour (decoded-time-hour (parse-time-string time-string))))
    (cond ((string-match-p "PM" upcased) (if (= hour 12) 12 (+ hour 12)))
	  ((string-match-p "AM" upcased) (if (= hour 12) 0 hour))
	  (t hour))))

(defun tzc--get-hour-shift (from-zone to-zone &optional from-date)
  "Get the shift in hour between FROM-ZONE and TO-ZONE.
Optionally provide FROM-DATE."
  (/ (tzc--get-time-shift-between-zones from-zone to-zone from-date) 60))

(defun tzc--get-minute-shift (from-zone to-zone &optional from-date)
  "Get the shift in minute between FROM-ZONE and TO-ZONE.
Optionally provide FROM-DATE."
  (% (tzc--get-time-shift-between-zones from-zone to-zone from-date) 60))

(defun tzc--get-converted-time (time-string from-zone to-zone &optional from-date)
  "Convert a given time as given in TIME-STRING from FROM-ZONE to TO-ZONE.
Optionally provide FROM-DATE.
Returns a list of the form `(min hour day)`."
  (let* ((from-zone-hour (tzc--get-hour time-string))
	 (from-zone-minute (decoded-time-minute (parse-time-string time-string)))
	 (hour-shift (tzc--get-hour-shift from-zone to-zone from-date))
	 (minute-shift (tzc--get-minute-shift from-zone to-zone from-date))
	 (to-zone-hour (+ from-zone-hour hour-shift))
	 (to-zone-minute (+ from-zone-minute minute-shift))
	 (to-zone-day 0))
    (cond ((< to-zone-minute 0) (setq to-zone-minute (+ to-zone-minute 60)
				      to-zone-hour (1- to-zone-hour)))
	  ((>= to-zone-minute 60) (setq to-zone-minute (- to-zone-minute 60)
					to-zone-hour (1+ to-zone-hour))))
    (cond ((< to-zone-hour 0) (setq to-zone-hour (+ to-zone-hour 24)
				    to-zone-day (1- to-zone-day)))
	  ((>= to-zone-hour 24) (setq to-zone-hour (- to-zone-hour 24)
				      to-zone-day (1+ to-zone-day))))
    (list to-zone-minute to-zone-hour to-zone-day)))

(defun tzc--get-converted-time-string (time-string from-zone to-zone &optional use-date use-offset from-date)
  "Convert a given time as given in TIME-STRING from FROM-ZONE to TO-ZONE.
Optionally use FROM-DATE.
If USE-DATE is non-nil then the full date and day is shown,
otherwise only relative information is shown.  If USE-OFFSET is non-nil
then offset will be displayed."
  (unless (string-match-p ":" time-string)
    (user-error "Seems like the time is not specified in HH:MM format.  This might lead to
erroneous calculation.  Please use correct format for time!"))
  (let* ((to-zone-list (tzc--get-converted-time time-string from-zone to-zone from-date))
	 (minute (nth 0 to-zone-list))
	 (hour (nth 1 to-zone-list))
	 (day (nth 2 to-zone-list))
	 (to-time-string (format "%02d:%02d" hour minute))
	 (to-day-string "")
	 (offset-string ""))
    (if use-date
	(setq to-day-string (format-time-string " %a %d %B %Y" (time-add (current-time) (days-to-time day))))
      (setq to-day-string (cond
			   ((= day 0) "")
			   ((> day 0) (format " +%sD" day))
			   ((< day 0) (format " %sD" day)))))
    (when use-offset
      (setq offset-string (format " %s" (tzc--get-offset to-zone from-date))))
    (concat (propertize to-time-string 'face 'tzc-face-time-string)
	    (propertize to-day-string 'face 'tzc-face-date-string)
	    (propertize offset-string 'face 'tzc-face-offset-string))))

(defun tzc--time-list (time-zone)
  "A list of times to display for completion based on TIME-ZONE."
  (let* ((time-now (format-time-string "%R" nil time-zone))
	 (hour-now (string-to-number (format-time-string "%H" nil time-zone)))
	 (time-list-after (cl-loop for time in (number-sequence (1+ hour-now) 23)
				   collect (format "%02d:00" time)))
	 (time-list-before (cl-loop for time in (number-sequence 0 (1- hour-now))
				   collect (format "%02d:00" time))))
    (append (cons time-now time-list-after) time-list-before)))

;;;###autoload
(defun tzc-convert-time (time-string from-zone to-zone &optional from-date)
  "Convert a given time as given in TIME-STRING from FROM-ZONE to TO-ZONE.
Optionally on a given FROM-DATE."
  (interactive
   (let* ((time-string (completing-read "Enter time to convert: " (tzc--time-list tzc-home-time-zone)))
	  (from-zone (tzc--select-time-zone-with-preview-for-offset
		      (format "Enter time zone to convert %s from: " time-string)))
	  (to-zone (tzc--select-time-zone-with-preview-for-offset
		    (format "Enter time zone to convert %s from %s to: " time-string from-zone)
		    time-string
		    from-zone))
	  (from-date (org-read-date nil nil nil "Enter date to compute the conversion on: ")))
   (list time-string from-zone to-zone from-date)))
  (message (concat (propertize time-string 'face 'tzc-face-time-string) " "
		   (propertize (tzc--get-time-zone-label from-zone) 'face 'tzc-face-time-zone-label) " = "
		   (tzc--get-converted-time-string time-string from-zone to-zone tzc-use-date-in-convert-time nil from-date) " "
		   (propertize (tzc--get-time-zone-label to-zone) 'face 'tzc-face-time-zone-label))))

;;;###autoload
(defun tzc-convert-current-time (to-zone)
  "Convert current local time to TO-ZONE."
  (interactive (list (tzc--select-time-zone-with-preview-for-offset
		      (format "Enter time zone to convert %s from %s to: "
			      (format-time-string "%R")
			      (format-time-string "%Z"))
		      (format-time-string "%R")
		      (format-time-string "%z"))))
  (let ((time-now (format-time-string "%R")))
    (message (concat (propertize (concat "Local Time (" (format-time-string "%Z") ")")
				 'face 'tzc-face-time-zone-label)
		     " "
		     (propertize time-now 'face 'tzc-face-time-string)
		     " = "
		     (tzc--get-converted-time-string time-now nil to-zone tzc-use-date-in-convert-time)
		     " "
		     (propertize (tzc--get-time-zone-label to-zone) 'face 'tzc-face-time-zone-label)))))

;;;###autoload
(defun tzc-convert-time-to-favourite-time-zones (time-string from-zone from-date)
  "Convert time in TIME-STRING from FROM-ZONE to `(tzc--favourite-time-zones)`.
The conversion is computed for the given FROM-DATE."
  (interactive
   (let* ((from-zone (completing-read "Enter From Zone: " (tzc--time-zones)))
	  (time-string (completing-read "Enter time to covert: " (tzc--time-list from-zone)))
	  (from-date (org-read-date nil nil nil "Enter date to compute the conversion on: ")))
   (list time-string from-zone from-date)))
  (with-current-buffer (get-buffer-create "*tzc-times*")
    (erase-buffer)
    (insert (propertize time-string 'face 'tzc-face-time-string)
	    " "
	    (propertize
	     (tzc--get-time-zone-label from-zone) 'face 'tzc-face-time-zone-label)
	    " on " (propertize from-date 'face 'tzc-face-date-string))
    (dolist (to-zone (tzc--favourite-time-zones))
      (unless (string-equal to-zone from-zone)
	(insert " = " (tzc--get-converted-time-string
		       time-string
		       from-zone
		       to-zone
		       tzc-use-date-in-convert-time
		       tzc-use-offset-in-world-clock
		       from-date)
		" "
		(propertize
		 (tzc--get-time-zone-label to-zone) 'face 'tzc-face-time-zone-label) "\n")))
    (align-regexp (point-min) (point-max) "\\(\\s-*\\)=")
    (switch-to-buffer-other-window (current-buffer))))

;;;###autoload
(defun tzc-convert-current-time-to-favourite-time-zones ()
  "Convert current local time to `(tzc--favourite-time-zones)`."
  (interactive)
  (with-current-buffer (get-buffer-create tzc-world-clock-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (to-zone (tzc--favourite-time-zones))
	(insert (tzc--get-converted-time-string (format-time-string "%R") nil to-zone)
		" " (tzc--get-time-zone-label to-zone) "\n"))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)[0-9]\\{2\\}:"))
    (switch-to-buffer-other-window (current-buffer))))

(defun tzc--time-zone-format-error ()
"Error message to display for invalid time zone format."
(user-error
 "Invalid time zone format.
Use Area/City (e.g. Europe/London) or an offset such as UTC+0530 or GMT-0400!"))

(defun tzc--timestamp-time-zone-regexp ()
  "Regexp matching tzdata names or numeric offsets, including UT."
  (concat
   "\\("
   ;; tzdata name
   "[A-Za-z]+/[A-Za-z0-9_+\\-]+"
   "\\|"
   ;; UT / UTC / GMT / GM prefix with offset
   "\\(?:UTC\\|UT\\|GMT\\|GM\\)[-+][0-9]\\{1,2\\}\\(?::?[0-9]\\{1,2\\}\\)?"
   "\\|"
   ;; Raw offset (+HHMM, +HHM, +HH)
   "[-+][0-9]\\{1,2\\}\\(?::?[0-9]\\{1,2\\}\\)?"
   "\\)"))

(defun tzc--time-zone-annotation-function (time-zone &optional time from-zone)
  "Annotate time-zone TIME-ZONE with offset preview."
  (format "%s %s %s %s %s%s %s"
	  (propertize " " 'display `(space :align-to 30))
	  (propertize "→" 'face 'tzc-face-time-zone-label)
	  (propertize (format-time-string "%Z" (current-time) time-zone) 'face 'tzc-face-time-zone-label)
	  (propertize " " 'display `(space :align-to 20))
	  (propertize "UTC" 'face 'tzc-face-time-zone-label)
	  (propertize (tzc--get-offset time-zone) 'face 'tzc-face-offset-string)
	  (if (and time from-zone)
	      (tzc--get-converted-time-string time from-zone time-zone)
	    "")))

(defun tzc--select-time-zone-with-preview-for-offset (&optional describe time from-zone)
  "Prompt for a time-zone with offset preview.
Optional argument DESCRIBE for additional descreption in the prompt."
  (interactive)
  (let* ((time-zones (tzc--get-time-zones))
         (completion-extra-properties
	  `(:annotation-function
	    ,(lambda (tz)
	       (tzc--time-zone-annotation-function tz time from-zone)))))
    (completing-read (format "Select time zone: %s (default %s): "
			     (if describe
				 describe
			       "")
			     tzc-home-time-zone)
		     time-zones
		     nil t nil nil
		     tzc-home-time-zone)))

(defcustom tzc-world-clock-auto-update t
  "Whether the `tzc-world-clock' buffer refreshes itself every minute.
Only has an effect while the clock is showing the current time."
  :type 'boolean
  :group 'tzc)

(defvar-local tzc-world-clock--time nil
  "Time the `tzc-world-clock' buffer is showing, or nil for the current time.")

(defvar-local tzc-world-clock--zone nil
  "Time zone `tzc-world-clock--time' is expressed in, or nil for local time.")

(defvar-local tzc-world-clock--date nil
  "Date the `tzc-world-clock' buffer is showing, or nil for today.")

(defvar tzc-world-clock--timer nil
  "Timer refreshing the `tzc-world-clock' buffer.
See `tzc-world-clock-auto-update'.")

(defface tzc-face-button
  '((t :inherit button :weight normal))
  "Face for the buttons in the `tzc-world-clock' buffer."
  :group 'tzc-face)

(defun tzc-world-clock--showing-now-p ()
  "Return non-nil if the world clock is showing the current time."
  (and (null tzc-world-clock--time) (null tzc-world-clock--date)))

(defun tzc-world-clock--time-string ()
  "Return the HH:MM string the world clock is currently showing."
  (format-time-string "%R" tzc-world-clock--time))

(defun tzc-world-clock--insert-button (label command help)
  "Insert a button labelled LABEL running COMMAND, described by HELP."
  (insert-text-button label
		      'action (lambda (_) (call-interactively command))
		      'help-echo help
		      'follow-link t
		      'face 'tzc-face-button))

(defun tzc-world-clock--insert-toolbar ()
  "Insert the row of buttons at the top of the world clock buffer."
  (dolist (spec `(("[< prev]" tzc-world-clock-previous "Step back one hour")
		  ("[now]" tzc-world-clock-now "Show the current time")
		  ("[next >]" tzc-world-clock-next "Step forward one hour")
		  ("[time...]" tzc-world-clock-for-given-time "Show another date and time")
		  ("[+ zone]" tzc-world-clock-add-zone "Add a time zone")
		  (,(format "[date: %s]" (if tzc-use-date-in-world-clock "on" "off"))
		   tzc-world-clock-toggle-date "Show or hide the full date")
		  (,(format "[offset: %s]" (if tzc-use-offset-in-world-clock "on" "off"))
		   tzc-world-clock-toggle-offset "Show or hide the UTC offset")
		  ("[save]" tzc-world-clock-save-zones "Save the current zones for future sessions")
		  ("[quit]" quit-window "Close the world clock")))
    (tzc-world-clock--insert-button (nth 0 spec) (nth 1 spec) (nth 2 spec))
    (insert " "))
  (insert "\n")
  (insert (propertize
	   (if (tzc-world-clock--showing-now-p)
	       "now"
	     (format "%s %s"
		     (or tzc-world-clock--date (format-time-string "%F" tzc-world-clock--time))
		     (tzc-world-clock--time-string)))
	   'face 'tzc-face-date-string))
  (unless (tzc-world-clock--showing-now-p)
    (insert " ")
    (insert (propertize (tzc--get-time-zone-label tzc-world-clock--zone)
			'face 'tzc-face-time-zone-label)))
  (insert "\n\n"))

(defun tzc-world-clock--insert-zones ()
  "Insert one line per favourite time zone, each with a button to remove it."
  (let* ((zones (tzc--favourite-time-zones))
	 ;; Pad to a common width instead of `align-regexp', which would insert
	 ;; tabs and split labels that contain a space (e.g. "New York").
	 (width (apply #'max 0 (mapcar (lambda (z)
					 (string-width (tzc--get-time-zone-label z)))
				       zones))))
    (dolist (to-zone zones)
      (let ((label (tzc--get-time-zone-label to-zone)))
	(insert (propertize label 'face 'tzc-face-time-zone-label)
		(make-string (1+ (- width (string-width label))) ?\s)
		(tzc--get-converted-time-string
		 (tzc-world-clock--time-string)
		 tzc-world-clock--zone
		 to-zone
		 tzc-use-date-in-world-clock
		 tzc-use-offset-in-world-clock
		 tzc-world-clock--date)
		" ")
	(insert-text-button "[x]"
			    'action (lambda (_) (tzc-world-clock-remove-zone to-zone))
			    'help-echo (format "Remove %s from the world clock" to-zone)
			    'follow-link t
			    'face 'tzc-face-button)
	(insert "\n")))))

(defun tzc-world-clock--render ()
  "Redraw the world clock buffer, preserving point."
  (let ((inhibit-read-only t)
	(op (point)))
    (erase-buffer)
    (tzc-world-clock--insert-toolbar)
    (tzc-world-clock--insert-zones)
    (goto-char (min op (point-max)))))

(defun tzc-world-clock--refresh ()
  "Redraw the world clock if it is live and showing the current time."
  (let ((buffer (get-buffer tzc-world-clock-buffer-name)))
    (if (not (buffer-live-p buffer))
	(when tzc-world-clock--timer
	  (cancel-timer tzc-world-clock--timer)
	  (setq tzc-world-clock--timer nil))
      (with-current-buffer buffer
	(when (tzc-world-clock--showing-now-p)
	  (tzc-world-clock--render))))))

(defun tzc-world-clock--in-buffer ()
  "Return the live world clock buffer, or signal an error."
  (or (get-buffer tzc-world-clock-buffer-name)
      (user-error "No world clock buffer.  Use `tzc-world-clock' first")))

(defmacro tzc-world-clock--with-buffer (&rest body)
  "Run BODY in the world clock buffer and redraw it."
  (declare (indent 0) (debug t))
  `(with-current-buffer (tzc-world-clock--in-buffer)
     ,@body
     (tzc-world-clock--render)))

(defun tzc-world-clock-update (&optional _arg _noconfirm)
  "Update the `tzc-world-clock' buffer.
Used as the `revert-buffer-function' of `tzc-world-clock-mode'."
  (when (get-buffer tzc-world-clock-buffer-name)
    (tzc-world-clock--with-buffer)))

;;;###autoload
(defun tzc-world-clock-shift-hours (hours)
  "Shift the time shown in the `tzc-world-clock' buffer by HOURS."
  (interactive "nShift by how many hours: ")
  (tzc-world-clock--with-buffer
    (let* ((now-p (tzc-world-clock--showing-now-p))
	   ;; Steps land on whole hours, so from the current time the first step
	   ;; moves to the adjacent whole hour rather than keeping the minutes.
	   (base (org-read-date
		  nil t (format "%s %s:00"
				(or tzc-world-clock--date
				    (format-time-string "%F" tzc-world-clock--time))
				(format-time-string "%H" tzc-world-clock--time))))
	   (hours (if (and now-p (< hours 0)
			   (> (string-to-number (format-time-string "%M")) 0))
		      (1+ hours)
		    hours))
	   (shifted (time-add base (seconds-to-time (* 3600 hours)))))
      (setq tzc-world-clock--time shifted
	    tzc-world-clock--date (format-time-string "%F" shifted)))))

;;;###autoload
(defun tzc-world-clock-previous ()
  "Show the `tzc-world-clock' for the previous hour."
  (interactive)
  (tzc-world-clock-shift-hours -1))

;;;###autoload
(defun tzc-world-clock-next ()
  "Show the `tzc-world-clock' for the next hour."
  (interactive)
  (tzc-world-clock-shift-hours 1))

;;;###autoload
(defun tzc-world-clock-previous-or-next (previous-or-next)
  "Show the `tzc-world-clock' for the PREVIOUS-OR-NEXT hour.
PREVIOUS-OR-NEXT is the string \"previous\" or \"next\"."
  (tzc-world-clock-shift-hours (if (string-equal previous-or-next "previous") -1 1)))

;;;###autoload
(defun tzc-world-clock-now ()
  "Show the current time in the `tzc-world-clock' buffer."
  (interactive)
  (tzc-world-clock--with-buffer
    (setq tzc-world-clock--time nil
	  tzc-world-clock--zone nil
	  tzc-world-clock--date nil)))

;;;###autoload
(defun tzc-world-clock-add-zone (time-zone)
  "Add TIME-ZONE to the zones shown in the `tzc-world-clock' buffer."
  (interactive (list (tzc--select-time-zone-with-preview-for-offset
		      "Add time zone to the world clock: ")))
  (if (member time-zone (tzc--favourite-time-zones))
      (message "%s is already in the world clock" time-zone)
    (setq tzc-favourite-time-zones-alist
	  (append tzc-favourite-time-zones-alist
		  (list (list time-zone
			      (string-replace "_" " "
					      (car (last (split-string time-zone "/"))))))))
    (tzc-world-clock--with-buffer)))

(defun tzc-world-clock-remove-zone (time-zone)
  "Remove TIME-ZONE from the zones shown in the `tzc-world-clock' buffer."
  (interactive (list (completing-read "Remove time zone: " (tzc--favourite-time-zones) nil t)))
  (setq tzc-favourite-time-zones-alist
	(seq-remove (lambda (entry) (string-equal (car entry) time-zone))
		    tzc-favourite-time-zones-alist))
  (tzc-world-clock--with-buffer))

(defun tzc-world-clock-remove-zone-at-point ()
  "Remove the time zone on the current line from the world clock."
  (interactive)
  (let* ((line-number (- (line-number-at-pos) (line-number-at-pos (point-min))))
	 ;; The toolbar occupies the first three lines.
	 (index (- line-number 3))
	 (zones (tzc--favourite-time-zones)))
    (if (and (>= index 0) (< index (length zones)))
	(tzc-world-clock-remove-zone (nth index zones))
      (user-error "Point is not on a time zone line"))))

(defun tzc-world-clock-toggle-date ()
  "Toggle whether the world clock shows the full date."
  (interactive)
  (setq tzc-use-date-in-world-clock (not tzc-use-date-in-world-clock))
  (tzc-world-clock--with-buffer))

(defun tzc-world-clock-toggle-offset ()
  "Toggle whether the world clock shows the UTC offset."
  (interactive)
  (setq tzc-use-offset-in-world-clock (not tzc-use-offset-in-world-clock))
  (tzc-world-clock--with-buffer))

(defun tzc-world-clock-save-zones ()
  "Persist the current world clock zones in `tzc-favourite-time-zones-alist'."
  (interactive)
  (customize-save-variable 'tzc-favourite-time-zones-alist tzc-favourite-time-zones-alist)
  (message "Saved %d time zones" (length tzc-favourite-time-zones-alist)))

(defvar tzc-world-clock-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'tzc-world-clock-next)
    (define-key map "p" #'tzc-world-clock-previous)
    (define-key map "." #'tzc-world-clock-now)
    (define-key map "t" #'tzc-world-clock-for-given-time)
    (define-key map "a" #'tzc-world-clock-add-zone)
    (define-key map "k" #'tzc-world-clock-remove-zone-at-point)
    (define-key map "d" #'tzc-world-clock-toggle-date)
    (define-key map "o" #'tzc-world-clock-toggle-offset)
    (define-key map "s" #'tzc-world-clock-save-zones)
    (define-key map "g" #'tzc-world-clock-update)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `tzc-world-clock-mode'.")

(define-derived-mode tzc-world-clock-mode special-mode "tzc world clock"
  "Major mode for buffer that displays times in various time zones.
See `tzc-world-clock'.

\\{tzc-world-clock-mode-map}"
  :interactive nil
  (setq-local revert-buffer-function #'tzc-world-clock-update)
  (setq show-trailing-whitespace nil)
  (when tzc-world-clock-auto-update
    (unless tzc-world-clock--timer
      (setq tzc-world-clock--timer
	    (run-at-time t 60 #'tzc-world-clock--refresh)))
    (add-hook 'kill-buffer-hook
	      (lambda ()
		(when tzc-world-clock--timer
		  (cancel-timer tzc-world-clock--timer)
		  (setq tzc-world-clock--timer nil)))
	      nil t)))

;;;###autoload
(defun tzc-world-clock (&optional from-time from-zone from-date)
  "Display a world clock buffer for zones in `tzc-favourite-time-zones-alist'.
Optional argument FROM-TIME is the time to convert from, FROM-ZONE the zone
that time is in, and FROM-DATE the date to convert on.  With all three
omitted the clock shows the current time."
  (interactive)
  (let ((buffer (get-buffer-create tzc-world-clock-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tzc-world-clock-mode)
	(tzc-world-clock-mode))
      (setq tzc-world-clock--time from-time
	    tzc-world-clock--zone from-zone
	    tzc-world-clock--date from-date)
      (tzc-world-clock--render))
    (pop-to-buffer buffer)))

;;;###autoload
(defun tzc-world-clock-for-given-time ()
  "Show the `tzc-world-clock' for a time and date read from the minibuffer."
  (interactive)
  (let ((time+date (org-read-date t t nil "Enter time+date: ")))
    (tzc-world-clock time+date nil (format-time-string "%F" time+date))))

;;;###autoload
(defun tzc-check-time-zone (time-zone)
  "Check info for TIME-ZONE."
  (interactive "sEnter Time Zone: ")
  (if (member time-zone (tzc--get-time-zones))
      (progn
	(let* ((name (tzc--get-time-zone-label time-zone))
	       (offset (tzc--get-offset time-zone)))
	  (message "%s %s" name offset)))
    (message "%s is not a recognized time zone name." time-zone)))

;;;###autoload
(defun tzc-get-time-shift-between-zones (from-zone to-zone from-date)
  "Get time shift between FROM-ZONE and TO-ZONE.
Optionally on a given FROM-DATE."
  (interactive
   (let ((from-zone (completing-read "Enter from zone: " (tzc--time-zones)))
	 (to-zone (completing-read "Enter to zone: " (tzc--time-zones)))
	 (from-date (org-read-date nil nil nil "Enter Date to calculate the conversion: ")))
     (list from-zone to-zone from-date)))
  (when (string-equal from-zone to-zone)
    (user-error "You have enetered the same time zones!"))
  (let* ((from-zone-offset (tzc--get-offset from-zone from-date))
	 (to-zone-offset (tzc--get-offset to-zone from-date))
	 (offset (tzc--get-time-shift-between-zones from-zone to-zone from-date))
	 (hour-offset (number-to-string (tzc--get-hour-shift from-zone to-zone from-date)))
	 (minute-offset (number-to-string (tzc--get-minute-shift from-zone to-zone from-date)))
	 (from-zone-label (tzc--get-time-zone-label from-zone))
	 (to-zone-label (tzc--get-time-zone-label to-zone)))
    (message "%s is %s hours %s minutes %s %s. UTC offset for %s is %s and %s is %s."
	     (propertize to-zone-label 'face 'tzc-face-time-zone-label)
	     (propertize (string-replace "-" "" hour-offset) 'face 'tzc-face-time-string)
	     (propertize (string-replace "-" "" minute-offset) 'face 'tzc-face-time-string)
	     (if (> offset 0)
		 "ahead of"
	       "behind")
	     (propertize from-zone-label 'face 'tzc-face-time-zone-label)
	     (propertize from-zone-label 'face 'tzc-face-time-zone-label)
	     (propertize from-zone-offset 'face 'tzc-face-offset-string)
	     (propertize to-zone-label 'face 'tzc-face-time-zone-label)
	     (propertize to-zone-offset 'face 'tzc-face-offset-string))))

(transient-define-prefix tzc ()
  "TZC operations for Org timestamp at point."
  [:description
   (lambda () (format "TZC: %s" (format-time-string "%F %R")))

   ["Convert"
    ("c" "Convert current time" tzc-convert-current-time)
    ("t" "Convert time" tzc-convert-time)]

   ["Time Zone"
    ("s" "time shift between time zones" tzc-get-time-shift-between-zones)]

   ["World Clock"
    ("w" "View current time in world clock" tzc-world-clock)
    ("v" "View a time in world clock" tzc-world-clock-for-given-time)
    ("a" "Add a time zone" tzc-world-clock-add-zone)
    ("k" "Remove a time zone" tzc-world-clock-remove-zone)]

   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(provide 'tzc)
;;; tzc.el ends here
