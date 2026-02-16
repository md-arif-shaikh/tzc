;;; tzc-org.el --- Org mode integration for tzc  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Md Arif Shaikh

;; Author: Md Arif Shaikh <arifshaikh.astro@gmail.com>
;; Homepage: https://github.com/md-arif-shaikh/tzc
;; Keywords: convenience, time zone, org

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

;; Integration between tzc (time zone converter) and Org mode.
;; Provides `tzc-org-schedule' for scheduling org items with time zone conversion.
;; Provides `tzc-org-deadline' for scheduling org items with time zone conversion.

;;; Code:
(require 'tzc)
(require 'org-element)
(require 'transient)

(defcustom tzc-org-local-time-zone (format-time-string "%z" (current-time))
  "Default local time zone or offset to use when converting org timestamp."
  :type 'string
  :group 'tzc)

(cl-defun tzc-org--get-time-zone-from-timestamp (timestamp
					      &optional (check-time-zone t)
					      ask-for-tz-when-nil)
  "Return plist (:tz STRING :beg POS :end POS) if time zone exists in TIMESTAMP.
Optionally check validity of the time zone using CHECK-TIME-ZONE.
Optionally ask for time zone when not found using ASK-FOR-TZ-WHEN-NIL."
  (let* ((case-fold-search nil)
         ;; Only valid time zone tokens
         (tz-regexp (tzc--timestamp-time-zone-regexp))
	 (tz-plist (or
                    ;; Check for external timezone: (Timezone)
                    (when (string-match (concat "(\\(" tz-regexp "\\))") timestamp)
                      (list :tz (match-string 1 timestamp)
                            :beg (match-beginning 1)
                            :end (match-end 1)))
                    ;; Check for internal timezone: <... Timezone>
                    (when (string-match
			  (concat
			   "[0-9]\\{1,2\\}:[0-9]\\{2\\}"  ;; anchor: must appear after time
			   "[[:space:]]+"
			   tz-regexp)
			  timestamp)
		     (list :tz  (match-string 1 timestamp)
			   :beg (match-beginning 1)
			   :end (match-end 1)))))
	 (tz (plist-get tz-plist :tz)))
    (when (and (null tz) ask-for-tz-when-nil)
      (setq tz (tzc--select-time-zone-with-preview-for-offset
		(format "No time zone found in timestamp %s! Enter a time zone to convert from: " timestamp))))
    (when check-time-zone
      (setq tz (cond ((string-match-p "\\`[A-Za-z]+/[A-Za-z_]+\\'" tz)
		      (if (member tz tzc-time-zones)
			  tz
			(let* ((closest-tz (tzc--closest-string tz tzc-time-zones)))
			  (completing-read (format "%s is not a valid time zone.  Perhaps looking for %s?"
						   tz closest-tz)
					   tzc-time-zones nil t nil nil closest-tz))))
		     (t tz)))
      (setq tz-plist (plist-put tz-plist :tz tz)))
    tz-plist))

(defun tzc-org--get-timestamp-at-point ()
  "Return Org timestamp at point as (STRING BEGIN END)."
  (let* ((pos (point))
         (ctx (org-element-context))
         ts)
    ;; Case 1: real timestamp element
    (setq ts (org-element-lineage ctx '(timestamp) t))
    ;; Case 2: planning timestamps — choose by point location
    (when (and (null ts)
               (eq (org-element-type ctx) 'planning))
      (dolist (prop '(:scheduled :deadline :closed))
        (let ((p (org-element-property prop ctx)))
          (when (and p
                     (<= (org-element-property :begin p) pos)
                     (>= (org-element-property :end p) pos))
            (setq ts p)))))
    ;; Case 3: cursor on external timezone (Asia/Kolkata)
    (when (null ts)
      (save-excursion
        ;; If we are potentially on an external timezone
        (when (or (looking-at-p "[])]") ;; on closing bracket
                  (looking-at-p "[A-Za-z0-9_+-/]") ;; on timezone char
                  (looking-at-p "[ \t]") ;; on space
                  (max (point-min) (1- (point))))
          ;; Search backward for "> ("
          (when (re-search-backward ">[ \t]*(" (line-beginning-position) t)
             (goto-char (match-beginning 0))
             ;; Now at '>', so check context
             (let ((prev-ctx (org-element-context)))
               (cond
                ((eq (org-element-type prev-ctx) 'timestamp)
                 (setq ts prev-ctx))
                ((eq (org-element-type prev-ctx) 'planning)
                 ;; If it's a planning element, check if we are at the end of schedule/deadline/closed
                 (let ((pos (point)))
                   (dolist (prop '(:scheduled :deadline :closed))
                     (let ((p (org-element-property prop prev-ctx)))
                       (when (and p
                                  (<= (org-element-property :end p) (+ pos 2)) ;; relaxed check
                                  (>= (org-element-property :end p) pos))
                         (setq ts p))))))))))))

    (when ts
      (let ((raw-value (org-element-property :raw-value ts))
            (begin (org-element-property :begin ts))
            (end (org-element-property :end ts)))
        (save-excursion
          (goto-char end)
          (when (looking-at "[ \t]*(\\([^)]+\\))")
             (setq end (match-end 0))
             (setq raw-value (concat raw-value (match-string 0))))
          (list raw-value begin end))))))

;;;###autoload
(defun tzc-org-add-or-update-time-zone-in-timestamp-at-point (time-zone)
  "Add or update TIME-ZONE info for a timestamp at point."
  (interactive (list (tzc--select-time-zone-with-preview-for-offset)))
  (let* ((ts-list (or (tzc--get-timestamp-at-point) (user-error "No timestamp found at point")))
         (ts (nth 0 ts-list))
         (ts-begin (nth 1 ts-list))
         (ts-end (nth 2 ts-list))
         (tz-plist (tzc-org--get-time-zone-from-timestamp ts nil)))
    (if-let* ((tz (plist-get tz-plist :tz)))
        ;; If time zone exists → replace it
        (save-excursion
          (goto-char ts-begin)
          (when (search-forward tz ts-end t)
            (replace-match time-zone)))
      ;; Else → append new time zone after closing bracket
      (goto-char ts-end)
      (insert " (" time-zone ")"))))

;;;###autoload
(defun tzc-org-convert-time-at-point (to-zone)
  "Convert time at point to TO-ZONE."
  (interactive
   (list (completing-read "Enter To Zone:  " (tzc--get-time-zones))))
  (let* ((timestamp (or (nth 0 (tzc-org--get-timestamp-at-point))
                        (error "No timestamp found at point!")))
	 (parsed-list (parse-time-string timestamp))
	 (from-zone)
	 (hour)
	 (minute)
	 (day)
	 (month)
	 (year))
    (if (not (string-match-p ":" timestamp))
	(user-error "Seems like the time is not specified in HH:MM format.  This might lead to
erroneous calculation.  Please use correct format for time!")
      (setq hour (tzc--get-hour timestamp))
      (setq minute (decoded-time-minute parsed-list)))
    (when (not (string-match-p "\d{4}-\d{2}-\d{2}" timestamp))
      (setq timestamp (format "%s %s" (format-time-string "%F") timestamp))
      (setq parsed-list (parse-time-string timestamp)))
      (setq day (decoded-time-day parsed-list))
      (setq month (decoded-time-month parsed-list))
      (setq year (decoded-time-year parsed-list))
    (cond ((tzc--+-p timestamp)
	   (setq from-zone (tzc--format-time-shift timestamp)))
	  (t (setq from-zone (plist-get (tzc-org--get-time-zone-from-timestamp timestamp) :tz))))
    (tzc-convert-time (format "%02d:%02d" hour minute) from-zone to-zone (format "%04d-%02d-%02d" year month day))))

(defun tzc-org--time-zone-annotation-function-for-timestamp (time-zone timestamp)
  "Annotate time-zone TIME-ZONE for given TIMESTAMP.
TIMESTAMP is converted to TIME-ZONE."
  (let* ((timestamp (if (stringp timestamp)
			timestamp
		      (car timestamp)))
         (converted-timestamp (tzc-org-convert-timestamp timestamp time-zone)))
    (format "%s %s %s"
	    (propertize " " 'display `(space :align-to 30))
	    (propertize "→" 'face 'tzc-face-time-zone-label)
	    (propertize converted-timestamp 'face 'font-lock-keyword-face))))

(defun tzc-org--select-time-zone-with-preview-for-timestamp (timestamp &optional describe)
  "Prompt for a time-zone for TIMESTAMP with converted timestamps.
Optional argument DESCRIBE to use in the prompt."
  (interactive)
  (let* ((time-zones (tzc--get-time-zones))
         (completion-extra-properties
          (list :annotation-function (lambda (tz)
				       (tzc-org--time-zone-annotation-function-for-timestamp tz timestamp)))))
    (completing-read (format "Select time zone: %s" (if describe
							describe
						      ""))
		     time-zones)))

(defun tzc-org-convert-and-replace-time-at-mark (to-zone)
  "Convert time at point to TO-ZONE and replace it."
  (interactive
   (list (completing-read "Enter To Zone:  " (tzc--get-time-zones))))
  (let* ((timestamp-details (tzc-org--get-timestamp-at-point))
	 (beg)
	 (end))
    (if timestamp-details
	(setq beg (nth 1 timestamp-details)
	      end (nth 2 timestamp-details))
      (user-error "No org timestamp found at point!"))
    (let* ((converted-time-strings
	    (split-string (tzc-org-convert-time-at-point to-zone) " = "))
           (converted-time (nth 1 converted-time-strings)))
      (delete-region beg end)
      (insert converted-time))))

;;;; convert org timestamp
;;;###autoload
(defun tzc-org-convert-timestamp (timestamp to-zone)
  "Convert TIMESTAMP to TO-ZONE."
  (interactive
   (let* ((timestamp (read-string "Enter timestamp to convert: "))
	  (to-zone (completing-read (format "Convert %s to time zone:  " timestamp) (delete-dups (append (tzc--favourite-time-zones) (tzc--get-time-zones))))))
     (list timestamp to-zone)))
  (let* ((from-zone-exists-p (plist-get (tzc--get-time-zone-from-timestamp timestamp t t) :tz))
	 (from-zone (if from-zone-exists-p
			from-zone-exists-p
		      (completing-read "No Time Zone info found in the timestamp. Enter Time Zone of the current timestamp in Area/City format:  " (delete-dups (append (tzc--favourite-time-zones) (tzc--get-time-zones))))))
	 (parsed-time (org-parse-time-string timestamp))
	 (minute (nth 1 parsed-time))
	 (hour (nth 2 parsed-time))
	 (day (nth 3 parsed-time))
	 (month (nth 4 parsed-time))
	 (year (nth 5 parsed-time))
	 (converted-time (tzc--get-converted-time (format "%02d:%02d" hour minute) from-zone to-zone (format "%04d-%02d-%02d" year month day)))
	 (converted-min (nth 0 converted-time))
	 (converted-hour (nth 1 converted-time))
	 (converted-day)
	 (day-shift (nth 2 converted-time))
	 (shift (cond ((equal day-shift 1) "++1")
		      ((equal day-shift -1) "--1")
		      (t "++0")))
	 (converted-date (format-time-string "%F"
			  (org-read-date nil t shift nil (org-time-string-to-time (format "%04d-%02d-%02d" year month day)))))
	 (start-bracket (cond ((string-match-p "<" timestamp) "<")
			      ((string-match-p "\\[" timestamp) "[")
			      (t "")))
	 (end-bracket (cond ((string-match-p ">" timestamp) ">")
			    ((string-match-p "\\]" timestamp) "]")
			    (t ""))))
    (setq converted-day (format-time-string "%a" (org-time-string-to-time converted-date)))
    (message "%s%s %s %02d:%02d%s%s" start-bracket converted-date converted-day converted-hour converted-min
	     end-bracket
             (if from-zone-exists-p (concat " (" to-zone ")") "") )))

;;;###autoload
(defun tzc-org-convert-timestamp-at-point (to-zone &optional from-zone)
  "Convert `org-timestamp` at point to TO-ZONE.
Optional argument FROM-ZONE to use when not found at point."
  (interactive
   (let* ((timestamp (or (car (tzc-org--get-timestamp-at-point))
                         (error "No org timestamp found at point!")))
	  (tz-plist (tzc--get-time-zone-from-timestamp timestamp nil))
	  (from-zone (when (null tz-plist)
		       (tzc--select-time-zone-with-preview-for-offset
			(format "No time zone found in timestamp %s! Enter a time zone to convert from: " timestamp)))))
     (when (null tz-plist)
       (setq timestamp (concat timestamp " (" from-zone ")")))
     (list (tzc--select-time-zone-with-preview-for-timestamp
	    timestamp
	    (format "Convert current timestamp %s to time zone: " timestamp)) from-zone)))
  (let ((timestamp (car (tzc-org--get-timestamp-at-point))))
    (when from-zone
      (setq timestamp (concat timestamp " (" from-zone ")")))
    (tzc-org-convert-timestamp timestamp to-zone)))

;;;###autoload
(defun tzc-org-convert-and-replace-timestamp-at-point (to-zone &optional from-zone)
  "Convert `org-timestamp` at point to TO-ZONE and replace it.
Optional argument FROM-ZONE to use when not found at point."
  (interactive
   (let* ((timestamp (or (car (tzc-org--get-timestamp-at-point))
                         (error "No org timestamp found at point!")))
	  (tz-plist (tzc--get-time-zone-from-timestamp timestamp nil))
	  (from-zone (when (null tz-plist)
		       (tzc--select-time-zone-with-preview-for-offset
			(format "No time zone found in timestamp %s! Enter a time zone to convert from: " timestamp)))))
     (when (null tz-plist)
       (setq timestamp (concat timestamp " (" from-zone ")")))
     (list (tzc--select-time-zone-with-preview-for-timestamp
	    timestamp
	    (format "Convert current timestamp %s to time zone: " timestamp)) from-zone)))
  (let* ((timestamp-details (tzc-org--get-timestamp-at-point))
	 (timestamp)
	 (beg)
	 (end))
    (if timestamp-details
	(setq timestamp (nth 0 timestamp-details)
	      beg (nth 1 timestamp-details)
	      end (nth 2 timestamp-details))
      (user-error "No org timestamp found at point!"))
    (when from-zone
      (setq timestamp (concat timestamp " (" from-zone ")")))
    (let* ((converted-timestamp (tzc-org-convert-timestamp timestamp to-zone)))
      (delete-region beg end)
      (insert converted-timestamp))))

(defun tzc-org--get-planning-ts (schedule-or-deadline)
  "Get the timestamp for SCHEDULE-OR-DEADLINE.
Return org timestamp as (STRING BEGIN END)."
  (let* ((ctx (org-element-context))
         (ts (org-element-property schedule-or-deadline ctx)))
    (list
     (org-element-property :raw-value ts)
     (org-element-property :begin ts)
     (org-element-property :end ts))))

(defun tzc-org--schedule-or-deadline (schedule-or-deadline)
  "SCHEDULE-OR-DEADLINE with time zone conversion on the fly.
SCHEDULE-OR-DEADLINE can be SCHEDULED or DEADLINE."
  ;; Get date and time using org-read-date (which returns both date and time)
  (let* ((from-datetime (org-read-date nil t nil (format "Enter %s date and time: " schedule-or-deadline)))
	 ;; Parse the datetime to get all components
	 (org-timestamp (format-time-string "<%F %a %R>" from-datetime))
	 ;; Get from-zone
	 (from-zone (completing-read (format "Enter a time zone or UTC offset (default %s): " tzc-org-local-time-zone)
				     (delete-dups (append (tzc--favourite-time-zones) (tzc--get-time-zones)))
				     nil t nil nil tzc-org-local-time-zone))
	 ;; Get to-zone
	 (to-zone (completing-read (format "Convert %s from %s to time zone or UTC offset (default %s): "
					   org-timestamp from-zone tzc-org-local-time-zone)
				     (delete-dups (append (tzc--favourite-time-zones) (tzc--get-time-zones)))
				     nil t nil nil tzc-org-local-time-zone))
	 ;; Add zoneinfo to the timestamp
	 (org-timestamp-with-zoneinfo (format "%s (%s)" org-timestamp from-zone))
	 ;; Convert the timestamp using tzc
	 (converted-timestamp (tzc-convert-org-timestamp org-timestamp-with-zoneinfo to-zone))
	 (ts))
    
    (setq ts (cond ((string-equal schedule-or-deadline "SCHEDULED") (tzc-org--get-planning-ts :scheduled))
		   ((string-equal schedule-or-deadline "DEADLINE") (tzc-org--get-planning-ts :deadline))))
    (if (nth 0 ts)
	(progn
	  (goto-char (nth 1 ts))
	  (delete-region (nth 1 ts) (nth 2 ts))
	  (insert converted-timestamp " "))
      (org-back-to-heading t)
      (forward-line 1)
      (insert (format "%s: " schedule-or-deadline) converted-timestamp " "))))

;;;###autoload
(defun tzc-org-schedule ()
  "Schedule an org item with time zone conversion.
Similar to `org-schedule', but prompts for time zone conversion.
Prompts for date and time first, then asks for from-zone and to-zone,
converts the time, and inserts the result with the to-zone in the timestamp.
Optional argument ARG."
  (interactive)
  (tzc-org--schedule-or-deadline "SCHEDULED"))

;;;###autoload
(defun tzc-org-deadline ()
  "Schedule an org item with time zone conversion.
Similar to `org-deadline', but prompts for time zone conversion.
Prompts for date and time first, then asks for from-zone and to-zone,
converts the time, and inserts the result with the to-zone in the timestamp.
Optional argument ARG."
  (interactive)
  (tzc-org--schedule-or-deadline "DEADLINE"))

;;;world clock for time at point
(defun tzc-org-world-clock-for-timestamp-at-point ()
"Get a `world-clock' for the timestamp at point."
  (interactive)
  ;;; remove existing world clock
  (when (get-buffer tzc-world-clock-buffer-name)
    (kill-buffer tzc-world-clock-buffer-name))
  (let* ((timestamp (nth 0 (tzc--get-timestamp-at-point))))
    (tzc-world-clock (org-time-string-to-time timestamp)
		     (plist-get (tzc-org--get-time-zone-from-timestamp timestamp) :tz))))

(transient-define-prefix tzc-org ()
  "TZC operations for Org timestamp at point."
  [:description
   (lambda () (format "TZC: %s" (nth 0 (tzc--get-timestamp-at-point))))

   ["Convert"
    ("c" "Convert (keep original)" tzc-org-convert-timestamp-at-point)
    ("r" "Convert (replace)" tzc-org-convert-and-replace-timestamp-at-point)]

   ["Time Zone"
    ("m" "modify (add or update) time zone" tzc-org-add-or-update-time-zone-in-timestamp-at-point)]

   ["Schedule"
    ("s" "Schedule" tzc-org-schedule)
    ("d" "Deadline" tzc-org-deadline)]

   ["Inspect"
    ("v" "View in world clock" tzc-org-world-clock-for-timestamp-at-point)]

   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(defvar tzc-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-x z") #'tzc-org)
    map)
  "Keymap for `tzc-org-mode'.")

(defvar tzc-org-font-lock-keywords
  `((,(lambda (limit)
        (let ((case-fold-search nil))
          (re-search-forward (concat ">[ \t]*(\\(?:" (tzc--timestamp-time-zone-regexp) "\\))") limit t)))
     0 'tzc-face-time-zone-label prepend))
  "Font lock keywords for tzc-org.")

;;;###autoload
(define-minor-mode tzc-org-mode
  "Minor mode for TZC org features."
  :lighter " TZC"
  :keymap tzc-org-mode-map
  (if tzc-org-mode
      (font-lock-add-keywords nil tzc-org-font-lock-keywords)
    (font-lock-remove-keywords nil tzc-org-font-lock-keywords))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure))))

(provide 'tzc-org)
;;; tzc-org.el ends here


