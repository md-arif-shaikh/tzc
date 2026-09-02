(require 'ert)
(load-file "tzc.el")
(load-file "tzc-org.el")

(ert-deftest tzc-test-get-offset-lisbon ()
  (should (equal (mapcar (lambda (date) (tzc--get-offset "Europe/Lisbon" date)) (list "2024-03-25" "2024-04-05")) (list "+0000" "+0100"))))

(ert-deftest tzc-test-get-offset-los-angeles ()
  (should (equal (mapcar (lambda (date) (tzc--get-offset "America/Los_Angeles" date)) (list "2024-02-25" "2024-03-15")) (list "-0800" "-0700"))))

;;;timestamp conversion
(ert-deftest tzc-test-timestamp-convesion ()
  (should (equal (mapcar (lambda (timestamp) (tzc-org-convert-timestamp timestamp "Asia/Kolkata"))
			 (list "<2024-02-25 9:00> (America/Los_Angeles)" "<2024-03-15 9:00> (America/Los_Angeles)"))
		 (list "<2024-02-25 Sun 22:30> (Asia/Kolkata)" "<2024-03-15 Fri 21:30> (Asia/Kolkata)"))))

(ert-deftest tzc-test-timestamp-convesion-plus ()
  (should (equal (mapcar (lambda (timestamp) (tzc-org-convert-timestamp timestamp "+0530"))
			 (list "<2024-02-25 9:00> (America/Los_Angeles)" "<2024-03-15 9:00> (America/Los_Angeles)"))
		 (list "<2024-02-25 Sun 22:30> (+0530)" "<2024-03-15 Fri 21:30> (+0530)"))))

(ert-deftest tzc-test-timestamp-conversion-kolkata-seoul ()
  (should (equal (tzc-org-convert-timestamp "<2025-01-15 10:00> (Asia/Kolkata)" "Asia/Seoul") "<2025-01-15 Wed 13:30> (Asia/Seoul)")))

(ert-deftest tzc-test-timestamp-conversion-kolkata-seoul-plus ()
  (should (equal (tzc-org-convert-timestamp "<2025-01-15 10:00> (+0530)" "Asia/Seoul") "<2025-01-15 Wed 13:30> (Asia/Seoul)")))

;;; 12-hour clock parsing
(ert-deftest tzc-test-get-hour-24-hour-clock ()
  (should (equal (mapcar #'tzc--get-hour (list "00:30" "09:15" "13:45" "23:59"))
		 (list 0 9 13 23))))

(ert-deftest tzc-test-get-hour-am ()
  (should (equal (mapcar #'tzc--get-hour (list "12:30 AM" "12:00 am" "01:15 AM" "11:45 AM"))
		 (list 0 0 1 11))))

(ert-deftest tzc-test-get-hour-pm ()
  (should (equal (mapcar #'tzc--get-hour (list "12:30 PM" "12:00 pm" "01:15 PM" "11:45 PM"))
		 (list 12 12 13 23))))

(ert-deftest tzc-test-convert-noon-and-midnight ()
  "Noon and midnight on the 12-hour clock must convert like their 24-hour forms."
  (should (equal (tzc-org-convert-timestamp "<2025-01-15 12:00> (Asia/Kolkata)" "Asia/Seoul")
		 "<2025-01-15 Wed 15:30> (Asia/Seoul)"))
  (should (equal (tzc-org-convert-timestamp "<2025-01-15 00:00> (Asia/Kolkata)" "Asia/Seoul")
		 "<2025-01-15 Wed 03:30> (Asia/Seoul)")))

;;; time zone extraction from a timestamp
(ert-deftest tzc-test-get-time-zone-from-timestamp-external ()
  (should (equal (plist-get (tzc-org--get-time-zone-from-timestamp
			     "<2025-01-15 Wed 10:00> (Asia/Kolkata)")
			    :tz)
		 "Asia/Kolkata")))

(ert-deftest tzc-test-get-time-zone-from-timestamp-offset ()
  (should (equal (plist-get (tzc-org--get-time-zone-from-timestamp
			     "<2025-01-15 Wed 10:00> (+0530)")
			    :tz)
		 "+0530")))

(ert-deftest tzc-test-get-time-zone-from-timestamp-missing ()
  "A timestamp without a time zone returns nil rather than signalling."
  (should (equal (plist-get (tzc-org--get-time-zone-from-timestamp
			     "<2025-01-15 Wed 10:00>")
			    :tz)
		 nil))
  (should (equal (tzc-org--get-time-zone-from-timestamp "<2025-01-15 Wed 10:00>" nil)
		 nil)))

;;; every function called is actually defined: catches call sites left
;;; behind by a rename, which `fboundp' on the definitions would not.
(ert-deftest tzc-test-no-undefined-function-calls ()
  (let* ((byte-compile-dest-file-function
	  (lambda (_) (make-temp-file "tzc-test-" nil ".elc")))
	 (warnings '()))
    (dolist (file '("tzc.el" "tzc-org.el"))
      (with-current-buffer (get-buffer-create byte-compile-log-buffer)
	(erase-buffer))
      (byte-compile-file file)
      (with-current-buffer byte-compile-log-buffer
	(goto-char (point-min))
	(while (re-search-forward "is not known to be defined" nil t)
	  (push (buffer-substring (line-beginning-position) (line-end-position))
		warnings))))
    (should (equal warnings nil))))

;;; time zone list
(ert-deftest tzc-test-time-zones-honours-custom-value ()
  (let ((tzc-time-zones '("Asia/Kolkata" "Europe/Berlin"))
	(tzc--time-zones-cache nil))
    (should (equal (tzc--time-zones) '("Asia/Kolkata" "Europe/Berlin")))))

(ert-deftest tzc-test-time-zones-detected-and-cached ()
  (let ((tzc-time-zones nil)
	(tzc--time-zones-cache nil))
    (should (member "Asia/Kolkata" (tzc--time-zones)))
    ;; second call is served from the cache
    (should (eq (tzc--time-zones) tzc--time-zones-cache))))

(ert-deftest tzc-test-get-time-zones-without-zoneinfo-dir ()
  "A missing or unset zoneinfo directory yields no zones rather than an error."
  (should (equal (let ((tzc-main-dir nil)) (tzc--get-time-zones)) nil))
  (should (equal (let ((tzc-main-dir "/nonexistent/zoneinfo/")) (tzc--get-time-zones)) nil)))

;;; convert-timestamp returns its value without messaging
(ert-deftest tzc-test-convert-timestamp-is-quiet ()
  "Non-interactive conversion returns the string without touching the echo area."
  (let (messaged)
    (cl-letf (((symbol-function 'message)
	       (lambda (&rest args) (setq messaged args) nil)))
      (should (equal (tzc-org-convert-timestamp "<2025-01-15 10:00> (Asia/Kolkata)" "Asia/Seoul")
		     "<2025-01-15 Wed 13:30> (Asia/Seoul)"))
      (should (equal messaged nil)))))

;;; output buffers are reused rather than duplicated
(ert-deftest tzc-test-favourite-time-zones-buffer-is-reused ()
  "Repeated conversions reuse one buffer and show the latest result."
  (dolist (name (list "*tzc-times*" tzc-world-clock-buffer-name))
    (when (get-buffer name) (kill-buffer name)))
  (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'identity))
    (tzc-convert-time-to-favourite-time-zones "10:00" "Asia/Kolkata" "2025-01-15")
    (tzc-convert-time-to-favourite-time-zones "11:00" "Asia/Kolkata" "2025-01-15")
    (tzc-convert-current-time-to-favourite-time-zones)
    (tzc-convert-current-time-to-favourite-time-zones))
  ;; No "*tzc-times*<2>" style duplicates.
  (should (equal (seq-filter (lambda (b) (string-match-p "<[0-9]+>\\'" (buffer-name b)))
			     (buffer-list))
		 nil))
  (with-current-buffer "*tzc-times*"
    (should (string-prefix-p "11:00" (buffer-substring-no-properties (point-min) (point-max))))))

;;; world clock rendering and controls
(defmacro tzc-test--with-world-clock (&rest body)
  "Run BODY in a freshly rendered world clock buffer with known zones."
  (declare (indent 0))
  `(let ((tzc-favourite-time-zones-alist '(("Asia/Kolkata" "Kolkata")
					   ("America/New_York" "New York")
					   ("Europe/London" "London")))
	 (tzc-use-date-in-world-clock t)
	 (tzc-use-offset-in-world-clock t)
	 (tzc-world-clock-auto-update nil))
     (when (get-buffer tzc-world-clock-buffer-name)
       (kill-buffer tzc-world-clock-buffer-name))
     (save-window-excursion (tzc-world-clock))
     (unwind-protect
	 (with-current-buffer tzc-world-clock-buffer-name ,@body)
       (when (get-buffer tzc-world-clock-buffer-name)
	 (kill-buffer tzc-world-clock-buffer-name)))))

(defun tzc-test--buffer-lines ()
  "Return the current buffer's lines as a list of strings."
  (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))

(defun tzc-test--zone-lines ()
  "Return the world clock's time zone lines, skipping the toolbar."
  (seq-filter (lambda (l) (string-match-p "[0-9]\\{2\\}:[0-9]\\{2\\}" l))
	      (nthcdr 3 (tzc-test--buffer-lines))))

(defun tzc-test--buttons ()
  "Return every button in the current buffer, in order."
  (let ((buttons '())
	;; `next-button' skips a button starting exactly at POS, so seed the
	;; walk with the one at point-min when there is one.
	(pos (if (button-at (point-min)) (point-min) (next-button (point-min)))))
    (while pos
      (let ((button (button-at pos)))
	(push button buttons)
	(setq pos (next-button (button-end button)))))
    (nreverse buttons)))

(defun tzc-test--button-label (button)
  "Return the label of BUTTON."
  (buffer-substring-no-properties (button-start button) (button-end button)))

(defun tzc-test--button-labels ()
  "Return the labels of every button in the current buffer."
  (mapcar #'tzc-test--button-label (tzc-test--buttons)))

(ert-deftest tzc-test-world-clock-renders-a-line-per-zone ()
  (tzc-test--with-world-clock
    (should (equal (length (tzc-test--zone-lines)) 3))
    ;; Labels containing a space survive the column padding intact.
    (should (string-match-p "\\`New York +[0-9]\\{2\\}:[0-9]\\{2\\}"
			    (nth 1 (tzc-test--zone-lines))))
    ;; Padding uses spaces, not the tabs `align-regexp' used to insert.
    (should-not (string-match-p "\t" (buffer-string)))))

(ert-deftest tzc-test-world-clock-has-controls ()
  (tzc-test--with-world-clock
    (let ((labels (tzc-test--button-labels)))
      (dolist (label '("[< prev]" "[now]" "[next >]" "[+ zone]" "[save]" "[quit]"))
	(should (member label labels)))
      ;; One removal button per zone.
      (should (equal (length (seq-filter (lambda (l) (equal l "[x]")) labels)) 3)))))

(ert-deftest tzc-test-world-clock-steps-in-whole-hours ()
  (tzc-test--with-world-clock
    (should (tzc-world-clock--showing-now-p))
    (tzc-world-clock-next)
    (should-not (tzc-world-clock--showing-now-p))
    (let ((after-next (tzc-world-clock--time-string)))
      (should (string-suffix-p ":00" after-next))
      (tzc-world-clock-next)
      ;; Two steps forward from the same start is two hours later.
      (should (equal (tzc-world-clock--time-string)
		     (format-time-string
		      "%R" (time-add (org-read-date nil t after-next) 3600))))
      (tzc-world-clock-previous)
      (should (equal (tzc-world-clock--time-string) after-next))
      (tzc-world-clock-now)
      (should (tzc-world-clock--showing-now-p)))))

(ert-deftest tzc-test-world-clock-toggles ()
  (tzc-test--with-world-clock
    (should (string-match-p "\\[date: on\\]" (buffer-string)))
    (tzc-world-clock-toggle-date)
    (should (string-match-p "\\[date: off\\]" (buffer-string)))
    ;; With the date hidden the zone lines carry only a time and an offset.
    (should (string-match-p "\\`Kolkata +[0-9]\\{2\\}:[0-9]\\{2\\} [-+][0-9]\\{4\\} \\[x\\]\\'"
			    (car (tzc-test--zone-lines))))
    (tzc-world-clock-toggle-offset)
    (should (string-match-p "\\[offset: off\\]" (buffer-string)))
    (should (string-match-p "\\`Kolkata +[0-9]\\{2\\}:[0-9]\\{2\\} \\[x\\]\\'"
			    (car (tzc-test--zone-lines))))))

(ert-deftest tzc-test-world-clock-add-and-remove-zones ()
  (tzc-test--with-world-clock
    (tzc-world-clock-remove-zone "Europe/London")
    (should (equal (length (tzc-test--zone-lines)) 2))
    (should-not (string-match-p "London" (buffer-string)))
    (tzc-world-clock-add-zone "Asia/Tokyo")
    (should (equal (length (tzc-test--zone-lines)) 3))
    (should (string-match-p "Tokyo" (buffer-string)))
    ;; Adding a zone that is already shown is a no-op.
    (tzc-world-clock-add-zone "Asia/Tokyo")
    (should (equal (length (tzc-test--zone-lines)) 3))))

(ert-deftest tzc-test-world-clock-remove-zone-button ()
  "Pushing a removal button drops that zone."
  (tzc-test--with-world-clock
    (goto-char (point-min))
    (let ((button (seq-find (lambda (b) (equal "[x]" (tzc-test--button-label b)))
			    (tzc-test--buttons))))
      (should button)
      (button-activate button)
      (should (equal (length (tzc-test--zone-lines)) 2))
      (should-not (string-match-p "Kolkata" (buffer-string))))))

(ert-deftest tzc-test-world-clock-reuses-its-buffer ()
  (tzc-test--with-world-clock
    (save-window-excursion (tzc-world-clock))
    (should (equal (seq-filter (lambda (b) (string-match-p "tzc-wclock.*<[0-9]+>" (buffer-name b)))
			       (buffer-list))
		   nil))))
