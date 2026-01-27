(require 'ert)
(load-file "tzc.el")

(ert-deftest tzc-test-get-offset-lisbon ()
  (should (equal (mapcar (lambda (date) (tzc--get-offset "Europe/Lisbon" date)) (list "2024-03-25" "2024-04-05")) (list "+0000" "+0100"))))

(ert-deftest tzc-test-get-offset-los-angeles ()
  (should (equal (mapcar (lambda (date) (tzc--get-offset "America/Los_Angeles" date)) (list "2024-02-25" "2024-03-15")) (list "-0800" "-0700"))))

;;;timestamp conversion
(ert-deftest tzc-test-timestamp-convesion ()
  (should (equal (mapcar (lambda (timestamp) (tzc-convert-org-time-stamp timestamp "Asia/Kolkata"))
			 (list "<2024-02-25 9:00 America/Los_Angeles>" "<2024-03-15 9:00 America/Los_Angeles>"))
		 (list "<2024-02-25 Sun 22:30 Asia/Kolkata>" "<2024-03-15 Fri 21:30 Asia/Kolkata>"))))
