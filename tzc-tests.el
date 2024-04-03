(require 'ert)
(load-file "tzc.el")

(ert-deftest tzc-test-get-offset-lisbon ()
  (should (equal (mapcar (lambda (date) (tzc--get-offset "Europe/Lisbon" date)) (list "2024-03-25" "2024-04-05")) (list "+0000" "+0100"))))

(ert-deftest tzc-test-get-offset-los-angeles ()
  (should (equal (mapcar (lambda (date) (tzc--get-offset "America/Los_Angeles" date)) (list "2024-02-25" "2024-03-15")) (list "-0800" "-0700"))))

