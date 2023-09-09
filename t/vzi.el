(require 'vzi)
(require 'ert)

(ert-deftest test-vzi-flags ()
  "Test the command flags."
  (let ((vzi-flag-cli "render_event ..."))
    (should (equal (vzi--make-command-args) '("-c" "render_event ..."))))
  (let ((vzi-flag-headless t)
        (vzi-flag-module "hist"))
    (should (equal (vzi--make-command-args) '("-H" "-m" "hist"))))
  (let ((vzi-flag-page-token "1")
        (vzi-flag-verbosity "2")
        (vzi-flag-page-uri "about:blank")
        (vzi-file-args '("a-file")))
    (should (equal (vzi--make-command-args) '("a-file" "-P" "1" "-u" "about:blank" "-V" "2"))))
  (let ((vzi-flag-defines '("c=2" "d=3"))
        (vzi-flag-output "/dev/null"))
    (should (equal (vzi--make-command-args) '("-d" "c=2" "-d" "d=3" "-o" "/dev/null")))))

(ert-deftest test-eval-with-let-headers ()
  "Test the headers extraction."
  (with-temp-buffer
    (insert "#+LET: var1 '(\"sexp1\" \"value1\")\n")
    (insert "#+LET: var2 '(\"sexp2\" \"value2\")\n")
    (insert "Some other text here\n")
    (insert "#+LET: var3 '(\"sexp3\" \"value3\")\n")
    (let ((var3 nil))
      (vzi-eval-with-let-headers
       `(should (equal var3 '("sexp3" "value3")))))))

(ert-deftest test-vzi--extract-table ()
  "Test the table extraction."
  (with-temp-buffer
    (insert "| 1 | apple  | red    |\n")
    (insert "| 2 | banana | yellow |\n")
    (insert "| 3 | grape  | purple |\n")
    (forward-line -1)
    (should
     (equal
      (vzi--extract-table)
      "1\tapple\tred\n2\tbanana\tyellow\n3\tgrape\tpurple\n"))))
