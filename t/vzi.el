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
    (insert "#+LET: var4 (lambda (x y) (when x y))\n")
    (insert "#+SKIP\n")
    (let ((var3 nil))
      (vzi-eval-with-let-headers
       (progn
         (should (equal var3 '("sexp3" "value3")))
         (should (equal (funcall var4 t 5) 5)))))))

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

(ert-deftest test-vzi--extract-table-with-select ()
  "Test the table extraction with row selection."
  (with-temp-buffer
    (insert "| 1 | apple  | red    |\n")
    (insert "| 2 | banana | yellow |\n")
    (insert "| 3 | grape  | purple |\n")
    (forward-line -1)
    (let ((vzi-row-select (lambda (row i) (when (< i 2) row))))
      (should
       (equal
        (vzi--extract-table)
        "1\tapple\tred\n")))
    (let ((vzi-row-select (lambda (row _i) (when (equal (nth 1 row) "banana") row))))
      (should
       (equal
        (vzi--extract-table)
        "2\tbanana\tyellow\n")))))
