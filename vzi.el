;;; vzi.el --- Pipe data from emacs to the browser -*- lexical-binding: t -*-

;; Copyright (C) 2023 Jared Flatow

;; Author: Jared Flatow <jared@convex.io>
;; Created: 1 Aug 2023
;; Homepage: https://github.com/jflatow/vzi.el
;; Keywords: extensions
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-3.0-only
;; Version: prerelease

;;; Commentary:

;; Pipe data from emacs to the browser (and produce visual reports).

;;; Code:

(require 'ansi-color)
(require 'org)

(defgroup vzi-flags nil
  "Flags to vzi process."
  :group 'convenience)

(defcustom vzi-file-args ()
  "The optional file (code) arguments."
  :type '(sequence string)
  :group 'vzi-flags)

(defcustom vzi-flag-browser-bind nil
  "The browser-bind flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-browser-host nil
  "The browser-host flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-browser-path nil
  "The browser-path flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-browser-port nil
  "The browser-port flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-cli nil
  "The cli flag (e.g. \"render_event = (ev, doc) => Sky.$(doc.body).hl(ev)\")."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-defines ()
  "The user defined flags (e.g. '(\"c=2\"))."
  :type '(sequence string)
  :group 'vzi-flags)

(defcustom vzi-flag-headless nil
  "The headless flag."
  :type '(choice boolean (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-keep-alive nil
  "The keep-alive flag."
  :type '(choice boolean (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-let-die nil
  "The let-die flag."
  :type '(choice boolean (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-module nil
  "The (builtin) module flag (e.g. \"hist\", \"scatter\", etc)."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-no-output nil
  "The no-output flag."
  :type '(choice boolean (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-output nil
  "The output flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-page-token nil
  "The page-token flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-page-uri nil
  "The page-uri flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-separator nil
  "The separator flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defcustom vzi-flag-verbosity nil
  "The verbosity flag."
  :type '(choice string (const nil))
  :group 'vzi-flags)

(defgroup vzi-behaviors nil
  "Behaviors of the vzi.el commands."
  :group 'convenience)

(defcustom vzi-keys
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'vzi-send-raw)
    (define-key map (kbd "t") 'vzi-send-table)
    (define-key map (kbd "w") 'vzi-buffer-to-webkit)
    map)
  "A customizable keymap for vzi."
  :type 'keymap
  :group 'vzi-behaviors)

(defcustom vzi-focus-report nil
  "Whether or not to focus the report window after a call."
  :type 'boolean
  :group 'vzi-behaviors)

(defcustom vzi-focus-errors t
  "Whether or not to focus the errors window after a failure."
  :type 'boolean
  :group 'vzi-behaviors)

(defcustom vzi-let-header-bind-pattern "^#\\+LET: \\([^ ]+\\) \\(.*\\)$"
  "Pattern to match LET header lines, capturing the variable name and value."
  :type 'string
  :group 'vzi-behaviors)

(defcustom vzi-let-header-skip-pattern "^\\(\\s-*\\|#\\+.*\\)$"
  "Pattern to match lines to skip when searching for a LET header block."
  :type 'string
  :group 'vzi-behaviors)

(defcustom vzi-row-select nil
  "Function used to select rows when extracting a table: (row, i) => row | nil."
  :type 'function
  :group 'vzi-behaviors)

(defmacro vzi-eval-with-let-headers (&rest body)
  "Evaluate BODY within a let block with variables defined by LET headers."
  `(eval (list 'let (vzi--extract-let-headers) '(progn ,@body))))

(defmacro vzi-with-live-buffer (buffer &rest body)
  "Execute BODY within the buffer as current, if live, otherwise ignore.
Also ensures BODY executes with `read-only-mode' inhibited."
  (declare (indent 1))
  `(when (buffer-live-p ,buffer)
     (with-current-buffer ,buffer
       (let ((inhibit-read-only t))
         ,@body))))

(defun vzi--stderr-filter (proc string)
  "Handle the stderr of a vzi process appropriately."
  (vzi-with-live-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert (ansi-color-apply string))
    (newline)))

(defun vzi--stdout-filter (proc string)
  "Handle the stdout of a vzi process appropriately."
  (vzi-with-live-buffer (process-buffer proc)
    (insert string)))

(defun vzi--stderr-sentinel (_proc _event)
  (ignore))

(defun vzi--stdout-sentinel (proc event)
  (message "vzi process: %s" (string-trim-right event "\n"))
  (cond
   ((string-match "exited abnormally" event)
    (vzi-with-live-buffer (process-get proc :stderr)
      (when-let* ((focus? vzi-focus-errors)
                  (buffer (current-buffer))
                  (window (display-buffer-use-some-window buffer ())))
        (set-window-point window (point-max)))))
   ((string-match "finished" event)
    (vzi-with-live-buffer (process-buffer proc)
      (when-let* ((focus? vzi-focus-report)
                  (buffer (current-buffer))
                  (window (display-buffer-use-some-window buffer ())))
        (set-window-point window (point-min)))))))

(defun vzi--make-command-args ()
  "Build the command args from the flag variables."
  (let ((args (cons nil (copy-sequence vzi-file-args))))
    (when vzi-flag-browser-bind
      (nconc args (list "-B" vzi-flag-browser-bind)))
    (when vzi-flag-browser-host
      (nconc args (list "-R" vzi-flag-browser-host)))
    (when vzi-flag-browser-path
      (nconc args (list "-b" vzi-flag-browser-path)))
    (when vzi-flag-browser-port
      (nconc args (list "-p" vzi-flag-browser-port)))
    (when vzi-flag-cli
      (nconc args (list "-c" vzi-flag-cli)))
    (when vzi-flag-defines
      (nconc args (mapcan (lambda (str) (list "-d" str)) vzi-flag-defines)))
    (when vzi-flag-headless
      (nconc args (list "-H")))
    (when vzi-flag-keep-alive
      (nconc args (list "-K")))
    (when vzi-flag-let-die
      (nconc args (list "-L")))
    (when vzi-flag-module
      (nconc args (list "-m" vzi-flag-module)))
    (when vzi-flag-no-output
      (nconc args (list "-O")))
    (when vzi-flag-output
      (nconc args (list "-o" vzi-flag-output)))
    (when vzi-flag-page-token
      (nconc args (list "-P" vzi-flag-page-token)))
    (when vzi-flag-page-uri
      (nconc args (list "-u" vzi-flag-page-uri)))
    (when vzi-flag-separator
      (nconc args (list "-s" vzi-flag-separator)))
    (when vzi-flag-page-token
      (nconc args (list "-V" vzi-flag-verbosity)))
    (delq nil args)))

(defun vzi--extract-let-headers ()
  "Extract LET headers backwards in the current buffer."
  (let ((headers nil)
        (bind-pattern vzi-let-header-bind-pattern)
        (skip-pattern vzi-let-header-skip-pattern))
    (catch 'bobp
      (save-excursion
        (beginning-of-line)
        (while (and (not (bobp))         ;; Skip irrelevant lines
                    (looking-at skip-pattern)
                    (not (looking-at bind-pattern)))
          (forward-line -1))
        (while (looking-at bind-pattern) ;; Collect LET headers
          (let ((var (match-string-no-properties 1))
                (val (match-string-no-properties 2)))
            (push (list (intern var) (car (read-from-string val))) headers))
          (when (bobp)
            (throw 'bobp nil))
          (forward-line -1))))
    headers))

(defun vzi--extract-table ()
  "Extract the current org table at point with formatting removed."
  (if (org-at-table-p)
      (let ((table
             (save-excursion
               (goto-char (org-table-begin))
               (org-table-to-lisp)))
            (row-select (or vzi-row-select (lambda (row _i) row)))
            (row-index 0))
        (with-temp-buffer
          (dolist (row table)
            (when (listp row)  ;; to avoid table separators (strings)
              (setq row-index (1+ row-index))
              (when-let (selected-row (funcall row-select row row-index))
                (insert (mapconcat 'identity row "\t") "\n"))))
          (buffer-substring-no-properties (point-min) (point-max))))
    (user-error "Not at an org table")))

(defun vzi-send-raw (&optional start end)
  "Send the region or current line to vzi."
  (interactive)
  (let* ((start (or start (if (use-region-p) (region-beginning) (line-beginning-position))))
         (end (or end (if (use-region-p) (region-end) (line-end-position))))
         (raw (buffer-substring-no-properties start end))
         (data (if (string-suffix-p "\n" raw) raw (concat raw "\n")))
         (stdout (get-buffer-create "*vzi report*"))
         (stderr (get-buffer-create "*vzi errors*"))
         (command (cons "vzi" (vzi--make-command-args)))
         (process
          (make-process
           :name "vzi"
           :buffer stdout
           :stderr stderr
           :command command
           :filter #'vzi--stdout-filter
           :sentinel #'vzi--stdout-sentinel))
         (stderr-process (get-buffer-process stderr)))
    (set-process-filter stderr-process #'vzi--stderr-filter)
    (set-process-sentinel stderr-process #'vzi--stderr-sentinel)
    (with-current-buffer stderr
      (view-mode 1)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "Running: " (mapconcat 'identity command " "))
        (newline)))
    (with-current-buffer stdout
      (view-mode 1)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (process-put process :stderr stderr)
    (process-send-string process data)
    (process-send-eof process)))

(defun vzi-send-table ()
  "Send the org table at point to vzi."
  (interactive)
    (save-excursion
      (goto-char (org-table-begin))
      (forward-line -1)
      (vzi-eval-with-let-headers
       (forward-line 1)
       (let ((data (vzi--extract-table)))
          (with-temp-buffer
            (insert data)
            (vzi-send-raw (point-min) (point-max)))))))

(defun vzi-buffer-to-webkit (&optional buffer)
  "Visit the contents of the buffer using `xwidget-webkit-browse-url'."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (data-url-prefix "data:text/html;base64,")
         (buffer-contents
          (with-current-buffer buffer (encode-coding-string (buffer-string) 'utf-8)))
         (data-url
          (concat data-url-prefix (base64-encode-string buffer-contents t))))
    (xwidget-webkit-browse-url data-url)))

(provide 'vzi)

;;; vzi.el ends here