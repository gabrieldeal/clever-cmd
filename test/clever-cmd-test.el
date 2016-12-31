;;; clever-cmd-test.el --- ERT tests!

;;; Commentary:

;; Flycheck is so picky.

;;; Code:

(load (expand-file-name "clever-cmd.el" default-directory))

(require 'el-mock) ; https://www.emacswiki.org/emacs/EmacsLispMock
(eval-when-compile (require 'cl)) ;; for el-mock

(ert-deftest replace-placeholders ()
  (with-mock
    (stub buffer-file-name => "/foo/bar.baz")
    (stub line-number-at-pos => 123)
    (should (equal (clever-cmd--replace-placeholders "")
		   ""))
    (should (equal (clever-cmd--replace-placeholders "% %% %d")
		   "% %% %d"))
    (should (equal (clever-cmd--replace-placeholders "start %s %s %l %l end")
		   "start /foo/bar.baz /foo/bar.baz 123 123 end"))
    (should (equal (clever-cmd--replace-placeholders "%%%s")
		   "%/foo/bar.baz"))
    (should (equal (clever-cmd--replace-placeholders "%%s %%l")
		   "%s %l"))
    (should (equal (clever-cmd--replace-placeholders "%%%%s %%%%l %%%%%%%%%%%%l")
		   "%%s %%l %%%%%%l"))))

(ert-deftest default-command/grep/empty-alist ()
  (let ((major-mode 'some-mode)
	(clever-cmd-grep-major-mode-alist '((fake-mode . "The fake-mode command"))))
    (should (equal (clever-cmd--default-command "grep" "The default command")
		   "The default command"))))

(ert-deftest default-command/grep/no-match ()
  (let ((major-mode 'some-mode))
    (should (equal (clever-cmd--default-command "grep" "The default command")
		   "The default command"))))

(ert-deftest default-command/grep/match/string ()
  (let ((major-mode 'fake-mode)
	(clever-cmd-grep-major-mode-alist '((fake-mode . "The fake-mode command"))))
    (should (equal (clever-cmd--default-command "grep" "The default command")
		   "The fake-mode command"))))

(ert-deftest default-command/grep/match/function ()
  (let* ((major-mode-function (lambda () "Return value of lambda"))
	 (clever-cmd-grep-major-mode-alist (list (cons major-mode major-mode-function))))
    (should (equal (clever-cmd--default-command "grep" "The default command")
		   "Return value of lambda"))))

(ert-deftest default-command/compile/match ()
  (with-mock
   (stub buffer-file-name => "/tmp/super-file")
   (let ((clever-cmd-compile-file-name-regexp-alist '(("super-file$" . "The super-file command"))))
     (should (equal (clever-cmd--default-command "compile" "The default command")
		    "The super-file command")))))

(ert-deftest read-shell-command/grep/with-placeholder ()
  (let ((clever-cmd-grep-file-name-regexp-alist '(("super-file$" . "The super-file command")))
	(history '("Command added by read-shell-command")))
    (with-mock
      (stub buffer-file-name => "/tmp/super-file")
      (mock (read-shell-command "Command: "
				"The super-file command"
				'history)
	    => "The command from the user %s")
      (should (equal (clever-cmd--read-shell-command "grep -rn " 'history "grep")
		     "The command from the user /tmp/super-file"))
      (should (equal history '("The command from the user /tmp/super-file"))))))

(provide 'clever-cmd-test)
;;; clever-cmd-test.el ends here
