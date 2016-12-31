;;; clever-cmd.el --- Cleverize things that run commands (compile, grep)

;; Copyright (C) 2016 Gabriel M Deal
;; Author: Gabriel M Deal <gabriel.m.deal@gmail.com>
;; Version: 0.0.1
;; Package-Requires:
;; Keywords: tools, unix, compilation-mode, grep
;; URL:

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Choose default grep/compile commands based on the major mode or the file name.
;; Commands can be created dynamically using callbacks.
;; Substitute %s and %l with the path and line number in commands.

;;; Code:

(defvar clever-cmd-compile-file-name-regexp-alist
  '()
  "Alist containing a filename pattern and a function/string.
Each element looks like (REGEXP . FUNCTION) or (REGEXP . STRING).
In the former, the function is called to determine the compile
command.  In the latter, the string is used as the compile command.

Used by `clever-cmd-compile-wrapper'.

Example:
\(add-to-list 'clever-cmd-grep-file-name-regexp-alist
	     '(\"\\\\<package\\\\.json$\" . \"npm run test\"))
")

(defvar clever-cmd-grep-file-name-regexp-alist
  '()
  "Alist containing a filename pattern and a function/string.
Each element looks like (REGEXP . FUNCTION) or (REGEXP . STRING).
In the former, the function is called to determine the grep command.
In the latter, the string is used as the grep command.

Used by `clever-cmd-grep-wrapper'.")

(defvar clever-cmd-compile-major-mode-alist
  '()
  "Alist containing a filename pattern and a function/string.
Each element looks like (MAJOR-MODE . FUNCTION) or (MAJOR-MODE . STRING).
In the former, the function is called to determine the compile command.
In the latter, the string is used as the compile command.

Used by `clever-cmd-compile-wrapper'.")

(defvar clever-cmd-grep-major-mode-alist
  '()
  "Alist containing a filename pattern and a function/string.
Each element looks like (MAJOR-MODE . FUNCTION) or (MAJOR-MODE . STRING).
In the former, the function is called to determine the grep command.
In the latter, the string is used as the grep command.

Used by `clever-cmd-grep-wrapper'.")

(defun clever-cmd--find-command(command-type
				key-comparator-type
				key-comparator-func)
  (let* ((comparison-alist-name (format "clever-cmd-%s-%s-alist"
					command-type
					key-comparator-type))
	 (comparison-alist (symbol-value (intern comparison-alist-name)))
	 (command))
    (while (and (not command) comparison-alist)
      (let* ((comparison-assoc (car comparison-alist))
	     (key (car comparison-assoc))
	     (potential-command (cdr comparison-assoc)))
	(setq comparison-alist (cdr comparison-alist))
	(if (funcall key-comparator-func key)
	    (setq command potential-command))))
    command))

(defun clever-cmd--find-command-from-major-mode-alist(command-type current-major-mode)
  (clever-cmd--find-command command-type
			    "major-mode"
			    (lambda (candidate-major-mode)
			      (equal candidate-major-mode current-major-mode))))

(defun clever-cmd--find-command-from-file-name-regexp-alist(command-type file-name)
  (clever-cmd--find-command command-type
			    "file-name-regexp"
			    (lambda (regexp) (string-match-p regexp file-name))))

(defun clever-cmd--default-grep-command()
  (unless grep-command
    (grep-compute-defaults))
  grep-command)

(defun clever-cmd--default-command(command-type default)
  (let ((command (or (and (buffer-file-name)
			  (clever-cmd--find-command-from-file-name-regexp-alist command-type (buffer-file-name)))
		     (clever-cmd--find-command-from-major-mode-alist command-type major-mode)
		     default)))
    (cond ((stringp command) command)
	  ((functionp command) (funcall command))
	  (t default))))

(defun clever-cmd--create-regexp-replacement(matched-text replacement-text)
  (let* ((format-char (substring matched-text -1))
	 (percent-chars (substring matched-text 0 -1))
	 (new-format-string (concat percent-chars "s")))
    (if (evenp (length percent-chars))
	(concat (substring percent-chars 0 (/ (length percent-chars) 2))
		format-char)
      (format (concat percent-chars "s") replacement-text))))

(defun clever-cmd--replace-placeholders(command-template)
  (let ((filename (if (buffer-file-name) (buffer-file-name) ""))
	(line-number-str (number-to-string (line-number-at-pos)))
	(command command-template)
	(create-line-replacement (lambda (matched-text)
				   (clever-cmd--create-regexp-replacement matched-text line-number-str)))
	(create-file-replacement (lambda (matched-text)
				   (clever-cmd--create-regexp-replacement matched-text filename))))
    (setq command (replace-regexp-in-string "%+l" create-line-replacement command))
    (setq command (replace-regexp-in-string "%+s" create-file-replacement command))
    command))

(defun clever-cmd--read-shell-command(default-default-command command-history command-type)
  (let* ((default-command (clever-cmd--default-command command-type default-default-command))
	 (command-from-user-with-placeholders (read-shell-command "Command: "
								  default-command
								  command-history))
	 (command-from-user (clever-cmd--replace-placeholders command-from-user-with-placeholders)))
    (if (not (string= command-from-user command-from-user-with-placeholders))
	;; Save the file and line number in history instead of the placeholder:
	(set command-history (push command-from-user (cdr (symbol-value command-history)))))
    command-from-user))

;;;###autoload
(defun clever-cmd-compile-wrapper(orig-fun &rest args)
  "Uses `clever-cmd-compile-file-name-regexp-alist' and
`clever-cmd-compile-major-mode-alist' to determine the default
 command. Then prompts the user for an override command.

%s in the command is replaced with the current buffer's filename.
%l is replaced with the current line number.

Install it like this:
\(advice-add 'compile :around #'clever-cmd-compile-wrapper)
"
  (interactive (list (clever-cmd--read-shell-command compile-command 'compile-history "compile")))
  (apply orig-fun args))

;;;###autoload
(defun clever-cmd-grep-wrapper(orig-fun &rest args)
  "Uses `clever-cmd-grep-file-name-regexp-alist' and
`clever-cmd-grep-major-mode-alist' to determine the default
command. Then prompts the user for an override command.

Install it like this:
\(advice-add 'grep :around #'clever-cmd-grep-wrapper)"
  (interactive (list (clever-cmd--read-shell-command (clever-cmd--default-grep-command) 'grep-history "grep")))
  (apply orig-fun args))

(provide 'clever-cmd)
;;; clever-cmd.el ends here
