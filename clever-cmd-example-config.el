;;; clever-cmd-example-config.el --- Example clever-cmd configuration

;; Copyright (C) 2017 Gabriel M Deal
;; Author: Gabriel M Deal <gabriel.m.deal@gmail.com>
;; Version: 0.0.2
;; Package-Requires: ((clever-cmd "0.0.2") (emacs "24.4"))
;; Keywords: tools, unix, compilation-mode, grep
;; URL: https://github.com/gabrielmdeal/clever-cmd

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

;;; Gabriel's configuration for clever-cmd.

;;; To use this:
;;; (require 'clever-cmd-example-config)
;;; (clever-cmd-example-config-initialize)

;;; Code:

(require 'clever-cmd)

(defun clever-cmd-ec--cd-to-project-root-command()
  (concat  "cd " (or (clever-cmd-ec--vc-root-dir) ".")))

(defun clever-cmd-ec--vc-root-dir()
  (let ((root-dir (clever-cmd-ec--chomp (clever-cmd-ec--shell-command-to-string "git rev-parse --show-toplevel"))))
    (if root-dir
	(concat root-dir "/") ; Without the trailing slash, the diff buffer has nothing in it.
      nil)))

(defun clever-cmd-ec--shell-command-to-string(command)
  (with-temp-buffer
    (if	(= 0 (call-process-shell-command command nil (current-buffer)))
	(buffer-string)
      nil)))

(defun clever-cmd-ec--chomp(str)
  (if (and str (string-match "[\n\t\s]+\\'" str))
      (replace-match "" t t str)
    str))

(defun clever-cmd-ec--ruby-grep-command()
  (format "grep -nr --include=\"*.rb\" --include=\"*.erb\" --include=\"*.rake\" %s --regexp "
	  default-directory))

(defun clever-cmd-ec--javascript-grep-command()
  (format "grep -nr --exclude-dir generated --exclude-dir node_modules --include=\"*.js\" --include=\"*.jsx\" --include=\"*.es6\" %s --regexp "
	  default-directory))

(defun clever-cmd-ec--rspec-compile-command()
  (concat (clever-cmd-ec--cd-to-project-root-command)
	  " && bundle exec rspec  ~/config/.rspec_color.rb --format documentation %s:%l"))

(defun clever-cmd-ec--javascript-compile-command()
  (concat (clever-cmd-ec--cd-to-project-root-command)
	  " && yarn run test:watch # eslint:all"))

(defun clever-cmd-ec--ert-runner-compile-command()
  (concat (clever-cmd-ec--cd-to-project-root-command) " && cask exec ert-runner && bin/lint"))

;;;###autoload
(advice-add 'compile :around #'clever-cmd-compile-wrapper)

;;;###autoload
(advice-add 'grep :around #'clever-cmd-grep-wrapper)

;;;###autoload
(add-to-list 'clever-cmd-compile-major-mode-alist
	     '(js-mode . clever-cmd-ec--javascript-compile-command))

;;;###autoload
(add-to-list 'clever-cmd-compile-major-mode-alist
	     '(web-mode . clever-cmd-ec--javascript-compile-command))

;;;###autoload
(add-to-list 'clever-cmd-compile-file-name-regexp-alist
	     '("/specs/.*\\.rb$\\|_spec.rb$" . clever-cmd-ec--rspec-compile-command))

;;;###autoload
(add-to-list 'clever-cmd-compile-file-name-regexp-alist
	     '("\\<package\\.json$" . clever-cmd-ec--javascript-compile-command))

;;;###autoload
(add-to-list 'clever-cmd-grep-file-name-regexp-alist
	     '("\\<package\\.json$" . clever-cmd-ec--javascript-grep-command))

;;;###autoload
(add-to-list 'clever-cmd-compile-file-name-regexp-alist
	     '("/test/.*test.el$" . clever-cmd-ec--ert-runner-compile-command))

;;;###autoload
(add-to-list 'clever-cmd-grep-major-mode-alist
	     '(web-mode . clever-cmd-ec--javascript-grep-command))

;;;###autoload
(add-to-list 'clever-cmd-grep-major-mode-alist
	     '(js-mode . clever-cmd-ec--javascript-grep-command))

;;;###autoload
(add-to-list 'clever-cmd-grep-major-mode-alist
	     '(ruby-mode . clever-cmd-ec--ruby-grep-command))

(provide 'clever-cmd-example-config)
;;; clever-cmd-example-config.el ends here
