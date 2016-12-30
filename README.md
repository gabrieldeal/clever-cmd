# clever-cmd.el

Emacs lisp package for customizing the default command used by `M-x
grep` and `M-x compile` based on major mode or a filename regexp.
Also replaces %s and %l with path and line number in the command.

# Installing

Download it from the UNIX shell:
```
$ cd YOUR-LISP-DIRECTORY
$ wget https://raw.github.com/gabrielmdeal/clever-cmd/master/clever-cmd.el

```

Then add this to your Emacs init file:
```
(add-to-list 'load-path (expand-file-name "YOUR-LISP-DIRECTORY"))
(require 'clever-cmd)
(advice-add 'compile :around #'clever-cmd-compile-wrapper)
(advice-add 'grep :around #'clever-cmd-grep-wrapper)
```

Then in your Emacs init file configure it to do something special for your special files:
```
;; Run bundle exec rspec when in a file whose name looks like a Ruby rspec:
(defun example-rspec-command()
	(format "cd %s && bundle exec rspec --format documentation %%s:%%l" (or (vc-root-dir) ".")))
(add-to-list 'clever-cmd-compile-file-name-regexp-alist '("/specs/\\|_spec.rb$" . example-rspec-command))

;; Run an NPM test script when the major mode is js-mode:
(add-to-list 'clever-cmd-compile-major-mode-alist '(js-mode . "npm run test"))
```

# Using

Use `M-x compile` or `M-x grep` just like normal.
