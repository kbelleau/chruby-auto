# chruby-auto.el
chruby-auto is an implementation of [Chruby](https://github.com/postmodern/chruby) with Chruby's `auto.sh` functionality, in Emacs.  

This package _requires_ reading a `.ruby-version` file within the current buffer's directory tree. In this way, it does not act as a complete Chruby implementation.

## Installation
Clone this repository:  
```sh
git clone https://github.com/kbelleau/chruby-auto.git
```

Either symlink or copy `chruby-auto.el` into a path that is loaded in your `init.el`. I typically use a symlink:  
```sh
ln -s chruby-auto/chruby-auto.el .emacs.d/lisp/notif.el
```

Ensure your path is loaded, and load `chruby-auto`:  
```elisp
(add-to-list 'load-path
             (concat user-emacs-directory "lisp/"))

(require 'chruby-auto)
```

## Usage
chruby-auto has one interactive function, also named `chruby-auto`.  

You must have a `.ruby-version` file somewhere in the current buffer's directory tree. `chruby-auto` will find the nearest `.ruby-version` file.

## Configuration
By default, chruby-auto will search for rubies in `~/.rubies` and `/opt/rubies`. If you need to configure other directories to search for rubies, you can use the `chruby-auto-rubies-dirs` variable. This variable is a list, so you can add rubies like so:  
```elisp
(setq chruby-auto-rubies-dirs '("/path/to/ruby-dir" "/path/to/ruby-dir2"))
```

Use a list, even if you are just adding a single directory:  
```elisp
(setq chruby-auto-rubies-dirs '("/usr/local/rubies"))
```

chruby-auto will search directories in `chruby-auto-rubies-dirs` _before_ searching the default directories.  

If you use a configuration function to hook into major modes, you can have that function run the `chruby-auto` function:  
```elisp
;; a simple ruby-config function and hook
(defun ruby-config ()
  (chruby-auto)
  (visual-line-mode -1)
  (inf-ruby-minor-mode 1)
  (setq truncate-lines t
        fill-column 80
        ruby-indent-tabs-mode nil
        ruby-indent-level 2)
  (flymake-mode 1)
  (company-mode 1))
(add-hook 'ruby-mode-hook #'ruby-config)

```

You can also just add chruby-auto as a single parameter hook:  
```elisp
(add-hook 'ruby-mode-hook #'chruby-auto)
```

Lastly, you can run the `chruby-auto` function at any time to have your in-tree `.ruby-version` file discovered and environment variables set.

## Acknowledgments
chruby-auto aims to be an implementation of [Chruby](https://github.com/postmodern/chruby) by [postmodern](https://github.com/postmodern). The methodology used to set environment variables in Chruby was influential in the methodology used in `chruby-auto.el`.
