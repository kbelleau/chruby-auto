;;; chruby-auto.el --- chruby-auto function in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Kyle Belleau

;; Author: Kyle Belleau <kylejbelleau@gmail.com>
;; URL: https://github.com/kbelleau/chruby-auto
;; Keywords: ruby chruby environment

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; chruby-auto is an implementation of Chruby's auto.sh by postmodern
;; [https://github.com/postmodern/chruby] for Emacs.
;; It behaves in a similar manner to Chruby's auto.sh functionality,
;; setting the exact same environment variables in Emacs.
;; Select the desired Ruby by stating it in a file named '.ruby-version'
;; inside of your buffers directory tree.

;;; Code:
(defcustom chruby-auto-rubies-dirs nil
  "Define custom directories to search for rubies.
By default, $HOME/.rubies and /opt/rubies are searched."
  :type '(repeat directory))

(defun chruby-auto--find-all-rubies (user-rubies-dirs)
  "Finds all Ruby root directories in common and user defined locations"
  ;; ARG: (list) `user-rubies-dirs' is the global variable
  ;;      `chruby-auto-rubies-dirs' which can be user assigned in init.el

  ;; declare local variables
  (let ((static-rubies-dirs (list (expand-file-name "~/.rubies")
                              "/opt/rubies" "/usr/bin"))
        (all-rubies-dirs ())
        (rubies-root-dirs ()))

    ;; combine and `static-rubies-dirs' with `user-rubies-dirs' for a complete
    ;; list of directories to check for rubies.
    ;; `user-rubies-dirs' should be lower index than `static-rubies-dirs'
    (dolist (dir (append user-rubies-dirs static-rubies-dirs))
      (unless (member dir all-rubies-dirs)
        (push dir all-rubies-dirs)))

    ;; iterate through each item in `all-rubies-dirs', ensuring the dir exists
    (dolist (ruby-dir all-rubies-dirs)
      (when (file-exists-p ruby-dir)
        ;; chomp all text before last / (including the /)
        ;; search for a match of "ruby-#.#.#"
        (let ((version-dirs (directory-files ruby-dir)))
          (dolist (version-dir version-dirs)
            (when (string-match-p
                   "\\(ruby-[0-9]+\\.[0-9]+\\.[0-9]+\\)"
                   version-dir)
              ;; add valid rubies root dirs to `rubies-root-dirs'
              (push (concat ruby-dir "/" version-dir) rubies-root-dirs))))))

      ;;check if rubies-root-dirs is empty
      (if (null rubies-root-dirs)
          (progn
            (message "[chruby-auto] error: no rubies found")
            ;; return nil if failed
            nil)

    ;; RET: (list) valid ruby root directories
    rubies-root-dirs)))

(defun chruby-auto--find-ruby-version-file ()
  "Searches the directory tree for a file named .ruby-version file."
  ;; ARG: none

  ;; assign local var `dir' to the directory containing the .ruby-version file
  (let ((dir
         (locate-dominating-file default-directory ".ruby-version")))

    ;; RET: (string) full path to the closest .ruby-version file in the
    ;;      buffers directory tree
    (if dir
        (concat dir ".ruby-version")
      ;; if .ruby-version not found, return nil
      (message "[chruby-auto] error: `.ruby-version' file not found")
      nil)))

(defun chruby-auto--read-ruby-version-file (ruby-version-file)
  "Reads the .ruby-version file and returns its contents."
  ;; ARG: (string) file path of nearest .ruby-version file

  ;; RET: (string) contents of the ruby-version-file
  (when ruby-version-file
    (with-temp-buffer
      (insert-file-contents ruby-version-file)
      (string-trim (buffer-string)))))

(defun chruby-auto--validate-ruby-version (rubies-dirs ruby-version)
  "Validates the specified .ruby-version is a found ruby on the system."
  ;; ARG #1: (list) list of found rubies
  ;; ARG #2: (string) ruby version specified in found .ruby-version file

  (catch 'match
    (dolist (dir rubies-dirs)
      (let ((dir-version (car (last (split-string dir "/")))))
        (when (string= dir-version ruby-version)
          ;; RET: (string) the matching directory
          (throw 'match dir))))

    ;; IF FAILED: returns nil
    (message "[chruby-auto] error: could not find ruby %s installed"
             ruby-version)
    nil))

(defun chruby-auto--gather-environment (auto-ruby)
  "Gathers Ruby environment variables to set in our Emacs environment."

  ;; ARG: (string) path to the directory containing the Ruby we are using
  (let* ((ruby-interp (concat auto-ruby "/bin/ruby")))
    (if (file-exists-p ruby-interp)
        (let* ((cmd (format "%s -e \"ruby_engine = Object.const_defined?(:RUBY_ENGINE) ? RUBY_ENGINE : 'ruby'; \
ruby_version = RUBY_VERSION; \
gem_root = begin require 'rubygems'; Gem.default_dir rescue nil end; \
puts [ruby_engine, ruby_version, gem_root].join(' ')\"" ruby-interp))
               (output (string-trim (shell-command-to-string cmd)))
               (elements (split-string output " " t)))
          ;; collect the gatherer script output and prep data for return
          (when (= (length elements) 3)
            (let* ((ruby-engine (nth 0 elements))
                   (ruby-version (nth 1 elements))
                   (gem-root (nth 2 elements))
                   (gem-home (concat (expand-file-name "~")
                                     "/.gem/" ruby-engine "/" ruby-version))
                   (gem-path (concat gem-home ":" gem-root)))
              ;; RET: (list) all chruby environment variables
              ;;      (note that `auto-ruby' will become RUBY_ROOT)
              (list
               auto-ruby ruby-engine ruby-version gem-home gem-root gem-path))))
      ;; if ruby interpreter is not found, return nil and message error
      (message "[chruby-auto] error: %s/bin/ruby not found" auto-ruby)
      nil)))

(defun chruby-auto--set-environment
    (ruby-root ruby-engine ruby-version gem-home gem-root gem-path)
  "Sets Chruby environment variables in our Emacs environment."
  ;; ARG #1: value to set RUBY_ROOT
  ;; ARG #2: value to set RUBY_ENGINE
  ;; ARG #3: value to set RUBY_VERSION
  ;; ARG #4: value to set GEM_HOME
  ;; ARG #5: value to set GEM_ROOT
  ;; ARG #6: value to set GEM_PATH

  (let ((current-ruby-root (getenv "RUBY_ROOT"))
        (current-gem-root (getenv "GEM_ROOT"))
        (current-gem-home (getenv "GEM_HOME"))

        (set-vars (lambda ()
                    (setenv "RUBY_ROOT" ruby-root)
                    (setenv "RUBY_ENGINE" ruby-engine)
                    (setenv "RUBY_VERSION" ruby-version)
                    (setenv "GEM_HOME" gem-home)
                    (setenv "GEM_ROOT" gem-root)
                    (setenv "GEM_PATH" gem-path)
                    (add-to-list 'exec-path (concat ruby-root "/bin"))
                    (add-to-list 'exec-path (concat gem-root "/bin"))
                    (add-to-list 'exec-path (concat gem-home "/bin")))))

    (if current-ruby-root
        (if (and (string-equal current-ruby-root ruby-root)
                 (string-equal current-gem-root gem-root)
                 (string-equal current-gem-home gem-home))
            ;; if above versions match; do nothing (same ruby versions used)
            (message "[chruby-auto]: no change, using %s" ruby-root)
          ;; return nil
          nil

            ;; if vars exists, but do not match
            ;; unset PATH and then set all variables and path
            (progn
              (chruby-auto--unset-path
               current-ruby-root current-gem-root current-gem-home)
              (funcall set-vars)
              (chruby-auto--set-path ruby-root gem-root gem-home)
              (message "[chruby-auto]: using %s" ruby-root)))

      ;; if RUBY_ROOT is not set; we'll set all of our vars
      (progn
        (funcall set-vars)
        (chruby-auto--set-path
         ruby-root gem-root gem-home)
        (message "[chruby-auto]: using %s" ruby-root)))))

(defun chruby-auto--set-path (ruby-root gem-root gem-home)
  "Sets PATH environment variable for Chruby."
  ;; ARG #1: value to become RUBY_ROOT
  ;; ARG #2: value to become GEM_ROOT
  ;; ARG #3: value to become GEM_HOME

  ;; append "/bin" to each path being added to PATH and make list
  (let* ((paths-to-add
         (mapcar
          (lambda (path)
            (concat path "/bin:"))
          (list gem-home gem-root ruby-root)))
        (path (getenv "PATH"))

        ;; concatenate them all together with the current path
        (new-path (concat (string-join paths-to-add "") path)))

    ;; RET: (none) set PATH
    (setenv "PATH" new-path)))

(defun chruby-auto--unset-path
    (current-ruby-root current-gem-root current-gem-home)
  "Removes RUBY_ROOT, GEM_ROOT, GEM_HOME from PATH."
  ;; ARG #1: current RUBY_ROOT
  ;; ARG #2: current GEM_ROOT
  ;; ARG #3: current GEM_HOME

  ;; append "/bin" to each path to remove from PATH and make list
  (let* ((paths-to-remove
         (mapcar
          (lambda (path)
            (concat path "/bin:"))
          (list current-gem-home current-gem-root current-ruby-root)))
        (path (getenv "PATH")))

    ;; iterate through list and remove from PATH
    (dolist (path-to-remove paths-to-remove)
      (setq path
            (replace-regexp-in-string
             (regexp-quote path-to-remove) "" path))
      ;; remove from exec-path as well
      (setq exec-path (remove path-to-remove exec-path)))

    ;; RET: (none) set PATH
    (setenv "PATH" path)))

;;;###autoload
(defun chruby-auto ()
  "Activate the Ruby from the .ruby-version in this buffer's directory tree."
  ;; ARG: none

  (interactive)
  ;; gather all rubies directories on system;
  (let* ((rubies-dirs (chruby-auto--find-all-rubies chruby-auto-rubies-dirs))
         ;; locate the appropriate .ruby-version file and read its contents
         (ruby-wanted
          (chruby-auto--read-ruby-version-file
           (chruby-auto--find-ruby-version-file))))

    ;; if both rubies-dirs and ruby-wanted are non-nil, proceed to validate
    (when (and rubies-dirs ruby-wanted)
      ;; validate ruby version from .ruby-version is available
      (let ((ruby-auto
             (chruby-auto--validate-ruby-version rubies-dirs ruby-wanted)))
        (when ruby-auto
          (let ((env-vars (chruby-auto--gather-environment ruby-auto)))
            (when (= (length env-vars) 6)
              (apply #'chruby-auto--set-environment env-vars))))))))

(provide 'chruby-auto)
;;; chruby-auto.el ends here
