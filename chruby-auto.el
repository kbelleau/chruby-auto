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

;; chruby-auto.el
;;
;;
;;
;;
;;

;;; Code:

(defvar chruby-auto-rubies-dirs nil) ;; (list) allow custom dirs for rubies

(defun chruby-auto--find-all-rubies (user-rubies-dirs)
  ;; ARG: (list) `user-rubies-dirs' is the global variable
  ;;      `chruby-auto-rubies-dirs' which can be user assigned in init.el
  "Finds all Ruby root directories in common and user defined locations"

  ;; declare local variables
  (let ((static-rubies-dirs '("~/.rubies" "/opt/rubies"))
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

    ;; RET: (list) valid ruby root directories
    rubies-root-dirs))

(defun chruby-auto--find-ruby-version-file ()
  ;; ARG: none
  "Searches the directory tree for a file named .ruby-version file."

  ;; assign local var `dir' to the directory containing the .ruby-version file
  (let ((dir
         (locate-dominating-file default-directory ".ruby-version")))

    ;; RET: (string) full path to the closest .ruby-version file in the
    ;;      buffers directory tree
    (when dir (concat dir ".ruby-version"))))

(defun chruby-auto--read-ruby-version-file (ruby-version-file)
  ;; ARG: (string) file path of nearest .ruby-version file
  "Reads the .ruby-version file and returns its contents."

  ;; RET: (string) contents of the ruby-version-file
  (when ruby-version-file
    (with-temp-buffer
      (insert-file-contents ruby-version-file)
      (string-trim (buffer-string)))))

(defun chruby-auto--validate-ruby-version (rubies-dirs ruby-version)
  ;; ARG: (list) list of found rubies
  ;; ARG: (string) ruby version specified in found .ruby-version file
  "Validates the specified .ruby-version is a found ruby on the system."
  (catch 'match
    (dolist (dir rubies-dirs)
      (let ((dir-version (car (last (split-string dir "/")))))
        (when (string= dir-version ruby-version)
          ;; RET: (string) the matching directory
          (throw 'match dir))))))
;; IF FAILED: returns nil

(defun chruby-auto--gather-environment (auto-ruby)
  ;; ARG: (string) path to the directory containing the Ruby we are using
  "Gathers Ruby environment variables to set in our Emacs environment."

  ;; assign local variables for the ruby interpreter and rub the gatherer script
  (let* ((ruby-interp (concat auto-ruby "/bin/ruby"))
         (cmd (format "%s %s" ruby-interp "chruby-auto-gatherer.rb"))
         (output (shell-command-to-string cmd))
         (elements (split-string output " " t)))

    ;; collect the gatherer scripts' output and prep data for return
    (when (= (length elements) 3)
      (let* ((ruby-engine (nth 0 elements))
             (ruby-version (nth 1 elements))
             (gem-root (nth 2 elements))
             (gem-home
              (concat (getenv "HOME") "/.gem/" ruby-engine "/" ruby-version))
             (gem-path (concat gem-home ":" gem-root)))

        ;; RET: (list) all chruby environment variables
        ;;      (not that `auto-ruby' will become RUBY_ROOT
        (list auto-ruby ruby-engine ruby-version gem-home gem-root gem-path)))))

(defun chruby-auto--set-environment
    ;; ARGs: the return of chruby-auto--gather-environment
    (ruby-root ruby-engine ruby-version gem-home gem-root gem-path)
  "Sets Chruby environment variables in our Emacs environment."

  (let ((current-path (getenv "PATH")))
    (setenv "PATH"
            (concat
             gem-path
             path-separator
             ruby-root
             path-separator
             current-path)))

  (setenv "RUBY_ROOT" ruby-root)
  (setenv "RUBY_ENGINE" ruby-engine)
  (setenv "RUBY_VERSION" ruby-version)
  (setenv "GEM_HOME" gem-home)
  (setenv "GEM_ROOT" gem-root)
  (setenv "GEM_PATH" gem-path))

;;;###autoload
(defun chruby-auto ()
  ;; ARG: none
  "Activate the Ruby from the .ruby-version in this buffer's directory tree."

  (interactive)
  ;; gather all rubies directories on system;
  (let* ((rubies-dirs (chruby-auto--find-all-rubies chruby-auto-rubies-dirs))
           ;; locate the appropriate .ruby_version file and read its contents
         (ruby-wanted
          (chruby-auto--read-ruby-version-file
           (chruby-auto--find-ruby-version-file))))

    ;; validate ruby version from .ruby_version is available
    (let ((ruby-auto
           (chruby-auto--validate-ruby-version rubies-dirs ruby-wanted)))

      ;; RET: none
      ;; set environment variables like Chruby
      (apply #'chruby-auto--set-environment
             (chruby-auto--gather-environment ruby-auto)))))
;;; chruby-auto.el ends here
