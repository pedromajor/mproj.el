;;; mproj.el --- Indexes projects by dir and allows selection in the minibuffer

;; Author: Pedro Major <pedro.major@gmail.com>
;; Keywords: project managment
;; Created: 2017-02-03
;; Version: 0.0.1
;; URL: http://github.com/pedromajor/mproj.el

;; Copyright (C) 2017 Pedro Major

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Guiven a list o directories creates an index to all projects. A
;; project can be selected from a list in the minibuffer with
;; `mproj/open-project'. For the moment only a default action is
;; supported, that can be customized via `mproj-default-action'

;;; Code:

(require 'cl)

(defgroup mproj nil
  "Open projects quickly"
  :group 'tools)

(defcustom mproj-projects-dirs-list nil
  "List of top level directory where projects live"
  :group 'mproj
  :type 'boolean)

(defcustom mproj-bind-global-key t
  "Bind globally the keys 'C-x p o' to `mproj/open-project'"
  :group 'mproj
  :type 'boolean)

(defcustom mproj-directory-regex "^[^.].+$"
  "The regex used to filter the results of top level directory
  listing expansion"
  :group 'mproj
  :type 'regexp)

(defcustom mproj-default-action
  (lambda (project)
    (find-file
     (mproj-project-root-dir project)))
  "A lambda expression that is evaluated by `mproj/open-project'
when a project is selected"
  :group 'mproj
  :type 'func)

(cl-defstruct
    (mproj-project
     :named
     (:type list)
     (:constructor mproj--create-project))
  root-dir
  top-dir
  (name (mproj--make-name-from top-dir root-dir)))

(defvar *mproj* nil
  "Global var holding a association list of registered projects")

(defun mproj--make-name-from (top-dir project-root-dir)
  "Generates the project name by concatenating the root and
top level directory base names"
  (format
   "%14s <%s>"
   (file-name-base (directory-file-name project-root-dir))
   (file-name-base (directory-file-name top-dir))))

(defun mproj--list-directory (dir)
  "Return a list of subdirectories of DIR"
  (reduce (lambda (a x)
            (if (file-directory-p x) (cons x a) a))
          (directory-files dir t mproj-directory-regex)
          :initial-value nil))

(defun mproj--process-top-dirs (top-dirs)
  "Processes TOP-DIRS and returns a list of projects data
structures."
  (reduce
   #'append
   (mapcar
    (lambda (top-dir) (mapcar
                  (lambda (root)
                    (mproj--create-project :root-dir root
                                           :top-dir top-dir))
                  (mproj--list-directory top-dir)))
    (mapcar #'expand-file-name top-dirs))))

(defun mproj--register (store-alist project)
  "Register a PROJECT in STORE-ALIST"
  (let ((name (mproj-project-name project)))
    (if (assoc name store-alist)
        (error "A project named %s already exists." name)
      (push (cons name project) store-alist))))

(defun mproj--lookup (store-alist name)
  "Lookup a project by NAME and return it's associated data from
the STORE-ALIST"
  (cdr (assoc name store-alist)))

(defun mproj--projects-names (store-alist)
  "Return a list of registered project names"
  (mapcar #'car store-alist))

(defun mproj--index-projects ()
  (setq *mproj*
        (reduce
         #'mproj--register
         (mproj--process-top-dirs mproj-projects-dirs-list)
         :initial-value nil)))

(defun mproj--open-project-really (proj-name)
  (interactive
   (list
    (completing-read "Open a project:"
                     (mproj--projects-names *mproj*))))
  (let ((project (mproj--lookup *mproj* proj-name)))
    (cond
     ((null project)
      (error
       "BUG: Can't locate information for project named '%s' "
       proj-name))
     ((not (mproj-project-p project))
      (error
       "BUG: Internal error, project object type mismatch"))
     ((not (functionp mproj-default-action))
      (error
       "Error: The `mproj-default-action' isn't callable"))
     (t
      (funcall mproj-default-action project)))))

(defun mproj/open-project ()
  "Executes associated action on the selected user project"
  (interactive)
  (when (zerop (length *mproj*)) (mproj--index-projects))
  (cond ((zerop (length *mproj*))
         (message "No projects found!"))
        (t (call-interactively 'mproj--open-project-really))))

(if mproj-bind-global-key
    (global-set-key (kbd "C-x p o") #'mproj/open-project))

(provide 'mproj)
