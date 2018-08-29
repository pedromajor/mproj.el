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
     (mproj-project-root project)))
  "A lambda expression that is evaluated by `mproj/open-project'
when a project is selected"
  :group 'mproj
  :type 'func)

(cl-defstruct
    (mproj-project
     :named
     (:type list)
     (:constructor mproj--make-project
                   (name container-name root)))
  (root nil :read-only)
  (container-name nil :read-only)
  (name nil :read-only))

(defvar *mproj* nil
  "Global var holding a association list of registered projects")

(defvar *mproj--current* nil
  "Global var holding the current selected project")

(defun mproj--dir-base-name (dir)
  (file-name-base (directory-file-name dir)))

(defun mproj--list-directory (dir)
  "Return a list of subdirectories of DIR"
  (reduce (lambda (a x)
            (if (file-directory-p x) (cons x a) a))
          (directory-files dir t mproj-directory-regex)
          :initial-value nil))

(defun mproj--find-projects-in (containers)
  "Returns a list of `mproj-project' data structures from the
identified projects in `CONTAINERS'"
  (reduce
   #'append
   (mapcar
    (lambda (container)
      (mapcar
       (lambda (root)
         (mproj--make-project (mproj--dir-base-name root)
                              (mproj--dir-base-name container)
                              root))
       (mproj--list-directory container)))
    (mapcar #'expand-file-name containers))))

(defun mproj--register (store-alist project)
  "Register a PROJECT returns an updated `STORE-ALIST'"
  (let ((k (mproj--gen-key project)))
    (if (assoc k store-alist)
      (error "Aborting, duplicated project name detected: %s" k)
      (acons k project store-alist))))

(defun mproj--lookup (store-alist key)
  "Lookup a project by KEY and return it's associated data from
the STORE-ALIST"
  (cdr (assoc key store-alist)))

(defun mproj--projects-names (store-alist)
  "Return a list of registered project names"
  (mapcar #'car store-alist))

(defun mproj--index-projects (containers)
  "Returns an association list of name/project as the result of
indexing projects found in the `CONTAINERS'"
  (reduce
   #'mproj--register
   (mproj--find-projects-in containers)
   :initial-value nil))

(defun mproj--gen-key (project)
  (concat (mproj-project-name project)
          "-"
          (mproj-project-container-name project)))

(defun mproj--format-minibar-entry (proj)
  (concat
   (format "%14s" (mproj-project-name proj))
   (if-let (cont (mproj-project-container-name proj))
       (format " <%s>" cont))))

(defun mproj--digest-named (name)
  "Index a detached project `NAME' from a directory."
  (let ((proj (mproj--make-project name nil nil)))
    (setq *mproj* (mproj--register *mproj* proj))
    proj))

(defun mproj--make-minibar-projects-alist (store)
  (mapcar
   (lambda (x) (cons (mproj--format-minibar-entry (cdr x))
                (cdr x)))
   *mproj*))

(defun mproj--open-project-really (minibar-entry)
  (interactive "i")
  (let ((mp (mproj--make-minibar-projects-alist *mproj*)))
    (setq minibar-entry
          (completing-read "Open a project:"
                           (mapcar #'car mp)))
    (let* ((project (or (mproj--lookup mp minibar-entry)
                        (mproj--digest-named minibar-entry))))
      (cond
       ((null project)
        (error
         "BUG: Couldn't create or get project '%s' "
         minibar-entry))
       ((not (mproj-project-p project))
        (error
         "BUG: Internal error, project object type mismatch"))
       ((not (functionp mproj-default-action))
        (error
         "Error: The `mproj-default-action' isn't callable"))
       (t
        (setq *mproj--current* project)
        (funcall mproj-default-action project))))))

(defun mproj-find-project-root (proj)
  "Open project root dir in dired"
  (unless (null proj)
    (find-file (mproj-project-root proj))))

(defun mproj-make-index (&rest containers)
  "Indexes all projects found in `containers'"
  (setq *mproj* (mproj--index-projects containers)))

(defun mproj/open-project ()
  "Executes associated action on the selected user project"
  (interactive)
  (if (or (zerop (length *mproj*)) current-prefix-arg)
    (setq *mproj*
          (mproj--index-projects mproj-projects-dirs-list)))
  (call-interactively 'mproj--open-project-really))

(when mproj-bind-global-key
  (global-set-key (kbd "C-x p o")
                  #'mproj/open-project)
  (global-set-key (kbd "C-x p r")
                  (lambda ()
                    (interactive)
                    (mproj-find-project-root *mproj--current*))))

(provide 'mproj)
