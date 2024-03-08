;;; quarto-transient.el --- Define a transient for quarto -*- lexical-binding: t -*-
;;
;; Author: Christophe Gouel
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:

;;; Code:

(require 'quarto-mode)
(require 'transient)

;;;###autoload (autoload 'quarto-transient "quarto-transient" nil t)
(transient-define-prefix quarto-transient ()
  "Quarto transient."  
  ["Actions"
   ("r" "Render files or projects" quarto-transient--not-defined)
   ("p" "Render and preview a document or website project" quarto-transient-preview)
   ("s" "Serve a Shiny interactive document" quarto-transient--not-defined)
   ("c" "Create a Quarto project or extension" quarto-transient-create)
   ("P" "Publish a document or project" quarto-transient--not-defined)
   ("k" "Kill all *quarto-...* buffers (including running previews)" quarto-transient--kill-buffers)])

;;;###autoload (autoload 'quarto-transient-preview "quarto-transient" nil t)
(transient-define-prefix quarto-transient-preview ()
  "Quarto transient preview."  
  [["Arguments"
   (quarto-transient-preview:-p)
   ("-s" "Don't run a local preview web server" "--no-serve")
   (quarto-transient-preview:-h)
   (quarto-transient-preview:-r)
   ("-n" "Don't navigate the browser automatically when outputs are updated" "--no-navigate")
   ("-o" "Don't open a browser to preview the site" "--no-browser")
   ("-w" "Do not re-render input files when they change" "--no-watch-inputs")
   (quarto-transient-preview:-t)
   ("-P" "Active project profile(s)" "--profile")]
   ["Log"
    (quarto-transient:-l)
    (quarto-transient:-L)
    (quarto-transient:-f)]
   ["Buffer"
    ("-q" "Run process without associated buffer" "--quiet")]]
  [["Preview"
   ("p" "This buffer" quarto-transient--preview-this-file)
   ("d" "This directory" quarto-transient--preview-this-directory)
   ("f" "A file" quarto-transient--preview-a-file)
   ("D" "A directory" quarto-transient--preview-a-directory)]
   ["Stop"
    ("s" "Stop all previews" quarto-transient--preview-stop)]])

;;;###autoload (autoload 'quarto-transient-create "quarto-transient" nil t)
(transient-define-prefix quarto-transient-create ()
  "Quarto transient create."  
  [["Arguments"
   ("-P" "Active project profile(s)" "--profile")]
   ["Log"
    (quarto-transient:-l)
    (quarto-transient:-L)
    (quarto-transient:-f)]
   ["Buffer"
    ("-q" "Run process without associated buffer" "--quiet")]]
  [["Project"
   ("d" "Default" quarto-transient--not-defined)
   ("w" "Website" quarto-transient--create)
   ("b" "Blog" quarto-transient--not-defined)
   ("B" "Book" quarto-transient--not-defined)
   ("c" "Confluence" quarto-transient--not-defined)
   ("m" "Manuscript" quarto-transient--not-defined)]])

(defun quarto-transient--not-defined ()
  "Action not yet defined."
  (interactive)
  (message "Action not yet defined"))

(defun quarto-preview--preview-async (target preview-buffer-name &optional args)
  "Run quarto preview on current buffer."
  ;; Split arguments into separate list elements when they include a space
  (if args 
      (setq args (quarto-preview--split-string-list-on-space args)))
  (let ((preview-command-list
	 ;; Need to flatten the list because args is itself a list
	 (flatten-tree (list quarto-command
			     "preview"
			     target
			     args)))
	(preview-process-name (format "quarto-preview-%s" target)))
    ;; Kill process and buffer to be used if they exist already
    (let ((process (get-process preview-process-name)))
      (when process
      	(kill-process process)))
    (when (get-buffer preview-buffer-name)
      (quarto-preview--kill-buffer-with-process preview-buffer-name))
    ;; No buffer if "--quiet" argument
    (when (member "--quiet" args)
      (setq preview-buffer-name nil))
    (message "Running %s" (mapconcat #'identity preview-command-list " "))
    (make-process
     :name preview-process-name
     :buffer preview-buffer-name
     :command preview-command-list)))

(defun quarto-transient--preview-this-file (&optional args)
  "Run quarto preview on current buffer"
  (interactive (list (transient-args 'quarto-transient-preview)))
    (let* ((input buffer-file-name)
	   (preview-buffer-name (format "*quarto-preview-%s*"
					(file-name-nondirectory input))))
    (quarto-preview--preview-async input preview-buffer-name args)))

(defun quarto-transient--preview-this-directory (&optional args)
  "Run quarto preview on current directory."
  (interactive (list (transient-args 'quarto-transient-preview)))
  (let* ((input default-directory)
	 (preview-buffer-name (format "*quarto-preview-%s*" input)))
    (quarto-preview--preview-async input preview-buffer-name args)))

(defun quarto-transient--preview-a-file (&optional args)
  "Run quarto preview on a file."
  (interactive (list (transient-args 'quarto-transient-preview)))
  (let* ((input (read-file-name "Select file: "))
	 (preview-buffer-name (format "*quarto-preview-%s*"
				      (file-name-nondirectory input))))
    (quarto-preview--preview-async input preview-buffer-name args)))

(defun quarto-transient--preview-a-directory (&optional args)
  "Run quarto preview on a directory."
  (interactive (list (transient-args 'quarto-transient-preview)))
  (let* ((input (read-directory-name "Select directory: "))
	 (preview-buffer-name (format "*quarto-preview-%s*"
				      (file-name-nondirectory input))))
    (quarto-preview--preview-async input preview-buffer-name args)))

(defun quarto-preview--split-string-list-on-space (string-list)
  "Split strings in STRING-LIST containing spaces into separate elements."
  (apply #'append
         (mapcar (lambda (str)
                   (if (string-match-p " " str)
                       (split-string str " ")
                     (list str)))
                 string-list)))

(defun quarto-preview--kill-buffer-with-process (buffer)
  "Kill the buffer and the process associated with it."
  ;; Retrieve the process associated with the buffer, if any
  (let ((process (get-buffer-process buffer)))
    ;; If there is a process, set the query-on-exit flag to nil
    (when process
      (set-process-query-on-exit-flag process nil)))
  ;; Kill the buffer
  (kill-buffer buffer))

(defun quarto-transient--kill-a-process (process-name-to-kill)
  "Kill a process with name PROCESS-NAME-TO-KILL."
  (interactive)
  (dolist (process (process-list))
    (when (string-match process-name-to-kill (process-name process))
	(delete-process process))))

(defun quarto-transient--kill-processes (&optional verb)
  "Kill all quarto processes."
  (interactive)
  (let ((to-kill (concat "quarto-" verb)))
    (dolist (process (process-list))
      (when (string-prefix-p to-kill (process-name process))
	(delete-process process)))))

(defun quarto-transient--kill-buffers (&optional verb)
  "Kill all quarto buffers."
  (interactive)
  (quarto-transient--kill-processes verb)
  (let ((to-kill (concat "*quarto-" verb)))
	(dolist (buffer (buffer-list))
    (when (string-prefix-p to-kill (buffer-name buffer))
      (kill-buffer buffer)))))

(defun quarto-transient--preview-stop ()
  "Stop all quarto previews."
  (interactive)
  (quarto-transient--kill-processes "preview")
  (quarto-transient--kill-buffers "preview")
  )

(transient-define-argument quarto-transient-preview:-p ()
  :description "Suggested port to listen on"
  :class 'transient-option
  :shortarg "-p"
  :argument "--port "
  :reader #'transient-read-number-N+)

(transient-define-argument quarto-transient-preview:-h ()
  :description "Hostname to bind to"
  :class 'transient-option
  :shortarg "-h"
  :argument "--host ")

(transient-define-argument quarto-transient-preview:-r ()
  :description "Render to the specified format(s) before previewing"
  :class 'transient-option
  :shortarg "-r"
  :argument "--render ")

(transient-define-argument quarto-transient-preview:-t ()
  :description "Time (in seconds) after which to exit if there are no active clients"
  :class 'transient-option
  :shortarg "-t"
  :argument "--timeout "
  :reader #'transient-read-number-N+)

(transient-define-argument quarto-transient:-l ()
  :description "Path to log file"
  :class 'transient-option
  :shortarg "-l"
  :argument "--log "
  :reader #'transient-read-file)

(transient-define-argument quarto-transient:-L ()
  :description "Log level"
  :class 'transient-option
  :shortarg "-L"
  :argument "--log-level "
  :choices '("info" "warning" "error" "critical"))

(transient-define-argument quarto-transient:-f ()
  :description "Log format"
  :class 'transient-option
  :shortarg "-f"
  :argument "--log-format "
  :choices '("plain" "json-stream"))

(defun quarto-transient--create (&optional args)
  (interactive (list (transient-args 'quarto-transient-create)))
  (let* ((input-directory (read-directory-name "Select directory: "))
	 (input (file-relative-name input-directory)))
    (make-process
     :name "quarto-create"
     :buffer nil
     :command (flatten-tree (list quarto-command
				  "create"
				  "project"
				  "website"
				  input
				  "--no-prompt"
				  args))
          :sentinel (lambda (proc event)
                      (when (string= event "finished\n")
			(dired input-directory))))))

;;; _
(provide 'quarto-transient)
;;; quarto-transient.el ends here
