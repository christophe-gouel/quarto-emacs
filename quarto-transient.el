;; Define a transient for quarto

(require 'quarto-mode)
(require 'transient)

(transient-define-prefix quarto ()
  "Quarto transient."  
  ["Actions"
   ("r" "Render files or projects" not-defined)
   ("p" "Render and preview a document or wbesite project" quarto-transient-preview)
   ("s" "Serve a Shiny interactive document" not-defined)
   ("c" "Create a Quarto project or extension" not-defined)
   ("C" "Create a project for rendering multiple documents" not-defined)
   ("P" "Publish a document or project" not-defined)
   ("k" "Kill all *quarto-...* buffers (including running previews)" quarto--kill-buffers)])

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
   ("-q" "Suppress console output" "--quiet")
   ("-P" "Active project profile(s)" "--profile")]
   ["Log"
    (quarto-transient-preview:-l)
    (quarto-transient-preview:-L)
    (quarto-transient-preview:-f)]]
  
  [["Preview"
   ("p" "This buffer" quarto--preview-this-file)
   ("d" "This directory" quarto--preview-this-directory)
   ("f" "A file" quarto--preview-a-file)
   ("D" "A directory" quarto--preview-a-directory)]
   ["Stop"
    ("s" "Stop all previews" quarto--preview-stop)]])

(defun not-defined ()
  (interactive)
  (message "Action not yet defined"))

(defun quarto--preview-async (target preview-buffer-name &optional args)
  "Run quarto preview on current buffer"
  ;; Split arguments into separate list elements when they include a space
  (if args 
      (setq args (split-string-list-on-space args)))
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
      (quarto--kill-buffer-with-process preview-buffer-name))
    ;; No buffer if "--quiet" argument
    (when (member "--quiet" args)
      (setq preview-buffer-name nil))
    (message "Running %s" (mapconcat #'identity preview-command-list " "))
    (make-process
     :name preview-process-name
     :buffer preview-buffer-name
     :command preview-command-list)))

(defun quarto--preview-this-file (&optional args)
  "Run quarto preview on current buffer"
  (interactive (list (transient-args 'quarto-transient-preview)))
    (let* ((input buffer-file-name)
	   (preview-buffer-name (format "*quarto-preview-%s*"
					(file-name-nondirectory input))))
    (quarto--preview-async input preview-buffer-name args)))

(defun quarto--preview-this-directory (&optional args)
  (interactive (list (transient-args 'quarto-transient-preview)))
  (let* ((input default-directory)
	 (preview-buffer-name (format "*quarto-preview-%s*" input)))
    (quarto--preview-async input preview-buffer-name args)))

(defun quarto--preview-a-file (&optional args)
  (interactive (list (transient-args 'quarto-transient-preview)))
  (let* ((input (read-file-name "Select file: "))
	 (preview-buffer-name (format "*quarto-preview-%s*"
				      (file-name-nondirectory input))))
    (quarto--preview-async input preview-buffer-name args)))

(defun quarto--preview-a-directory (&optional args)
  (interactive (list (transient-args 'quarto-transient-preview)))
  (let* ((input (read-directory-name "Select directory: "))
	 (preview-buffer-name (format "*quarto-preview-%s*"
				      (file-name-nondirectory input))))
    (quarto--preview-async input preview-buffer-name args)))

(defun split-string-list-on-space (string-list)
  "Split strings in STRING-LIST containing spaces into separate elements."
  (apply #'append
         (mapcar (lambda (str)
                   (if (string-match-p " " str)
                       (split-string str " ")
                     (list str)))
                 string-list)))

(defun quarto--kill-buffer-with-process (buffer)
  ;; Retrieve the process associated with the buffer, if any
  (let ((process (get-buffer-process buffer)))
    ;; If there is a process, set the query-on-exit flag to nil
    (when process
      (set-process-query-on-exit-flag process nil)))
  ;; Kill the buffer
  (kill-buffer buffer))

(defun quarto--kill-a-process (process-name-to-kill)
  (interactive)
  (dolist (process (process-list))
    (when (string-match process-name-to-kill (process-name process))
	(delete-process process))))


(defun quarto--kill-processes (&optional verb)
  (interactive)
  (let ((to-kill (concat "quarto-" verb)))
    (dolist (process (process-list))
      (when (string-prefix-p to-kill (process-name process))
	(delete-process process)))))

(defun quarto--kill-buffers (&optional verb)
  (interactive)
  (quarto--kill-processes verb)
  (let ((to-kill (concat "*quarto-" verb)))
	(dolist (buffer (buffer-list))
    (when (string-prefix-p to-kill (buffer-name buffer))
      (kill-buffer buffer)))))

(defun quarto--preview-stop ()
  (interactive)
  (quarto--kill-processes "preview")
  (quarto--kill-buffers "preview")
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

(transient-define-argument quarto-transient-preview:-l ()
  :description "Path to log file"
  :class 'transient-option
  :shortarg "-l"
  :argument "--log "
  :reader #'transient-read-file)

(transient-define-argument quarto-transient-preview:-L ()
  :description "Log level"
  :class 'transient-option
  :shortarg "-L"
  :argument "--log-level "
  :choices '("info" "warning" "error" "critical"))

(transient-define-argument quarto-transient-preview:-f ()
  :description "Log format"
  :class 'transient-option
  :shortarg "-f"
  :argument "--log-format "
  :choices '("plain" "json-stream"))

;;; _
(provide 'quarto-transient)
;;; quarto-transient.el ends here
