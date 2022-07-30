;;; feline --- a modeline with very little
;;; Commentary:
;;; be fast and small
;;; Code:

(defgroup feline nil
  "The feline modeline."
  :group 'mode-line
  :link '(url-link
           :tag "Homepage" "https://git.sr.ht/~chee/feline-mode"))

(defgroup feline-faces nil
  "Feline faces."
  :group 'feline
  :group 'faces
  :link '(url-link
           :tag "Homepage" "https://git.sr.ht/~chee/feline-mode"))

(defface feline-buffer-id-face
  '((t ()))
  "The buffer's name in the feline."
  :group 'feline-faces)

(defface feline-major-mode-face
  '((t (:weight bold)))
  "The major mode's name in the feline."
  :group 'feline-faces)

(defface feline-position-prefix-face
  '((t (:inherit font-lock-comment-face)))
  "The prefix for line and column positions."
  :group 'feline-faces)

(defface feline-project-name-face
  '((t (:weight bold)))
  "The name of the current project."
  :group 'feline-faces)

(defface feline-evil-face
  '((t (:weight bold)))
  "A face for evil in the feline."
  :group 'feline-faces)

(defface feline-evil-normal-face
  '((t (:inherit feline-evil-face
			:background "#000"
			:foreground "white")))
  "The evil normal feline."
  :group 'feline-faces)

(defface feline-evil-emacs-face
  '((t (:inherit feline-evil-face
			:background "#3366ff"
			:foreground "white")))
  "The evil Emacs feline."
  :group 'feline-faces)

(defface feline-evil-insert-face
  '((t (:inherit feline-evil-face
			:background "#3399ff"
			:foreground "white")))
  "The evil insert feline."
  :group 'feline-faces)

(defface feline-evil-visual-face
  '((t (:inherit feline-evil-face
			:background "#ffee88"
			:foreground "#333")))
  "The evil visual feline."
  :group 'feline-faces)

(defface feline-position-face
  '((t (:inherit font-lock-constant-face)))
  "The value for the line and column positions."
  :group 'feline-faces)

(defcustom feline-line-prefix
  "l"
  "The prefix for line number."
  :type 'string
  :group 'feline)

(defcustom feline-column-prefix
  "c"
  "The prefix for column number."
  :type 'string
  :group 'feline)

(defcustom feline-mode-symbols
  '()
  "The replacement for a mode's name.
The key is the full mode name including -mode, i.e. \"emacs-lisp-mode\".
If there is no match, the fallback is the mode name without the -mode suffix."
  :type '(plist :key-type symbol :value-type string)
  :group 'feline)

(defun feline/get-mode-symbol (mode)
  "Get an symbol representation for MODE from the list `feline/mode-symbol'."
  (propertize
	 (or
		(plist-get
		  feline-mode-symbols major-mode)
		(format "%s" major-mode))
	 'face 'feline-major-mode-face))

(defun feline/mode-name (mode)
  "Get a feline representation of MODE."
  (replace-regexp-in-string "-mode$" "" (feline/get-mode-symbol mode)))

(defun feline/major-mode nil
  "Get a feline representation of the major mode."
  (feline/mode-name major-mode))

(defun feline/buffer-id (buffer-id)
  "Get a feline representation of BUFFER-ID."
  (propertize
	 (replace-regexp-in-string
		"\\*Org Src \\([^[]*\\)\\[ \\(.*\\) +]"
		" \\1→src(\\2)"
		buffer-id)
	 'face 'feline-buffer-id-face))

(defun feline/evil nil
  "Get a feline representation of the evil mode."
  (when (bound-and-true-p evil-local-mode)
	 (apply 'propertize
		(cond
		  ((evil-normal-state-p) '("normal" face feline-evil-normal-face))
		  ((evil-emacs-state-p) '("emacs" face feline-evil-emacs-face))
		  ((evil-insert-state-p) '("insert" face feline-evil-insert-face))
		  ((evil-motion-state-p) '("motion" face feline-evil-motion-face))
		  ((evil-visual-state-p) '("visual" face feline-evil-visual-face))
		  ((evil-operator-state-p) '("operator" face feline-evil-operator-face))
		  ((evil-replace-state-p) '("replace" face feline-evil-replace-face))
		  (t ("normal" 'face 'feline-evil-normal-face))))))

(defun feline/positions nil
  "Present the line and column in the feline."
  (concat (propertize
		      feline-line-prefix
		      'face 'feline-position-prefix-face)
	 (propertize
		"%l"
		'face 'feline-position-face)
	 (propertize
		feline-column-prefix
		'face 'feline-position-prefix-face)
	 (propertize
		"%c"
		'face 'feline-position-face)))

(defvar feline--project-name-cache '())

(defun feline/--last-meaningful-string (list)
  "Get last meaningful string from LIST."
  (let* ((n (car list)) (rest (cdr list)) (nn (car rest)))
	 (if (or (not nn) (string-empty-p nn))
	   n
		(feline/--last-meaningful-string rest))))

(defun feline/--project-name nil
  "Get the current project, if we're in one."
  (let ((project (project-current)))
	 (when project
		(feline/--last-meaningful-string
		  (split-string (project-root project) "/")))))

(defun feline/project-name nil
  "Get a feline representation of the current project name."
  (let* ((filename (buffer-file-name))
			 (cached (plist-get feline--project-name-cache filename)))
	 (format
		"(%s)"
		(propertize
		  ;; tramp makes this _really slow_ so we're gonna just exit sideways
		  (if (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p filename))
			 "TRAMP"
	       (if cached cached
			   (or (feline/--project-name) "")))
		  'mouse-face 'mode-line-highlight
		  'local-map (let ((🐈‍⬛ (make-sparse-keymap)))
			            (define-key 🐈‍⬛ [mode-line mouse-1] 'project-switch-project)
			            🐈‍⬛)
		  'face 'feline-project-name-face))))

(defvar feline/original-modeline mode-line-format)

(defvar feline-mode nil "\"t\" if feline is active.")

(defun feline/spray nil
  "Stop feline, returning the previous modeline."
  (setq feline-mode nil)
  (setq-default mode-line-format feline/original-modeline))

(defun feline/purr nil
  "Start feline."
  (setq feline-mode t)
  (setq-default
	 mode-line-format
	 '(""
		 (:eval (feline/evil))
		 " "
		 (:eval (propertize (if (buffer-modified-p) "*" "") 'face 'feline-buffer-id-face))
		 (:eval (feline/major-mode))
		 " "
		 (:eval (feline/buffer-id (format-mode-line "%b")))
		 " "
		 (:eval (feline/project-name))
		 " "
		 (:eval (feline/positions))
		 " "
		 mode-line-misc-info)))

(define-minor-mode feline-mode
  "Toggle _f_e_l_i_n_e_."
  :group 'feline
  :global t
  :lighter nil
  (if feline-mode
	 (feline/purr)
	 (feline/spray)))

(provide 'feline)
;;; feline ends here
