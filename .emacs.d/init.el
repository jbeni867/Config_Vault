(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 20)
(menu-bar-mode -1)
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(column-number-mode)

(defvar void/default-font-size 115)
(defvar void/my-ui-font (if (eq system-type 'windows-nt) "Segoe UI" "Cantarell"))

;; Font Config
(set-face-attribute 'default nil :font "JetBrainsMono NF" :height void/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono NF" :height void/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
		    :font void/my-ui-font
		    :height void/default-font-size
		    :weight 'regular)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(setq org-confirm-babel-evaluate nil)

(defun void/org-babel-tangle-config ()
  "Automatically tangle the Org file when saved."
  (when (string-equal (file-name-nondirectory (buffer-file-name)) "Emacs.org")
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook 
          (lambda () 
            (add-hook 'after-save-hook #'void/org-babel-tangle-config nil t)))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;; Set faces for heading levels
(with-eval-after-load 'org-faces
    (dolist (face '((org-level-1 . 1.2)
		    (org-level-2 . 1.1)
		    (org-level-3 . 1.05)
		    (org-level-4 . 1.0)
		    (org-level-5 . 1.1)
		    (org-level-6 . 1.1)
		    (org-level-7 . 1.1)
		    (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font void/my-ui-font :weight 'regular :height (cdr face))))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(with-eval-after-load 'org
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun void/org-mode-visual-fill ()
  (setq visual-fill-column-width 200
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun void/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package command-log-mode)

  (use-package ivy
    :bind (("C-s" . swiper)
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)	
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

  (use-package counsel
    :bind (("M-x" . counsel-M-x)
  	 ("C-x b" . counsel-ibuffer)
  	 ("C-x C-f" . counsel-find-file)
  	 :map minibuffer-local-map
  	 ("C-r" . 'counsel-minibuffer-history))
    :config
    (setq ivy-initial-inputs-alist nil))

  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 15)))

  (use-package all-the-icons)

  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  (use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.2))

  (use-package ivy-rich
    :init
    (ivy-rich-mode 1))

  (use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

  (use-package doom-themes
    :config
    (load-theme 'doom-one t))

  (use-package general
    :ensure t
    :init
    (general-create-definer void/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")
    :config
    (void/leader-keys
      "t"  '(:ignore t :which-key "toggles")
      "tt" '(counsel-load-theme :which-key "choose theme")))

  (use-package evil
    :ensure t
    :demand t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-i-jump nil)
    :hook (evil-mode . void/evil-hook)
    :config
    (evil-mode 1)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
    ; Use visual line motions even outside of visual-line-mode buffers
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
    :custom ((evil-undo-system 'undo-redo)))

  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  (use-package hydra)
  (defhydra hydra-text-scale (:timeout 6)
  	   "scale text"
  	   ("j" text-scale-increase "in")
  	   ("k" text-scale-decrease "out")
  	   ("f" nil "finished" :exit t))

  (void/leader-keys
    "ts" '(hydra-text-scale/body :which-key "scale text"))


  (use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map)
    :init
    (when (file-directory-p "~/Development")
      (setq projectile-project-search-path '("~/Development")))
    (setq projectile-switch-project-action #'projectile-dired))
  ; TODO: Add info about <M-o> giving more info on additional operations
  (use-package counsel-projectile
    :config (counsel-projectile-mode))

  (use-package magit
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

  ; TODO: Still need to configure this plugin
  (use-package forge)

  (use-package org
    :hook (org-mode . void/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")
    (setq org-agenda-start-with-log-mode t)
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

    (require 'org-temp)
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("cs" . "src csharp"))
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)

    (setq org-agenda-files
  	'("/home/jbenitez/Documents/Super_Temp_Delete_Once_Seen/Tasks.org")))

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :hook (org-mode . void/org-mode-visual-fill))
