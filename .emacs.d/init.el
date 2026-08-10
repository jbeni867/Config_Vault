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

(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Increase how much data Emacs reads from the LSP process to 1MB
(setq read-process-output-max (* 1024 1024))

;; Increase the garbage collection threshold to 100MB to prevent stuttering
(setq gc-cons-threshold 100000000)

;; Scrolling
(setq scroll-margin 5)              ; Keep 5 lines visible above/below point
(setq scroll-conservatively 101)    ; Scroll line-by-line instead of recentering
(setq scroll-step 1)                ; Scroll one line at a time

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

(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t))

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

(use-package doom-themes)

(use-package general
  :ensure t
  :after (evil which-key)
  :config
  (general-create-definer void/leader-keys
    :keymaps '(normal visual)
    :prefix "SPC"
    :global-prefix "C-SPC")
  
  (void/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    ;; New bindings for docs
    "k"  '(:ignore t :which-key "docs")
    "ki" '(lsp-ui-doc-show :which-key "show hover doc")))

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
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

(use-package ivy-hydra
  :after (ivy hydra))

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
  
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("cs" . "src csharp"))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  
  (use-package org-modern
    :after org
    :hook (org-mode . org-modern-mode)
    :custom
    (org-modern-star '("◉" "○" "●" "○" "●" "○" "●")))
  
  (use-package visual-fill-column
    :hook (org-mode . void/org-mode-visual-fill)))

(use-package csproj-mode)
(use-package sharper
  :demand t
  :bind
  ("C-c n" . sharper-main-transient))

(use-package typescript-mode)
(use-package web-mode
  :mode "\\.html?\\'")
;; scss-mode still registers legacy Flymake variables at load time.
;; Define them only when absent; SCSS compilation remains disabled.
(defvar flymake-allowed-file-name-masks nil)
(defvar flymake-err-line-patterns nil)
(use-package scss-mode
  :custom
  (scss-compile-at-save nil))

;; `tab-prefix-map' (C-x t) and `project-prefix-map' (C-x p) live in built-in
;; packages that are not loaded at startup, so pull them in before binding.
(require 'tab-bar)
(require 'project)

;; `counsel-recentf' needs a recent file list to read from.
(recentf-mode 1)

(void/leader-keys
  ;; Buffers
  "b"  '(:ignore t :which-key "buffers")
  "bb" '(counsel-switch-buffer :which-key "switch buffer")
  "bi" '(counsel-ibuffer :which-key "ibuffer")
  "bd" '(kill-current-buffer :which-key "kill buffer")
  "bn" '(next-buffer :which-key "next buffer")
  "bp" '(previous-buffer :which-key "previous buffer")
  "br" '(revert-buffer-quick :which-key "revert buffer")
  "bs" '(save-buffer :which-key "save buffer")

  ;; Files
  "f"  '(:ignore t :which-key "files")
  "ff" '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "recent files")
  "fd" '(dired :which-key "dired")
  "fs" '(save-buffer :which-key "save file")
  "fS" '(write-file :which-key "save file as")

  ;; Git
  "g"  '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "status")
  "gb" '(magit-blame-addition :which-key "blame")
  "gd" '(magit-diff-buffer-file :which-key "diff file")
  "gl" '(magit-log-buffer-file :which-key "log file")
  "gL" '(magit-log-current :which-key "log branch")
  "gc" '(magit-clone :which-key "clone")

  ;; Sidebars and other windows worth opening
  "o"  '(:ignore t :which-key "open")
  "ot" '(treemacs :which-key "treemacs toggle")
  "op" '(treemacs-add-and-display-current-project-exclusively :which-key "treemacs this project")
  "os" '(treemacs-select-window :which-key "select treemacs")

  ;; Whole prefix maps, reachable from both their original prefix and SPC
  "p"   '(:keymap projectile-command-map :package projectile :which-key "projectile")
  "P"   '(:keymap project-prefix-map :which-key "project.el")
  "w"   '(:keymap evil-window-map :package evil :which-key "windows")
  "TAB" '(:keymap tab-prefix-map :which-key "tabs"))

;; which-key prints "prefix" for any nested keymap that nothing has named.
;; Keymap-based labels attach to the map itself, so a label shows up under
;; every prefix that reaches it (SPC p and C-c p, for example).
(defun void/which-key-label (keymap-symbol &rest pairs)
  "Name prefix keys inside KEYMAP-SYMBOL for which-key.
PAIRS is a sequence of KEY DESCRIPTION strings."
  (when (and (boundp keymap-symbol)
             (keymapp (symbol-value keymap-symbol)))
    (apply #'which-key-add-keymap-based-replacements
           (symbol-value keymap-symbol) pairs)))

(with-eval-after-load 'projectile
  (void/which-key-label 'projectile-command-map
                        "4" "other window"
                        "5" "other frame"
                        "s" "search"
                        "x" "run"))

;; lsp-mode ships descriptions for `lsp-command-map', but registers them
;; against `lsp-keymap-prefix' rather than the prefix the map is bound to.
;; Re-register them for the leader paths actually in use.
(with-eval-after-load 'lsp-mode
  (dolist (prefix '("SPC l" "C-SPC l"))
    (let ((lsp-keymap-prefix prefix))
      (lsp-enable-which-key-integration t))))

(defun void/treesit-language-ready-p (language)
  "Return non-nil when LANGUAGE has a usable tree-sitter grammar."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p language)))

(when (void/treesit-language-ready-p 'typescript)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))
(when (void/treesit-language-ready-p 'tsx)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))
(when (void/treesit-language-ready-p 'c-sharp)
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode)))
(when (void/treesit-language-ready-p 'css)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode)))

(defun void/angular-project-p (&optional directory)
  "Return non-nil when DIRECTORY belongs to an Angular project."
  (locate-dominating-file (or directory default-directory) "angular.json"))

(defun void/web-mode-setup-angular-template ()
  "Enable Angular template editing for HTML files in Angular projects."
  (when (and buffer-file-name
             (void/angular-project-p (file-name-directory buffer-file-name)))
    (web-mode-set-engine "angular")))

(add-hook 'web-mode-hook #'void/web-mode-setup-angular-template)

;; External language servers: typescript-language-server, ngserver,
;; vscode-html-language-server, and vscode-css-language-server.
;; (setenv "LSP_USE_PLISTS" "true")
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  ;; :init (setq lsp-keymap-prefix "SPC l")
  :hook ((csharp-mode . lsp-deferred)
         (csharp-ts-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
          (js-mode . lsp-deferred)
          (css-mode . lsp-deferred)
          (css-ts-mode . lsp-deferred))
  :custom
  (lsp-enable-snippet t)                   ; Ensure snippet support is on
  (lsp-completion-provider :capf)          ; Use the standard completion API
  (lsp-completion-show-detail t)           ; Force Roslyn to fetch docs/details
  (lsp-completion-show-kind t)             ; Show icons in the autocomplete menu
  :config
  (setq lsp-roslyn-package-version "5.0.0-1.25277.114")

   (with-eval-after-load 'lsp-javascript
     (setq lsp-clients-typescript-prefer-use-project-ts-server t))

   ;; MSSQL is loaded explicitly by `void/lsp-mssql-deferred' for SQL buffers.
   (setq lsp-client-packages (delq 'lsp-mssql lsp-client-packages))

   ;; Prefer lsp-mssql over the retained postgres-ls fallback for SQL buffers.
  (add-to-list 'lsp-disabled-clients '(sql-mode sql-ls sqls))

 (void/leader-keys
   "l" '(:keymap lsp-command-map :which-key "lsp")))

;; @angular/language-server loads `typescript/lib/tsserverlibrary', which
;; TypeScript 7 no longer ships, so a globally installed TypeScript 7 makes
;; the server crash on every start. Resolve TypeScript from the project's own
;; node_modules (Angular pins a 5.x/6.x release there) and refuse to start at
;; all unless every piece the server needs is present.
(require 'subr-x)
(require 'seq)

(defvar void/angular-ls-enabled t
  "When nil, `angular-ls' is never started.")

(defvar void/npm-global-node-modules nil
  "Cached path of the global npm node_modules directory.")

(defun void/npm-global-node-modules ()
  "Return the global npm node_modules directory, or nil when unavailable."
  (or void/npm-global-node-modules
      (setq void/npm-global-node-modules
            (let ((prefix (ignore-errors
                            (string-trim
                             (shell-command-to-string "npm config get --global prefix")))))
              (when (and prefix (file-directory-p prefix))
                (let ((modules (expand-file-name
                                (if (eq system-type 'windows-nt)
                                    "node_modules"
                                  "lib/node_modules")
                                prefix)))
                  (and (file-directory-p modules) modules)))))))

(defun void/angular-ts-probe-location (root)
  "Return ROOT's node_modules when it holds a usable tsserverlibrary."
  (let ((modules (expand-file-name "node_modules" root)))
    (and (file-exists-p (expand-file-name "typescript/lib/tsserverlibrary.js" modules))
         modules)))

(defun void/angular-ng-probe-locations (root)
  "Return directories that provide @angular/language-service for ROOT."
  (let* ((global-modules (void/npm-global-node-modules))
         (candidates (delq nil
                           (list (expand-file-name "node_modules" root)
                                 global-modules
                                 (when global-modules
                                   (expand-file-name
                                    "@angular/language-server/node_modules"
                                    global-modules))))))
    (seq-filter (lambda (directory)
                  (file-directory-p
                   (expand-file-name "@angular/language-service" directory)))
                candidates)))

(defun void/angular-ls-program (root)
  "Return the command that runs the Angular language server for ROOT."
  (let* ((global-modules (void/npm-global-node-modules))
         (entry (seq-find
                 #'file-exists-p
                 (delq nil
                       (list (expand-file-name
                              "node_modules/@angular/language-server/index.js" root)
                             (when global-modules
                               (expand-file-name
                                "@angular/language-server/index.js" global-modules))))))
         (node (executable-find "node")))
    (cond ((and entry node) (list node entry))
          ((executable-find "ngserver") (list "ngserver")))))

(defun void/angular-ls-root (&optional directory)
  "Return the Angular project root when `angular-ls' can start for DIRECTORY."
  (let ((root (and void/angular-ls-enabled
                   (void/angular-project-p directory))))
    (and root
         (void/angular-ls-program root)
         (void/angular-ts-probe-location root)
         (void/angular-ng-probe-locations root)
         root)))

(defun void/angular-ls-command ()
  "Return the `angular-ls' command line for the current buffer."
  (let ((root (or (void/angular-ls-root)
                  (user-error "Angular language server prerequisites are missing"))))
    (append (void/angular-ls-program root)
            (list "--stdio"
                  "--tsProbeLocations" (void/angular-ts-probe-location root)
                  "--ngProbeLocations" (string-join (void/angular-ng-probe-locations root) ",")))))

(defun void/angular-ls-activate-p (&rest _)
  "Return non-nil when `angular-ls' should attach to the current buffer."
  (and buffer-file-name
       (string-match-p "\\.\\(html\\|ts\\)\\'" buffer-file-name)
       (void/angular-ls-root (file-name-directory buffer-file-name))
       t))

(defun void/toggle-angular-ls ()
  "Toggle whether `angular-ls' is allowed to start."
  (interactive)
  (setq void/angular-ls-enabled (not void/angular-ls-enabled))
  (message "angular-ls %s" (if void/angular-ls-enabled "enabled" "disabled")))

(defun void/angular-ls-diagnose ()
  "Report what `angular-ls' resolves for the current buffer."
  (interactive)
  (let ((root (void/angular-project-p)))
    (message "angular.json root: %s | server: %s | ts probe: %s | ng probes: %s"
             root
             (and root (void/angular-ls-program root))
             (and root (void/angular-ts-probe-location root))
             (and root (void/angular-ng-probe-locations root)))))

(with-eval-after-load 'lsp-mode
  (require 'lsp-angular)
  (let ((client (gethash 'angular-ls lsp-clients)))
    (when client
      ;; lsp--client accessors register their setf forms only after lsp-mode
      ;; loads, so expand these assignments at configuration time.
      (eval `(setf (lsp--client-new-connection ,client)
                   (lsp-stdio-connection #'void/angular-ls-command)))
      (eval `(setf (lsp--client-activation-fn ,client)
                   #'void/angular-ls-activate-p)))))

(void/leader-keys
  "ta" '(void/toggle-angular-ls :which-key "toggle angular-ls")
  "tA" '(void/angular-ls-diagnose :which-key "diagnose angular-ls"))

;; SQLToolsService is downloaded on demand. sqlcmd is also required for
  ;; `void/sql-ms'.
  (defconst void/lsp-mssql-sqltoolsservice-version "6.0.20260731.1")
  (defconst void/lsp-mssql-sqltoolsservice-checksums
    '(("Microsoft.SqlTools.ServiceLayer-linux-arm64-net10.0.tar.gz" . "221a5ccbe9e7f81c4b920da40b3f6264d0df16d650e70a4dc7cc727b89e20ffd")
      ("Microsoft.SqlTools.ServiceLayer-linux-x64-net10.0.tar.gz" . "cf1ea0041de66079841a6bf7d76a3ce40679454acf4dfedd301a71d672b75d9a")
      ("Microsoft.SqlTools.ServiceLayer-osx-arm64-net10.0.tar.gz" . "576577b5f02db8fa5aea4593519116a9a056b15b88254b7ce2534c03554d512d")
      ("Microsoft.SqlTools.ServiceLayer-osx-x64-net10.0.tar.gz" . "2d4880d50671df4a96311fcb767d7b552180dc3147d2f26cdda2e5cabfd794c2")
      ("Microsoft.SqlTools.ServiceLayer-win-arm64-net10.0.zip" . "ab4e69b9b293cb060cea04e0dc1d8295e9cbfc481f7a49027962a4ebb89e558f")
      ("Microsoft.SqlTools.ServiceLayer-win-x64-net10.0.zip" . "0f10c2f91b85fffb87d69036613860bcd9ab24b699142802d603b79c7c901eda")
      ("Microsoft.SqlTools.ServiceLayer-win-x86-net10.0.zip" . "418e7c241059404266c1e1d527c9a20e74effe13429ddeebfbd7a942e5e4d481")))
  (defconst void/lsp-mssql-install-marker ".sqltoolsservice-version")

  ;; Set this before lsp-mssql loads so it never selects the obsolete directory.
  (setq lsp-mssql-download-location
        (expand-file-name (format "mssql/%s/" void/lsp-mssql-sqltoolsservice-version)
                          user-emacs-directory))

  (defun void/lsp-mssql-deferred ()
    "Load the MSSQL client before starting LSP in a SQL buffer.

Managed SQL workspaces started by `void/sql-workspace' launch LSP only
after the interactive workspace command has returned, avoiding minibuffer
project-root prompts racing with `SPC m s'."
    (require 'lsp-mssql)
    (unless (bound-and-true-p void/sql-workspace-connection)
      (lsp-deferred)))

  (add-hook 'sql-mode-hook #'void/lsp-mssql-deferred)

  (use-package lsp-mssql
    :defer t
    :config
    ;; lsp-mssql still defaults to an obsolete .NET Core 2.2 server release.
    ;; Keep the package untouched and use a current, pinned SQLToolsService.
    (defun void/lsp-mssql-server-archive ()
      "Return the SQLToolsService archive for this operating system."
      (let ((architecture (cond
                           ((string-match-p "aarch64\\|arm64" system-configuration) "arm64")
                           ((string-match-p "x86_64\\|amd64" system-configuration) "x64")
                           ((and (eq system-type 'windows-nt)
                                 (string-match-p "i[3-6]86" system-configuration)) "x86")
                           (t (user-error "Unsupported SQLToolsService architecture: %s"
                                          system-configuration))))
            (platform (pcase system-type
                        ('gnu/linux "linux")
                        ('darwin "osx")
                        ('windows-nt "win")
                        (_ (user-error "Unsupported SQLToolsService platform: %s"
                                       system-type)))))
        (format "Microsoft.SqlTools.ServiceLayer-%s-%s-net10.0.%s"
                platform architecture (if (eq system-type 'windows-nt) "zip" "tar.gz"))))

    (defun void/lsp-mssql-launcher-present-p (directory)
      "Return non-nil when DIRECTORY contains the SQLToolsService launcher."
      (let ((launcher (expand-file-name "MicrosoftSqlToolsServiceLayer" directory)))
        (or (file-executable-p launcher)
            (and (eq system-type 'windows-nt)
                 (file-exists-p (concat launcher ".exe"))))))

    (defun void/lsp-mssql-server-present-p (directory)
      "Return non-nil when DIRECTORY contains this completed server archive."
      (let ((marker (expand-file-name void/lsp-mssql-install-marker directory)))
        (and (void/lsp-mssql-launcher-present-p directory)
             (file-readable-p marker)
             (string-equal
              (with-temp-buffer
                (insert-file-contents marker)
                (string-trim (buffer-string)))
              (concat void/lsp-mssql-sqltoolsservice-version "\n"
                      (void/lsp-mssql-server-archive))))))

    (defun void/lsp-mssql-archive-sha256 (file)
      "Return the SHA-256 digest of FILE's literal bytes."
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (secure-hash 'sha256 (current-buffer))))

    (defun void/lsp-mssql-extract (archive target-directory)
      "Extract verified ARCHIVE into TARGET-DIRECTORY safely."
      (pcase system-type
        ('windows-nt
         (let ((process-environment (copy-sequence process-environment)))
           (setenv "SQLTOOLS_ARCHIVE" archive)
           (setenv "SQLTOOLS_DESTINATION" target-directory)
           (call-process "powershell" nil nil t
                         "-NoProfile" "-NonInteractive" "-Command"
                         (concat "Add-Type -AssemblyName System.IO.Compression.FileSystem;"
                                 "[IO.Compression.ZipFile]::ExtractToDirectory("
                                 "$env:SQLTOOLS_ARCHIVE, $env:SQLTOOLS_DESTINATION)"))))
        ((or 'gnu/linux 'darwin)
         (call-process "tar" nil nil t "xf" archive "-C" target-directory))
        (_ (error "Unsupported SQLToolsService platform: %s" system-type))))

    (defun void/lsp-mssql-server-command ()
      "Return the verified SQLToolsService command, installing it if requested."
      (let ((server (expand-file-name "MicrosoftSqlToolsServiceLayer"
                                      lsp-mssql-download-location)))
        (unless (void/lsp-mssql-server-present-p lsp-mssql-download-location)
          (unless (y-or-n-p "SQLToolsService is not installed. Download it? ")
            (user-error "SQLToolsService is not installed"))
          (lsp-mssql-download-server)
          (unless (void/lsp-mssql-server-present-p lsp-mssql-download-location)
            (error "SQLToolsService installation did not complete")))
        (list server)))

    (defun void/lsp-mssql-download-server ()
      "Download a current SQLToolsService release for `lsp-mssql'."
      (interactive)
      (let* ((archive (void/lsp-mssql-server-archive))
             (expected-checksum
              (alist-get archive void/lsp-mssql-sqltoolsservice-checksums nil nil #'string=))
             (url (format "https://github.com/microsoft/sqltoolsservice/releases/download/%s/%s"
                          void/lsp-mssql-sqltoolsservice-version archive))
             (install-directory (directory-file-name lsp-mssql-download-location))
             (install-parent (file-name-directory install-directory))
             download-location staging-directory backup-directory)
        (unless expected-checksum
          (error "No SHA-256 is pinned for SQLToolsService archive %s" archive))
        (unwind-protect
            (progn
              (make-directory install-parent t)
              (setq download-location
                    (make-temp-file "sqltoolsservice-" nil (concat "-" archive)))
              ;; A sibling staging directory keeps the final rename atomic.
              (setq staging-directory
                    (make-temp-file (expand-file-name ".sqltoolsservice-staging-" install-parent) t))
              (lsp--info "Downloading SQLToolsService from %s" url)
              (url-copy-file url download-location t)
              (let ((actual-checksum (void/lsp-mssql-archive-sha256 download-location)))
                (unless (string-equal actual-checksum expected-checksum)
                  (error "SQLToolsService checksum mismatch for %s: expected %s, got %s"
                         archive expected-checksum actual-checksum)))
              (let ((exit-status (void/lsp-mssql-extract download-location staging-directory)))
                (unless (and (integerp exit-status) (zerop exit-status))
                  (error "Failed to extract SQLToolsService archive %s (exit status %S)"
                         archive exit-status)))
              (dolist (file lsp-mssql-executable-files)
                (let ((target-file (expand-file-name file staging-directory)))
                  (when (file-exists-p target-file)
                    (set-file-modes target-file #o755))))
              (unless (void/lsp-mssql-launcher-present-p staging-directory)
                (error "SQLToolsService archive %s did not contain its launcher" archive))
              (with-temp-file (expand-file-name void/lsp-mssql-install-marker staging-directory)
                (insert void/lsp-mssql-sqltoolsservice-version "\n" archive "\n"))
              ;; Defer C-g until either the new installation or the restored one
              ;; is back at the configured path.
              (let ((inhibit-quit t))
                (when (file-directory-p install-directory)
                  (setq backup-directory
                        (make-temp-file (expand-file-name ".sqltoolsservice-backup-" install-parent) t))
                  (delete-directory backup-directory t)
                  (rename-file install-directory backup-directory))
                (condition-case err
                    (rename-file staging-directory install-directory)
                  (error
                   (when (and backup-directory (file-directory-p backup-directory)
                              (not (file-exists-p install-directory)))
                     (rename-file backup-directory install-directory)
                     (setq backup-directory nil))
                   (signal (car err) (cdr err)))))
              (setq staging-directory nil)
              (when (and backup-directory (file-directory-p backup-directory))
                (delete-directory backup-directory t)
                (setq backup-directory nil))
              (lsp--info "Installed SQLToolsService %s" void/lsp-mssql-sqltoolsservice-version))
          (when (and download-location (file-exists-p download-location))
            (delete-file download-location))
          (when (and staging-directory (file-directory-p staging-directory))
            (delete-directory staging-directory t))
          ;; Preserve a backup if restoration itself fails.
          (when (and backup-directory (file-directory-p backup-directory)
                     (file-exists-p install-directory))
            (delete-directory backup-directory t)))))

    (advice-add 'lsp-mssql-download-server :override #'void/lsp-mssql-download-server)

    (let ((client (gethash 'sql lsp-clients)))
      ;; lsp--client accessors register their setf forms only after lsp-mode
      ;; loads, so expand this assignment at configuration time.
      (eval `(setf (lsp--client-new-connection ,client)
                   (lsp-stdio-connection #'void/lsp-mssql-server-command))))

    ;; lsp-mssql calls a removed lsp-treemacs function when opening its explorer.
    (when (and (fboundp 'lsp-treemacs-render)
               (not (fboundp 'lsp-treemacs-initialize)))
      (defun void/lsp-mssql-show-explorer (tree title)
        "Render the MSSQL object explorer with current lsp-treemacs."
        (lsp-treemacs-render tree title 0 "*SQL Object explorer*" nil)
        (with-current-buffer "*SQL Object explorer*"
          (display-buffer-in-side-window (current-buffer) '((side . right)))
          (lsp-mssql-object-explorer-mode)))
      (advice-add 'lsp-mssql--show-explorer :override #'void/lsp-mssql-show-explorer)))

  (require 'sql)

  ;; Keep sqlcmd result sets compact and visually scannable in SQLi.
  (setq sql-ms-program "sqlcmd"
        sql-ms-options '("-w" "200" "-W" "-s" "|"))

  ;; Do not visually wrap wide result rows in the SQLi output buffer.
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (setq-local truncate-lines t)))

  ;; Use `sql-connection-alist' as the single source of truth for SQLi
  ;; and lsp-mssql.  `SPC m s' creates/reuses one small .sql workspace
  ;; per named connection, so SQLToolsService can attach IntelliSense
  ;; to a normal file-backed `sql-mode' buffer.
  (defconst void/sql-workspace-directory
    (expand-file-name ".cache/sql-workspaces/" user-emacs-directory))

  ;; Store only the connection name in the workspace buffer.  Credentials are
  ;; rebuilt from `sql-connection-alist' immediately before each LSP database
  ;; connection so stale/nil cached profiles cannot lose the SQL login.
  (defvar-local void/sql-workspace-connection nil
    "Name of the `sql-connection-alist' entry for this SQL workspace.")

  (defvar void/sql-workspace-pending-connection nil
    "Connection name being installed while a SQL workspace buffer is opened.")

  (defun void/sql-connection-value (connection variable)
    "Return VARIABLE's evaluated value from named SQL CONNECTION."
    (let* ((settings (cdr (assoc-string connection sql-connection-alist t)))
           (entry (assq variable settings)))
      (when entry
        ;; Match `sql-connect', which evaluates values stored in
        ;; `sql-connection-alist'.
        (eval (cadr entry)))))

  (defun void/sql-connection-lsp-profile (connection)
    "Build a fresh lsp-mssql profile from named SQL CONNECTION."
    (let* ((product (void/sql-connection-value connection 'sql-product))
           (server (void/sql-connection-value connection 'sql-server))
           (database (void/sql-connection-value connection 'sql-database))
           (user (void/sql-connection-value connection 'sql-user))
           (password (void/sql-connection-value connection 'sql-password)))
      (unless (eq product 'ms)
        (user-error "SQL connection <%s> is not a Microsoft SQL connection"
                    connection))
      (unless (and (stringp server) (not (string-empty-p server)))
        (user-error "SQL connection <%s> has no sql-server" connection))
      (when (functionp password)
        (setq password (funcall password)))
      (if (and (stringp user) (not (string-empty-p user)))
          (progn
            (unless (and (stringp password) (not (string-empty-p password)))
              (user-error
               "SQL connection <%s> has sql-user but no sql-password"
               connection))
            ;; SQLToolsService expects SqlLogin for username/password auth.
            (list :server server
                  :database (or database "")
                  :user user
                  :password password
                  :authenticationType "SqlLogin"))
        ;; `sql-comint-ms' uses -E when no SQL user is supplied, so keep
        ;; the LSP side aligned by using SQLToolsService integrated auth.
        (list :server server
              :database (or database "")
              :authenticationType "Integrated"))))

  (defun void/sql-refresh-lsp-connections ()
    "Derive `lsp-mssql-connections' from Microsoft SQL connections."
    (setq lsp-mssql-connections
          (vconcat
           (delq nil
                 (mapcar
                  (lambda (entry)
                    (let ((name (car entry)))
                      (when (eq (void/sql-connection-value name 'sql-product) 'ms)
                        (void/sql-connection-lsp-profile name))))
                  sql-connection-alist)))))

  (defun void/sql-workspace-ensure-project-root ()
    "Create the managed SQL workspace directory and Projectile root marker."
    (make-directory void/sql-workspace-directory t)
    ;; lsp-mode can auto-guess roots through Projectile.  Making this managed
    ;; directory an explicit Projectile project removes the interactive
    ;; "not part of any project" prompt for generated SQL workspaces.
    (let ((marker (expand-file-name ".projectile" void/sql-workspace-directory)))
      (unless (file-exists-p marker)
        (write-region "" nil marker nil 'silent))))

  (defun void/sql-workspace-file (connection)
    "Return the persistent scratch SQL file for CONNECTION."
    (void/sql-workspace-ensure-project-root)
    (expand-file-name
     (concat
      (replace-regexp-in-string "[^[:alnum:]_.-]+" "_" connection)
      ".sql")
     void/sql-workspace-directory))

  (defun void/sql-workspace-connect-lsp ()
    "Request an MSSQL database connection for the current SQL workspace."
    (when void/sql-workspace-connection
      ;; One-shot for this initialization.  Re-running `SPC m s' can call
      ;; this function directly for an already initialized workspace.
      (remove-hook 'lsp-after-initialize-hook
                   #'void/sql-workspace-connect-lsp t)
      (condition-case err
          (let ((profile
                 ;; Rebuild credentials at the last possible moment rather
                 ;; than relying on a buffer-local cached profile.
                 (void/sql-connection-lsp-profile
                  void/sql-workspace-connection)))
            (lsp-mssql-connect profile)
            ;; `lsp-mssql-connect' is asynchronous.  SQLToolsService reports
            ;; actual success/failure later through its connection notification.
            (message "MSSQL IntelliSense connection requested for %s/%s"
                     (plist-get profile :server)
                     (or (plist-get profile :database) "")))
        (error
         (message "MSSQL LSP database connection request failed: %s"
                  (error-message-string err))))))

  (defun void/sql-workspace-prepare-lsp ()
    "Attach the pending SQL connection name before SQL mode hooks continue."
    (when void/sql-workspace-pending-connection
      (setq-local void/sql-workspace-connection
                  void/sql-workspace-pending-connection)))

  ;; Run before `void/lsp-mssql-deferred'.  The latter notices the managed
  ;; workspace connection and deliberately does not start LSP from sql-mode-hook.
  ;; `void/sql-workspace' starts it after its interactive work is finished.
  (add-hook 'sql-mode-hook #'void/sql-workspace-prepare-lsp -90)

  (defun void/sql-workspace-start-lsp (buffer)
    "Start lsp-mssql for managed SQL workspace BUFFER after command completion."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when void/sql-workspace-connection
          (require 'lsp-mssql)
          ;; The managed workspace has a .projectile marker, so these settings
          ;; let lsp-mode select it without prompting in the minibuffer.
          (setq-local lsp-auto-guess-root t
                      lsp-guess-root-without-session t)
          (add-hook 'lsp-after-initialize-hook
                    #'void/sql-workspace-connect-lsp nil t)
          (if (and (bound-and-true-p lsp-mode)
                   (fboundp 'lsp-workspaces)
                   (lsp-workspaces))
              (void/sql-workspace-connect-lsp)
            (lsp-deferred))))))

  (defun void/sql-workspace (connection)
    "Open SQLi and an LSP-backed editor using named SQL CONNECTION.

The selected `sql-connection-alist' entry supplies the server, database,
user, and password to both SQLi/sqlcmd and lsp-mssql.  The workspace
buffer remembers only CONNECTION; lsp-mssql credentials are regenerated
from `sql-connection-alist' immediately before each database connection.
The workspace buffer is associated with the same SQLi session, so
`sql-send-region' and related commands use the selected connection."
    (interactive
     (if sql-connection-alist
         (list (sql-read-connection "Connection: "))
       (user-error "No SQL Connections defined")))
    (unless (executable-find sql-ms-program)
      (user-error "sqlcmd is not installed or is not on PATH"))
    (require 'lsp-mssql)
    (let* ((workspace-file (void/sql-workspace-file connection))
           (sqli-buffer (sql-connect connection))
           workspace-buffer)
      (unless (buffer-live-p sqli-buffer)
        (user-error "SQLi connection <%s> did not start" connection))

      (unless (file-exists-p workspace-file)
        (write-region "" nil workspace-file nil 'silent))

      ;; On first visit, `sql-mode-hook' sees this dynamic value and records
      ;; only the selected connection name before the normal MSSQL hook runs.
      (let ((void/sql-workspace-pending-connection connection))
        (find-file workspace-file))
      (setq workspace-buffer (current-buffer))

      (unless (derived-mode-p 'sql-mode)
        (sql-mode))
      (sql-set-product 'ms)
      (setq-local sql-buffer (buffer-name sqli-buffer))
      (setq-local void/sql-workspace-connection connection)

      ;; Keep manual lsp-mssql commands/object explorer synchronized with SQLi.
      (void/sql-refresh-lsp-connections)

      ;; Start LSP only after this interactive command returns.  Using an idle
      ;; timer avoids the previous "attempted to use minibuffer while in
      ;; minibuffer" race during project-root resolution.
      (run-with-idle-timer 0.1 nil
                           #'void/sql-workspace-start-lsp workspace-buffer)

      (message "SQL workspace <%s> opened; SQLi buffer: %s"
               connection (buffer-name sqli-buffer))))

  (void/leader-keys
    "m"  '(:ignore t :which-key "mssql/sql")
    "mc" '(lsp-mssql-connect :which-key "connect lsp")
    "md" '(lsp-mssql-disconnect :which-key "disconnect lsp")
    "mb" '(lsp-mssql-execute-buffer :which-key "execute buffer")
    "mr" '(lsp-mssql-execute-region :which-key "execute region")
    "mx" '(lsp-mssql-cancel :which-key "cancel query")
    "mo" '(lsp-mssql-object-explorer :which-key "object explorer")
    "ms" '(void/sql-workspace :which-key "open sql workspace")
    "mp" '(sql-send-paragraph :which-key "send paragraph")
    "mR" '(sql-send-region :which-key "send region")
    "mB" '(sql-send-buffer :which-key "send buffer"))

;; External formatters: prettier, csharpier, and sqlfluff.
(use-package apheleia
  :config
  (setf (alist-get 'prettier-angular apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=angular"))
  (setf (alist-get 'sqlfluff-tsql apheleia-formatters)
        '("sqlfluff" "format" "--disable-progress-bar" "--dialect" "tsql"
          "--stdin-filename" filepath "-"))
  (setf (alist-get 'sql-mode apheleia-mode-alist) 'sqlfluff-tsql)

  (defun void/apheleia-web-mode-formatter ()
    "Choose the appropriate Prettier parser for a Web Mode buffer."
    (setq-local apheleia-formatter
                (if (void/angular-project-p default-directory)
                    'prettier-angular
                  'prettier-html)))

  (add-hook 'web-mode-hook #'void/apheleia-web-mode-formatter)
  (with-eval-after-load 'lsp-mode
    (define-key lsp-command-map (kbd "f") #'apheleia-format-buffer)
    (define-key lsp-command-map (kbd "C-f")
                (lambda () (interactive) (let ((current-prefix-arg '(4)))
                                           (call-interactively #'apheleia-format-buffer))))))

;; Put private database profiles in ~/.emacs.d/local.el (ignored by Git).
;; `sql-connection-alist' is the single source of truth for SQLi and
;; lsp-mssql. Managed workspaces store only the connection name and rebuild
;; the LSP profile from this alist whenever they connect.
;; Example:
;; (setq sql-connection-alist
;;       '(("example" (sql-product 'ms) (sql-server "HOST")
;;          (sql-database "DATABASE") (sql-user "USER")
;;          (sql-password "PASSWORD"))))
(let ((local-settings (locate-user-emacs-file "local.el")))
  (when (file-exists-p local-settings)
    (load local-settings nil 'nomessage)))
(when (fboundp 'void/sql-refresh-lsp-connections)
  (void/sql-refresh-lsp-connections))

(defun void/lsp-mode-setup ()
  (setq lsp-header-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(add-hook 'lsp-mode-hook #'void/lsp-mode-setup)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)
              ("TAB"   . company-complete-selection)
              :map lsp-mode-map
              ("<tab>" . company-indent-or-complete-common)
              ("TAB"   . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; Sideline configuration
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  
  ;; Documentation popup configuration (the "hover" effect)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'at-point))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Treemacs was only ever arriving as an lsp-treemacs dependency; declare it
;; so it is installed and configured on its own terms.
(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-width 35)
  (treemacs-follow-after-init t))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy
  :defer t)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)
  :init
  (void/leader-keys
    "kc" '(evilnc-comment-or-uncomment-lines :which-key "comment lines")))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; (if (eq system-type 'windows-nt)
;;     (progn
;;       ;; Windows setup: Use PowerShell Core (pwsh)
;;       (setq explicit-shell-file-name "powershell")
;;       (setq explicit-powershell-args '("-NoLogo"))
      
;;       ;; Set pwsh for background shell commands
;;       (setq shell-file-name "powershell")
;;       (setq shell-command-switch "-Command"))
      
;;   ;; Non-Windows setup (Linux/macOS fallback)
;;   (progn
;;     (setq explicit-shell-file-name "/bin/bash")
;;     (setq shell-file-name "/bin/bash")
;;     (setq shell-command-switch "-c")))
