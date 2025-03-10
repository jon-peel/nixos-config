(setq inhibit-startup-message t)


(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "Fira Code" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 100)
(set-face-attribute 'variable-pitch nil :font "Noto Serif" :height 110 :weight 'regular )



(use-package doom-themes)
(load-theme 'adwaita)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

    ;; Enable which-key for keybinding suggestions
    (require 'which-key)
    (which-key-mode)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
  :diminish
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


(use-package ivy-rich :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package general
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader-keys
    "b"  '(:ignore t :which-key "buffers")
    "bk" '((lambda () (interactive) (kill-buffer (current-buffer)))  :which-key "kill buffer")
    "bs" '(counsel-switch-buffer :which-key "switch buffer")
    
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))



    ;; Enable Company for auto-completion
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode)

;; Enable Projectile for project management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Enable Rainbow Delimiters for colorful brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Enable visual-fill-column after the package is loaded
(with-eval-after-load 'visual-fill-column
  (setq visual-fill-column-width 120) ; Set the width of the text column
  (setq visual-fill-column-center-text t) ; Center the text
  (global-visual-fill-column-mode 1)) ; Enable globally


(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(my/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))


;; Reload nix home manager
(defun my/run-home-manager-switch ()
  "Run 'home-manager switch' and reload the Emacs configuration."
  (interactive)
  (async-shell-command "home-manager switch")
  (sleep-for 2))

(my/leader-keys
  "n"   '(:ignore t :which-key "nix")
  "nh"  '(:prefix t :which-key "home manager")
  "nhs" '(my/run-home-manager-switch :which-key "switch")
  "nhe" '( (lambda()(interactive)(find-file-existing "~/.config/home-manager/home.nix")) :which-key "edit")
  )


;; quick file open shortcuts
(my/leader-keys
  "f"  '(:ignore t :which-key "files")	
  "."  '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "files")) 


;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;(use-package evil-magit
;  :after magit)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)




;; ORG MODE
(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(setq org-directory "~/OneDrive/org/")

(defun my/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "noto sans" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (my/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . my/org-mode-visual-fill))



(setq org-capture-templates
      '(
        ;; ("j" "Journal")
        ;; ("jj" "journal" entry (file+datetree "~/OneDrive/org/journal.org")
        ;;  "\n\n* %U\n%?")
        ;; ("jt" "journal" entry (file+datetree "~/OneDrive/org/journal.org")
        ;;  "* [ ] %?\nSCHEDULED: %t")

        ("j" "Journal")
        ("jj" "Journal" entry
         (file+olp+datetree "journal.org" "Journal")
         "* Entry - %<%H:%M>\n%U\n\n%?"
         :empty-lines 1)
        ("jg" "Goals" entry
         (file+olp+datetree "journal.org" "Journal")
         "* TODO Goals - %<%d %B %Y> [/]\nSCHEDULED: %t\n** [ ] %?"
         :prepend t)

        ("b" "blog-post" entry (file+olp "~/repos/blog-home/blog.org" "blog")
         "* TODO %^{Title} %^g \n:PROPERTIES:\n:EXPORT_FILE_NAME: %^{Slug}\n:EXPORT_DATE: %T\n:END:\n\n%?"
         :empty-lines-before 2)

        ("m" "Email Workflow")
        ("mf" "Follow Up" entry (file+olp "~/OneDrive/org/mail.org" "Follow Up")
         "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\n\n%i")
        ("mr" "Read Later" entry (file+olp "~/OneDrive/org/mail.org" "Read Later")
         "* TODO Read %a\nSCHEDULED:%t\n\n%i")

      ("s" "Sleep Entry" table-line
         (file+headline "sleep.org" "Data")
         "|#|%^{Date}u|%^{Move (kcal)}|%^{Exercise (min)}|%^{Caffeine (mg)}|%^{Tim in daylight (min)}|%^{Time in bed}|%^{Time out of bed}|%^{Sleep Duration (h:mm)}||%^{Tags}g|"
         :immediate-finish t :jump-to-captured t
         )

        ("t" "Task" entry
         (file+headline "tasks.org" "Tasks")
         "** TODO %? %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

        ("T" "Task with Deadline" entry
         (file+headline "tasks.org" "Tasks")
         "** TODO %?  %^g\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)

        ))
