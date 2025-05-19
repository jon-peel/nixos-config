;;  (all-the-icons-install-fonts t)
;; (dolist (font all-the-icons-font-names)
;;  (print (cons font (find-font (font-spec :name font)))))

(setq inhibit-startup-message t)

;; Set a dark theme quickly, becase the doom theme sometimes takes a while to laod
;; this prevents a prolonged white screen when starting
(load-theme 'tango-dark t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(setq visible-bell t)

(global-auto-revert-mode t)
(setq auto-revert-verbose nil)           ; Suppress messages from auto-revert
(setq global-auto-revert-non-file-buffers t) ; Also revert Dired and other buffers

(setq revert-without-query '(".*"))

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

(my/leader-keys
  "e"  '(:ignore t :which-key "emacs")
  "ed"    '(dashboard-open :which-key "dashboard")
  "ec"    '((lambda () (interactive) (find-file "/home/me/nixos-config/home/me/emacs.org")) :which-key "config file"))

(my/leader-keys
  "f"  '(:ignore t :which-key "files")	
  "."  '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "files")
  "fd" '(dired :which-key "dired")
  )

(defun insert-latex-multicols ()
  "Insert a LaTeX multicols environment for Org mode."
  (interactive)
  (insert "
#+BEGIN_EXPORT latex
\\end{multicols}
#+END_EXPORT
    
#+BEGIN_EXPORT latex
\\begin{multicols}{2}
#+END_EXPORT
    "))

(my/leader-keys
  "s"  '(:ignore t :which-key "snippets")	
  "sm"  '(insert-latex-multicols :which-key "multicol"))

(require 'zone)
(zone-when-idle 60)

(setq inhibit-compacting-font-caches t)

(set-face-attribute 'default nil :font "Fira Code" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 100)
(set-face-attribute 'variable-pitch nil :font "Noto Serif" :height 110 :weight 'regular )

(use-package doom-themes)
(load-theme 'doom-acario-dark t)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

;; Display column number of mode line
(set-face-attribute 'mode-line nil :height 120)
(column-number-mode)

(global-display-line-numbers-mode 0)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		tetris-mode
                term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		vterm-mode-hook
		pdf-view-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq split-height-threshold nil)
(setq split-width-threshold 60)

(with-eval-after-load 'visual-fill-column
   (setq visual-fill-column-width 120) ; Set the width of the text column
   (setq visual-fill-column-center-text t) ; Center the text
   (global-visual-fill-column-mode 1)) ; Enable globally

(defun my/tetris-mode-visual-fill ()
    (setq visual-fill-column-width 40
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (tetris-mode . my/tetris-mode-visual-fill))

(defun my/snake-mode-visual-fill ()
    (setq visual-fill-column-width 80
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (snake-mode . my/tetris-mode-visual-fill))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(my/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(require 'which-key)
(which-key-mode)

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

(advice-add 'org-set-tags-command :override 'counsel-org-tag)

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
  ([remap org-set-tags-command] . councel-org-tag)
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

 (add-hook 'after-init-hook 'global-company-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
;; (use-package forge)

(my/leader-keys
  "g"  '(:ignore t :which-key "git")
  "gg" '(magit-status  :which-key "magit status"))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(my/leader-keys
   "l"   '(:ignore t :which-key "lsp mode")
   "ld"  '(flymake-show-project-diagnostics :which-key "diagnostics")
   "lt"  '(:prefix t :which-key "tree")
   "lts" '(lsp-treemacs-symbols :which-key "symbols")
   "ls"  '(lsp-ivy-workspace-symbol :which-key "find symbol")
   )

(use-package fsharp-mode
  :mode " \\.fs[iylx]?$'"
  :hook (fsharp-mode . lsp-deferred)
  :config
  (autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
  (autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)
  (autoload 'mdbg "mdbg" "The CLR debugger" t)
  ;; (setq inferior-fsharp-program "PATH_TO_YOUR_FSI_EXE")
  ;; (setq fsharp-compiler "PATH_TO_YOUR_FSC_EXE")
  (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
        )

(use-package nix-mode
  :mode " \\.nix$"
  :hook (nix-mode . lsp-deferred)
  :config
  (add-to-list 'auto-mode-alist '("\\.nix$" . nix-mode)))
      
;    (with-eval-after-load 'lsp-mode
;      (lsp-register-client
;        (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
;                         :major-modes '(nix-mode)
;                         :priority 0
;                         :server-id 'nixd)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Basic project.el configuration
(require 'project)

;; If you want project.el to use specific version control systems
(setq project-vc-extra-root-markers '(".project" "package.json" "Cargo.toml" "pyproject.toml"))

(defun my/create-project-marker ()
  "Create a .project file in the current directory and register it as a project."
  (interactive)
  (let ((project-dir (read-directory-name "Create project in directory: " default-directory)))
    (with-temp-buffer
      (write-file (expand-file-name ".project" project-dir)))
    (message "Created .project marker in %s" project-dir)
    (when (y-or-n-p "Register this directory as a project? ")
      (project-remember-project project-dir))))

(my/leader-keys
  "p"     '(:ignore t :which-key "project")
  "pp"    '(project-switch-project :which-key "switch project")
  "pf"    '(project-find-file :which-key "find file")
  "pb"    '(project-switch-to-buffer :which-key "switch buffer")
  "pd"    '(project-dired :which-key "dired")
  "pg"    '(consult-ripgrep :which-key "ripgrep")
  "pk"    '(project-kill-buffers :which-key "kill buffers")
  "ps"    '(project-shell :which-key "shell")
  "pc"    '(project-compile :which-key "compile")
  "pa"    '(project-remember-project :which-key "add project")
  "pr"    '(project-forget-project :which-key "remove project")
  "pn"    '(my/create-project-marker :which-key "new project marker")
  
  ;; Consult integration
  "pC"    '(:prefix t :which-key "consult")
  "pCb"   '(consult-project-buffer :which-key "project buffers")
  "pCf"   '(consult-find :which-key "find")
  "pCg"   '(consult-grep :which-key "grep"))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  )

(setq org-directory "~/OneDrive/org/")
(setq org-agenda-files '("~/OneDrive/org/tasks.org"
			 "~/OneDrive/org/shed.org"
			 "~/OneDrive/org/journal.org"
                         "~/OneDrive/org/anniversaries.org"))

(my/leader-keys
  "o"    '(:ignore t :which-key "org")
  "oa"   '(org-agenda :which-key "agenda")
  "oc"   '(org-capture :which-key "capture")
  "or"   '(org-refile :which-key "refile")
  "ot"   '(counsel-org-tags :which-key "tags")
  "of"   '((lambda () (interactive) (dired org-directory)) :which-key "files")
  ;; org dates
  "od"   '(:prefix t :which-key "date")
  "od."  '(org-timestamp :which-key "timestamp")
  "od!"  '(org-timestamp-inactive :which-key "inactive")
  "ods"  '(org-schedule :which-key "schedule")
  "odd"  '(org-deadline :which-key "deadline"))

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

(defun my/read-meditations-quotes (author)
      "Read quotes from ~/.emacs.d/quotes/meditations.txt and return them as a list of tuples.
    Each tuple contains (AUTHOR QUOTE-TEXT) where AUTHOR is the provided author name
    and QUOTE-TEXT is the text of a quote from the file."
      (let ((quotes-file "~/.emacs.d/quotes/meditations.txt")
            (quotes-list nil))
        (when (file-exists-p quotes-file)
          (with-temp-buffer
            (insert-file-contents quotes-file)
            (let ((quote-texts (split-string (buffer-string) "\n\n" t)))
              (dolist (quote-text quote-texts)
                ;; Clean up whitespace and add to the list as (author quote) tuple
                (let ((cleaned-quote (string-trim quote-text)))
                  (push (list author cleaned-quote) quotes-list))))))
        (nreverse quotes-list))) ; Return the list in the original order

    (defun my/random-meditation-quote ()
      (let ((quotes (my/read-meditations-quotes "Marcus Aurelius")))
        (if (null quotes)
            nil  ; Return nil if no quotes are found
          (nth (random (length quotes)) quotes))))

  (defun my/org-quote-meditation ()
    "Insert a random meditation quote formatted as an org-mode quote block.
  The quote will be formatted as:
  #+BEGIN_QUOTE
  Quote text goes here
  --- Author
  #+END_QUOTE"
    (interactive)
    (let* ((quote-tuple (my/random-meditation-quote))
           (author (car quote-tuple))
           (quote-text (cadr quote-tuple)))
      (if quote-tuple
          (format "#+BEGIN_QUOTE\n%s\n    ---%s\n#+END_QUOTE" 
                          quote-text author)
        (message "No meditation quotes found"))))

;; (my/org-quote-meditation)

(setq org-capture-templates
      '(("x" "Export D&D Session")
	("xd" "Export Dungeon" plain
	 (file+olp "dnd-session.org" "Random Dungeons")
	 "** %f%?\n#+INCLUDE: ./roam/%f"
	 :immediate-finish t
	 :jump-to-captured t)
      
	("j" "Journal")
	("jj" "Journal" entry
	 (file+olp+datetree "journal.org" "Journal")
	 "* Entry - %<%H:%M> %U\n\n%?"
	 :empty-lines 1
	 :kill-buffer t)
	("jm" "Morning" plain
	 (file+olp+datetree "journal.org" "Journal")
	 "\n%(my/org-quote-meditation)"
	 :prepend t
	 :immediate-finish t
	 :jump-to-captured t
	 )
      
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
          "| |%^{Date}u|%^{Move (kcal)}|%^{Exercise (min)}|%^{Caffeine (mg)}|%^{Tim in daylight (min)}|%^{Time in bed}|%^{Time out of bed}|%^{Sleep Duration (h:mm)}||%^{Tags}g|"
          :immediate-finish t :jump-to-captured t)

         ("t" "Task" entry
          (file+headline "tasks.org" "Tasks")
          "** TODO %? %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)
         
         ("T" "Task with Deadline" entry
          (file+headline "tasks.org" "Tasks")
          "** TODO %?  %^g\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n" :empty-lines 1)))

(setq dnd-org-dir "~/OneDrive/org/roam/dnd/")
(setq dnd-out-dir "~/Documents/dnd-session")
(setq org-publish-project-alist
      `(
  	("dnd-campaign"
         :base-directory ,dnd-org-dir
         :base-extension "org"
         :publishing-directory ,dnd-out-dir
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-doctype "html5"
         :html-html5-fancy t
         :with-toc t
         :section-numbers nil
         :html-head "<link rel=\"stylesheet\" href=\"./dnd-theme.css\" type=\"text/css\"/>
                     <link href=\"https://fonts.googleapis.com/css2?family=Cinzel:wght@400;700&display=swap\"
                           rel=\"stylesheet\">
       <link href=\"https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css\"
             rel=\"stylesheet\">"
         :html-preamble "<div class='campaign-header'>Home Brew Campaign</div>
                        <div class='nav-menu'>
                          <a href='index.html'>Home</a> |
                          <a href='random_locations_and_encounters.html'>Random</a>
                        </div>"
         :html-postamble "<div class='footer'>Campaign notes prepared by %a</div>"
         :auto-sitemap t
         :sitemap-title "D&D Campaign Index"
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         )
        
        ("dnd-static"
         :base-directory ,dnd-org-dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|map"
         :publishing-directory ,dnd-out-dir
         :recursive t
         :publishing-function org-publish-attachment
         )
        
        ("dnd-website" :components ("dnd-campaign" "dnd-static"))
 	))

(require 'org-tempo)
   (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
   (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
   (add-to-list 'org-structure-template-alist '("py" . "src python"))

(setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
            (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)"
			"|" "COMPLETED(c)" "CANC(k@)")))

(require 'org-habit)
 (add-to-list 'org-modules 'org-habit)
 (setq org-habit-graph-column 60)

(setq org-agenda-custom-commands
 '(("d" "Dashboard"
   ((agenda "" ((org-deadline-warning-days 7)))
    (todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))
    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

  ("n" "Next Tasks"
   ((todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))))

  ("W" "Work Tasks" tags-todo "+work-hold")

  ;; Low-effort next actions
  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
   ((org-agenda-overriding-header "Low Effort Tasks")
    (org-agenda-max-todos 20)
    (org-agenda-files org-agenda-files)))

  ("w" "Workflow Status"
   ((todo "WAIT"
          ((org-agenda-overriding-header "Waiting on External")
           (org-agenda-files org-agenda-files)))
    (todo "REVIEW"
          ((org-agenda-overriding-header "In Review")
           (org-agenda-files org-agenda-files)))
    (todo "PLAN"
          ((org-agenda-overriding-header "In Planning")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "BACKLOG"
          ((org-agenda-overriding-header "Project Backlog")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "READY"
          ((org-agenda-overriding-header "Ready for Work")
           (org-agenda-files org-agenda-files)))
    (todo "ACTIVE"
          ((org-agenda-overriding-header "Active Projects")
           (org-agenda-files org-agenda-files)))
    (todo "COMPLETED"
          ((org-agenda-overriding-header "Completed Projects")
           (org-agenda-files org-agenda-files)))
    (todo "CANC"
          ((org-agenda-overriding-header "Cancelled Projects")
           (org-agenda-files org-agenda-files)))))))

(setq org-refile-targets
  '(("archive.org" :maxlevel . 2)
    ("tasks.org" :maxlevel . 1)))
;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (my/org-font-setup))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t) 
   (emacs-lisp . t)
   (gnuplot . t)
   (python . t)
   (shell . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/nixos-config/home/me/emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(setq org-confirm-babel-evaluate nil)

(setq org-ditaa-jar-path nil)  ;; We're not using the jar directly
(setq org-babel-ditaa-command "/run/current-system/sw/bin/ditaa")

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/OneDrive/org/roam")
  (org-roam-completion-everywhere t)
  :config
  (org-roam-setup))

(setq org-roam-capture-templates
      '(("d" "D&D")
	("dn" "new" plain 
         "#+FILETAGS: %^{Tags}g\n\n* ${title}\n%?"
         :target (file+head "dnd/${slug}.org" "#+TITLE: ${title}\n")
	 :immediate-finish t :jump-to-captured t
         :unnarrowed t)
        
        ("n" "note" plain
         "* Notes\n%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)))

(my/leader-keys
  "n"     '(:ignore t :which-key "org-roam")
  "nf"    '(org-roam-node-find :which-key "find/create note")
  "ni"    '(org-roam-node-insert :which-key "insert link")
  "nc"    '(org-roam-capture :which-key "capture")
  "nl"    '(org-roam-buffer-toggle :which-key "toggle buffer")
  "ns"    '(org-roam-db-sync :which-key "sync database")
  "ng"    '(org-roam-graph :which-key "show graph")
  
  ;; Daily notes
  "nd"    '(:prefix t :which-key "dailies")
  "ndt"   '(org-roam-dailies-capture-today :which-key "today")
  "ndp"   '(org-roam-dailies-capture-previous :which-key "previous")
  "ndn"   '(org-roam-dailies-capture-next :which-key "next")
  "ndf"   '(org-roam-dailies-find-directory :which-key "find")
  
  ;; Tags
  "nt"    '(:prefix t :which-key "tags")
  "nta"   '(org-roam-tag-add :which-key "add")
  "ntr"   '(org-roam-tag-remove :which-key "remove")
  
  ;; Quick captures with templates
  "nq"    '(:prefix t :which-key "quick capture")
  "nqd"   '((lambda () (interactive) (org-roam-capture nil "d")) :which-key "default")
  "nqn"   '((lambda () (interactive) (org-roam-capture nil "n")) :which-key "note")
  
  ;; Alias section
  "na"    '(:prefix t :which-key "aliases")
  "naa"   '(org-roam-alias-add :which-key "add")
  "nar"   '(org-roam-alias-remove :which-key "remove"))

(setq dired-dwim-target t)

(use-package dired
      :ensure nil
      :commands (dired dired-jump)
      :bind (("C-x C-j" . dired-jump))
      :custom ((dired-listing-switches "-agho --group-directories-first"))
      :config
      (evil-collection-define-key 'normal 'dired-mode-map
        "h" 'dired-single-up-directory
        "l" 'dired-single-buffer))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
;; (all-the-icons-install-fonts t)
(face-attribute 'default :font)
    
    (use-package dired-open
      :config
      ;; Doesn't work as expected!
      ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
      (setq dired-open-extensions '(("png" . "feh")
                                    ("mkv" . "mpv"))))

    (use-package dired-hide-dotfiles
      :hook (dired-mode . dired-hide-dotfiles-mode)
      :config
      (evil-collection-define-key 'normal 'dired-mode-map
        "H" 'dired-hide-dotfiles-mode))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^.*?\ \ *")
  )

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun eshell-load-zsh-aliases ()
  "Read zsh aliases and add them to the list of eshell aliases."
  ;; Bash needs to be run - temporarily - interactively
  ;; in order to get the list of aliases.
    (with-temp-buffer
      (call-process "zsh" nil '(t nil) nil "-ci" "alias")
      (goto-char (point-min))
      (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
        (eshell/alias (match-string 1) (match-string 2)))))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  
  ;; Bind some useful keys for evil-mode

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)
  
  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "btop" "nvtop" "zsh" "vim")))
  
  (add-hook 'eshell-alias-load-hook 'eshell-load-zsh-aliases)
  (eshell-git-prompt-use-theme 'powerline))

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(setq org-latex-hyperref-template 
      "\\hypersetup{\n  colorlinks=true,\n  linkcolor=blue,\n  filecolor=cyan,\n  urlcolor=magenta,\n  citecolor=green\n}")

;; (setq org-latex-classes ())
(add-to-list
 'org-latex-classes
 '("dndbook"
   "
\\documentclass[10pt,twoside,twocolumn,openany,print,justified]{dndbook}
\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
   "
   ("\\chapter{%s}" . "\\chapter*{%s}")
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
 ))
(add-to-list
 'org-latex-classes
 '("rpg-module"
   "\\RequirePackage{pgfmath}
    \\documentclass[a4paper,acdesc]{rpg-module}."
   ("\\part{%s}" . "\\part{%s}")
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ))
(add-to-list
 'org-latex-classes
 '("koma-article"
   "\\documentclass{scrartcl}"
   ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
 )

;; Custom LaTeX table export environment
(with-eval-after-load 'ox-latex
  (defun my-org-latex-dnd-table (table contents info)
    "Convert an org table to a DndTable when a special property is set."
    (let* ((caption (org-export-get-caption table))
           (caption-str (if caption (format "[header=%s]" 
                                          (org-export-data caption info))
                          "[header=Nice Table]"))
           (attr (org-export-read-attribute :attr_latex table))
           (spec (or (plist-get attr :spec) "XX")))
      (format "\\begin{DndTable}%s{%s}\n%s\n\\end{DndTable}"
              caption-str spec contents)))
  
  (defun my-org-latex-table-wrapper (orig-fun table contents info)
    "Wrapper for org-latex-table that checks for DNDTABLE property."
    (if (org-export-get-node-property :DNDTABLE table)
        (my-org-latex-dnd-table table contents info)
      (funcall orig-fun table contents info)))
  
  ;; Add advice to the original function
  (advice-add 'org-latex-table :around #'my-org-latex-table-wrapper))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  ;; Automatically use pdf-view-mode for .pdf files
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(add-hook 'pdf-view-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (visual-fill-column-mode -1)))

(defun my/run-home-manager-switch ()
"sudo nixos-rebuild switch --flake ~/nixos-config#tuffy"
(interactive)
(async-shell-command "home-manager switch")
(sleep-for 2))

(my/leader-keys
"x"   '(:ignore t :which-key "nix")
"xh"  '(:prefix t :which-key "home manager")
"xhs" '(my/run-home-manager-switch :which-key "switch")
"xhe" '( (lambda()(interactive)(find-file-existing "~/nixos-config/home/me/default.nix")) :which-key "edit")
"xht" '( (lambda()(interactive)(find-file-existing "~/nixos-config/hosts/tuffy/default.nix")) :which-key "tuffy"))

;; Add the 'code' subdirectory containing org-include-generator.el to load-path
(add-to-list 'load-path (expand-file-name "code" (file-name-directory user-init-file)))

;; Load the package
(require 'org-include-generator)

(my/leader-keys
  "d"    '(:ignore t :which-key "D&D")
  "di"   '(org-include-generate-from-current :which-key "includes"))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)

  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  (setq dashboard-startup-banner 2)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  
  (setq dashboard-show-shortcuts t)
  )

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(setq dashboard-startupify-list '(dashboard-insert-banner
                                dashboard-insert-newline
                                dashboard-insert-banner-title
                                dashboard-insert-newline
                                dashboard-insert-navigator
                                dashboard-insert-newline
                                dashboard-insert-init-info
                                dashboard-insert-items
                                dashboard-insert-newline
                                dashboard-insert-footer))

(setq dashboard-set-navigator t)
  (setq dashboard-projects-backend 'project-el)

  ;; To enable cycle navigation between each section:
  (setq dashboard-navigation-cycle t)

;; (setq dashboard-icon-type 'all-the-icons)  ; use `all-the-icons' package
(setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  
  ;; (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
