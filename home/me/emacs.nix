{ config, pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;  # replace with pkgs.emacs-gtk, or a version provided by the community overlay if desired.
  	extraPackages = epkgs: [
      epkgs.dashboard
      epkgs.ascii-art-to-unicode
  	  epkgs.use-package
  	  epkgs.cask
  	  epkgs.consult
      epkgs.doom-modeline
      epkgs.doom-themes
  	  epkgs.nix-mode
      # epkgs.forge
      epkgs.fsharp-mode
      epkgs.general
      epkgs.helpful
      epkgs.hydra
      epkgs.evil # Vim keybindings
      epkgs.evil-collection
      epkgs.evil-nerd-commenter
      epkgs.pdf-tools
      epkgs.all-the-icons
      epkgs.all-the-icons-dired
      epkgs.nerd-icons 
      epkgs.dired-open
      epkgs.dired-hide-dotfiles
      
      # epkgs.evil-magit
      epkgs.lsp-mode
      epkgs.lsp-ui
      epkgs.lsp-treemacs
      epkgs.lsp-ivy
      epkgs.magit # Git integration
      epkgs.org-bullets
      # epkgs.org-habit
      epkgs.which-key # Keybinding suggestions
      epkgs.ivy # Fuzzy completion
      epkgs.ivy-rich
      epkgs.counsel # Ivy-based commands
      epkgs.swiper # Fuzzy search
      epkgs.typescript-mode
      epkgs.company # Auto-completion
      epkgs.company-box
      epkgs.rainbow-delimiters # Colorful brackets
      epkgs.visual-fill-column
      # shells
      epkgs.vterm
      epkgs.eshell-z
      epkgs.eshell-git-prompt
    ];
  };
  
  home.file.".emacs.d/.keep".text = "";
  home.file.".emacs.d/init.el".source = ./init.el;
}
