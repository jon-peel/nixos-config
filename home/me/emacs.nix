{ config, pkgs, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs;  # replace with pkgs.emacs-gtk, or a version provided by the community overlay if desired.
  	  extraPackages = epkgs: [
  	    epkgs.use-package
  	    epkgs.cask
  	    epkgs.counsel-projectile
        epkgs.doom-modeline
        epkgs.doom-themes
  	    epkgs.nix-mode
        epkgs.forge
        epkgs.general
        epkgs.helpful
        epkgs.hydra
        epkgs.evil # Vim keybindings
        epkgs.evil-collection
        # epkgs.evil-magit
        epkgs.magit # Git integration
        epkgs.org-bullets
        # epkgs.org-habit
        epkgs.which-key # Keybinding suggestions
        epkgs.ivy # Fuzzy completion
        epkgs.ivy-rich
        epkgs.counsel # Ivy-based commands
        epkgs.swiper # Fuzzy search
        epkgs.company # Auto-completion
        epkgs.projectile # Project management
        epkgs.rainbow-delimiters # Colorful brackets
        epkgs.visual-fill-column
      ];
  };

  home.file.".emacs.d/.keep".text = "";
  home.file.".emacs.d/init.el".source = ./init.el;
}
