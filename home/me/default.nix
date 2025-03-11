{ config, pkgs, ... }:

{
  imports = [
    # ./emacs.nix
  ];

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "me";
  home.homeDirectory = "/home/me";
  
  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.


  nixpkgs.config.allowUnfree = true;
  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # Fonts
    noto-fonts
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols

  emacs-all-the-icons-fonts
    font-awesome
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono

    # Coding
    gitAndTools.gh

    # other
    curl
    gnome-software
    veracrypt
    thefuck # that corrects errors in previous console commands.
    
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  fonts.fontconfig.enable = true;
    

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.

  # home.file.".emacs.d/init.el".source = ./init.el;
  home.file = {
    ".config/onedrive/sync_list".text = ''
org
Mongo-export
    '';


    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
   #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/me/etc/profile.d/hm-session-vars.sh
  #

  dconf = {
    enable = true;
    settings."org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = with pkgs.gnomeExtensions; [
        appindicator.extensionUuid
        blur-my-shell.extensionUuid
        gsconnect.extensionUuid
      ];
    };
  };
  
  home.sessionVariables = {
    EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  programs.kitty.enable = true;

  programs.git = {
	enable = true;
	userName = "JonPeel";
	userEmail = "me@jonathanpeel.co.za";
	aliases = {
		pu="push"; 
		co="checkout"; 
		cm="commit";
	};

  };

  gtk = {
	enable = true;
	theme.name = "Adwaita";
	cursorTheme.name = "Adwaita";
	iconTheme.name = "Adwaita";
  };

  xdg.mimeApps.defaultApplications = {
	"text/plain" = [ "emacs" ];
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    # autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    shellAliases = {
      ll = "ls -lh";
      la = "ls -lha";
      update = "sudo nixos-rebuild switch";
    };
	  envExtra = ''
    	 export NAME="Jonathan"
	  '';
    history.size = 10000;
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "thefuck" ];
      theme = "robbyrussell";
    };
  };



  programs.vscode = {
    enable = true;
    profiles.default = {
      extensions = with pkgs.vscode-extensions; [
        ms-vscode-remote.remote-containers
        ms-vscode-remote.vscode-remote-extensionpack
      ];
      userSettings = {
        "editor.fontSize" = 14;
        "files.autoSave" = "afterDelay";
        # "dev.containers.dockerPath" = "podman";
      };
    };
  };
  
  
  
  
  
  
  
  
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
