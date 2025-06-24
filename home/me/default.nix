{pkgs, ... }:

{  imports = [
   # ./mailboxs.nix
   # ./emacs.nix
    ./doom-emacs.nix
    ./vscode.nix
    ./sway-home.nix
  ];


  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "me";
  home.homeDirectory = "/home/me";
  #home-manager.gtk = {
  #  enable = true;
  #    theme = {
  #      name = "Adwaita-dark";
  #      package = pkgs.gnome.gnome-themes-extra;
  #    };
  #};
 
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
    duf
    ncdu
    nodejs
    nixfmt
    graphviz
    shellcheck

    thunderbird
    fd
    jetbrains.rider
    isync
    pandoc   
      firefox
      nvtopPackages.full	
      tree
      gnuplot 
    # Fonts
    noto-fonts
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    emacs-all-the-icons-fonts
    font-awesome
    material-design-icons
    weather-icons
    #nerd-fonts
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    powerline
      powerline-fonts
#      libreoffice
      # Coding
      nixd
    gitAndTools.gh
    github-cli
      # dotnet-sdk_9
      # dotnet-sdk_10
      #  dotnet-sdk_8
      mono
    fsautocomplete

      # other
      htop
      btop
    curl
    gnome-software
    veracrypt
    # thefuck # that corrects errors in previous console commands.
      zsh-powerlevel10k
      remmina
mudlet

      
    # Fun
    xorg.xeyes
    xorg.xclock
    
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
    ".config/i3/config_c".source = ./i3-config;
    ".config/isyncrc".source = ./isyncrc;
    
    ".config/onedrive/sync_list".text = ''
org
Mongo-export
    '';

    ".face".source = ./face.jpg;

    ".config/openbox/autostart".text = ''
mmaker -f -t OpenBox &
xclock -background "#333333" -geometry 150x150+10+10 &
xeyes -geometry 150x60+10+170 &
'';

    ".config/openbox/rc.xml".text = ''
<keybind key="W-space">
  <action name="Execute">
    <command>rofi -show drun</command>
  </action>
</keybind>
'';

    
    ".fvwm/o_config".text = ''

Read $[HOME]/.fvwm/menu
echo '
# Basic FVWM configuration
ImagePath +:$HOME/.fvwm/icons:+/usr/share/icons

# Virtual Desktops
DesktopSize 2x2
EdgeScroll 100 100

# Key bindings
Key F1 A M Menu MenuFvwmRoot
Key Tab A M WindowList Root c c NoDeskSort, SelectOnRelease Meta_L
Key F2 A M Exec exec xterm
Key F4 A M Close

# Mouse bindings
Mouse 1 R A Menu MenuFvwmRoot
Mouse 1 T A Function "MoveOrRaise"
Mouse 1 FS A Resize
Mouse 2 T A Function "MoveOrRaise"

# Functions
DestroyFunc MoveOrRaise
AddToFunc MoveOrRaise
+ I Raise
+ M Move
+ D Lower

# Styles
Style * BorderWidth 5, HandleWidth 5
Style * Colorset 1, HilightColorset 2
Style * MWMButtons, MWMBorder, MWMDecor

# Colors
Colorset 1 fg black, bg #c0c0c0
Colorset 2 fg white, bg #000080

# Simple menu
DestroyMenu MenuFvwmRoot
AddToMenu MenuFvwmRoot "Root Menu" Title
+ "XTerm" Exec exec xterm
+ "Firefox" Exec exec firefox
+ "" Nop
+ "Restart FVWM" Restart
+ "Exit FVWM" Quit
' > ~/.fvwm/config
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
	  userName = "Jonathan Peel";
	  userEmail = "me@jonathanpeel.co.za";
	  aliases = {
		  pu="push"; 
		  co="checkout"; 
		  cm="commit";
	  };
    extraConfig = {
      credential.helper = "store --file ~/.git-credentials";
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
    syntaxHighlighting.enable = true;
    shellAliases = {
      ll = "ls -lh";
      la = "ls -lha";
      update = "sudo nixos-rebuild switch --flake ~/nixos-config#tuffy";
    };
	  envExtra = ''
    	 export NAME="Jonathan"
	  '';
    history.size = 10000;
    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "docker" "sudo" "thefuck" "dirhistory" "history" "vi-mode" ];
      theme = "agnoster";
    };  
    plugins = [
      {
        name = "powerlevel10k-config";
        src = ./p10k;
        file = "p10k.zsh";
      }
      {
        name = "zsh-powerlevel10k";
        src = "${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/";
        file = "powerlevel10k.zsh-theme";
      }
    ];
  };
  
  services.redshift = {
   enable = true;
   temperature = {
     day = 5700;  # More neutral/blue during day
     night = 2500;  # Warmer/more orange at night
   };
   settings = {
   redshift = {
     brightness-day = "1.0";
     brightness-night = "0.8";
   };
   };
   # You can set your location manually (latitude:longitude)
   # Find your coordinates at https://latlong.net/
   latitude = "43.489298";  # Replace with your latitude
   longitude = "39.976851";  # Replace with your longitude
  
   # Or use geoclue2 for automatic location
   # settings.geoclue2 = {
   #   enable = true;
   # };
  }; 
}
