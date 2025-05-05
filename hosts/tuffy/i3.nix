{ pkgs, ... }:

{

  services.xserver.enable = true;
  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    extraPackages = with pkgs; [
      # Status bar
      polybar
      
      # Utilities
      i3lock       # Screen locker
      i3status     # Status bar
      dmenu        # Application launcher
      rofi         # Alternative application launcher
      feh          # For setting wallpapers
      dunst        # Notification daemon
      picom        # Compositor for transparency
      brightnessctl  # For controlling screen brightness
      pamixer        # For controlling audio (alternative to pactl)
      playerctl      # For media player controls
      
      # System tray applications
      networkmanagerapplet
      pasystray    # PulseAudio system tray
      cbatticon
      blueman
      clipit
      redshift
      
      # Terminal
      alacritty    # Fast terminal emulator
      
      # Fonts
      font-awesome # Icons for polybar

      # Themes
      glib
      adwaita-icon-theme
      gtk-engine-murrine
      gtk_engines
      gsettings-desktop-schemas
      dconf
      dconf-editor
    ];
  };

  services.xserver.displayManager.defaultSession = "none+i3";

  services.xserver.libinput = {
    enable = true;
    touchpad = {
      naturalScrolling = true;  # Reverse scrolling for trackpad
      tapping = true;           # Enable tap to click
      disableWhileTyping = true;
      scrollMethod = "twofinger";
    };
    # Mouse configuration - normal scrolling
    mouse = {
      naturalScrolling = false;  # Normal scrolling for mouse
    };
  };

  # Enable sound with pipewire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  services.actkbd = {
    enable = true;
  };

  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ dconf ];

  services.upower.enable = true;
  services.gvfs.enable = true;
  
  # Ensure users in the "video" group can change brightness
  # users.users.me = {
  #  extraGroups = [ "video" ];
  # };
}
