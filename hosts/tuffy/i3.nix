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
      
      # System tray applications
      networkmanagerapplet
      pasystray    # PulseAudio system tray
      
      # Terminal
      alacritty    # Fast terminal emulator
      
      # Fonts
      font-awesome # Icons for polybar
    ];
  };

  services.xserver.displayManager.defaultSession = "none+i3";
}
