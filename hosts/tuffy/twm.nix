{ config, lib, pkgs, inputs, ... }:
{
  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;

# Enable OpenBox Window Manager
  services.xserver.displayManager.defaultSession = "none+twm";
  services.xserver.windowManager.twm.enable = true;

  environment.systemPackages = with pkgs; [
    kitty
    twm
    feh         # For setting wallpapers
    # nitrogen    # Alternative for wallpaper management
    rofi        # Application launcher
    xorg.xev    # For debugging key presses
    menumaker
  ];
   
}
