{ config, lib, pkgs, inputs, ... }:
{
  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;

# Enable OpenBox Window Manager
  services.xserver.displayManager.defaultSession = "none+openbox";
  services.xserver.windowManager.openbox.enable = true;

  environment.systemPackages = with pkgs; [
    kitty
    openbox
    obconf      # OpenBox configuration tool
    feh         # For setting wallpapers
    # nitrogen    # Alternative for wallpaper management
    rofi        # Application launcher
    xorg.xev    # For debugging key presses
  ];
   
}
