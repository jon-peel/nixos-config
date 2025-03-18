{ config, lib, pkgs, inputs, ... }:
{
  # https://nixos.wiki/wiki/KDE
  
  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.desktopManager.plasma6.enable = true;

 # environment.systemPackages = with pkgs; [
    # KDE applications
 #   kate
 #   konsole
 #   dolphin
 #   ark
 #   okular
 #   # ... add other KDE applications you want
 # ];

  # Optional: Enable Wayland support for Plasma
  # services.xserver.displayManager.defaultSession = "plasmawayland";
  
  #   qt5.platformTheme = "qt5ct";
}
