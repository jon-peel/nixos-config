{ config, lib, pkgs, inputs, ... }:
{
  # Enable the KDE Plasma Desktop Environment
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

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
  services.xserver.displayManager.defaultSession = "plasmawayland";
  
  qt5.platformTheme = "qt5ct";
}
