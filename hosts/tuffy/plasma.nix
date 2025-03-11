{ config, lib, pkgs, inputs, ... }:
{
  # Enable the KDE Plasma Desktop Environment
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  

  # Optional: Enable Wayland support for Plasma
  services.xserver.displayManager.defaultSession = "plasmawayland";
  
  # Optional: Add Qt theme integration for non-KDE applications
  programs.qt5ct.enable = true;
}
