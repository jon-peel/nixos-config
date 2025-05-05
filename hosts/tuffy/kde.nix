{ config, lib, pkgs, inputs, ... }:
{
  # https://nixos.wiki/wiki/KDE
  services = {
    xserver = {
      enable = true;
      displayManager.defaultSession = "plasma";
    };
    
    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
    };
    
    desktopManager.plasma6.enable = true;
  };

  environment.systemPackages = with pkgs; [
    kdePackages.konqueror
  ];
}
