{ config, lib, pkgs, inputs, ... }:
{
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.windowManager.mlvwm.enable = true;
  
  environment.systemPackages = with pkgs; [
    mlvwm
    kitty
    xdgmenumaker
  ];
}
