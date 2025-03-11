{ config, lib, pkgs, inputs, ... }:
{
  # Enable WindowMaker
  services.xserver.enable = true;
  services.xserver.windowManager.windowmaker.enable = true;
  services.xserver.displayManager.lightdm.enable = true;

  environment.systemPackages = with pkgs; [
    windowmaker
    pcmanfm
  ];
}
