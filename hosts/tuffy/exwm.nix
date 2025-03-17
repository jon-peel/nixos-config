{ config, lib, pkgs, inputs, ... }:
{
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.windowManager.exwm.enable = true;
  
  environment.systemPackages = with pkgs; [
    kitty
  ];
}
