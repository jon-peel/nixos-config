{ config, lib, pkgs, inputs, ... }:
{
  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.windowManager.fvwm.enable = true;

  environment.systemPackages = with pkgs; [
    fvwm
    kitty
  ];
   
}
