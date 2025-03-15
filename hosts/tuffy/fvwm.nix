{ config, lib, pkgs, inputs, ... }:
{
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.windowManager.fvwm3.enable = true;

  # nixpkgs.overlays = [
  #   (final: prev: {
  #     fvwm = prev.fvwm.overrideAttrs (oldAttrs: {
  #       configureFlags = (oldAttrs.configureFlags or []) ++ ["--disable-warnings-as-errors"];
  #       # Alternatively, if that doesn't work:
  #       # preConfigure = (oldAttrs.preConfigure or "") + ''
  #       #   export CFLAGS=''${CFLAGS//-Werror/}
  #       #   export CPPFLAGS=''${CPPFLAGS//-Werror/}
  #       # '';
  #     });
  #   })
  # ];
  
  environment.systemPackages = with pkgs; [
    fvwm3
    kitty
    xdgmenumaker
  ];
}
