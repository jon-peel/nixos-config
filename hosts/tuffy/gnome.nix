{ config, lib, pkgs, inputs, ... }:
{
  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  programs.gnomeExtensions.appindicator.enable = true;
  
  environment.gnome.excludePackages = (with pkgs; [
    atomix # puzzle game
    cheese # webcam tool
    # epiphany # web browser
    evince # document viewer
    geary # email reader
    gedit # text editor
    gnome-calendar
    gnome-characters
    gnome-music
    gnome-photos
    gnome-terminal
    gnome-tour
    gnome-weather
    hitori # sudoku game
    iagno # go game
    tali # poker game
    totem # video player
  ]);


  
}
