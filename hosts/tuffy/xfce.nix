{ pkgs, ... }:
{
  services = {
    displayManager.defaultSession = "xfce";
    xserver = {
      enable = true;
      desktopManager = {
        xterm.enable = true;
        xfce.enable = true;
      };
    };
  };
  

  environment.systemPackages = with pkgs; [
    ghostty
  ];

  nixpkgs.config.pulseaudio = true;
}
