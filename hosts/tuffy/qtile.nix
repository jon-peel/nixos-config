{ pkgs, environment, ... }:
{
  environment.systemPackages = with pkgs; [
    # other packages ...
    qtile
    dmenu
  ];

  services.xserver = {
    enable = true;
    displayManager.defaultSession = "none+qtile";
    windowManager.qtile.enable = true;
  };
}
