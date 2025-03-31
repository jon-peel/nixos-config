{ config, pkgs, options, lib, ... }:
{
  home.packages = with pkgs; [
    davmail
  ];
  
  services.davmail = {
    enable = true;    
  };
}
