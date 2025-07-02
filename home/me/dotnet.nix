{ config, lib, pkgs, ... }:
with pkgs;
let
  dotnet-combined = (with pkgs.dotnetCorePackages; combinePackages [ dotnet_10.sdk dotnet_9.sdk dotnet_8.sdk ]);

in
{
  sessionVariables = { DOTNET_ROOT = "${dotnet-combined}"; };
  home.packages = with pkgs [ dotnet-combined ];
}
