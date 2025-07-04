{ config, lib, pkgs, ... }:
with pkgs;
let
  dotnet-combined =
    (with pkgs.dotnetCorePackages;
      combinePackages [
        sdk_10_0-bin
        sdk_9_0
        sdk_8_0
      ]);

in
{
  home = {
    packages = with pkgs; [
      jetbrains.rider
      docker
      dotnet-combined
    ];
  };
 }
