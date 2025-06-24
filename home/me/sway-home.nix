{ config, lib, pkgs, ... }:

let theme-toggle = pkgs.writeShellScriptBin "theme-toggle" (builtins.readFile ./theme-toggle.sh);

in
{
  services = {
    gnome-keyring.enable = true;
    mako.enable = true;
    udiskie = {
      enable = true;
      automount = true;
      notify = true;
      tray = "never";
    };
  };

  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # Fixes common issues with GTK 3 apps

    config = rec {
      modifier = "Mod4";
      # Use kitty as default terminal
      terminal = "kitty";

      bars = [{
        position = "top";
        statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-default.toml";
        fonts = {
          names = [ "FiraCode Nerd Font" "Font Awesome 6 Free" ];
          size = 11.0;
        };
      }];

      gaps = { inner = 10; outer = 5; smartGaps = true; };

      window = { titlebar = false; border = 2; hideEdgeBorders = "smart"; };

      startup = [
        #{command = "thunar --daemon";}
        {command = "autotiling -sr 1.618"; always=true;}
        {command = "sleep 5; systemctl --user start kanshi.service";}
      ];

      keybindings = let
        modifier = config.wayland.windowManager.sway.config.modifier;
        in lib.mkOptionDefault {
          "${modifier}+d" = "exec wofi --show drun";

          # Brightness
          "XF86MonBrightnessDown" = "exec light -U 10";
          "XF86MonBrightnessUp" = "exec light -A 10";

          # Volume
          "XF86AudioRaiseVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ +1%";
          "XF86AudioLowerVolume" = "exec pactl set-sink-volume @DEFAULT_SINK@ -1%";
          "XF86AudioMute" = "exec pactl set-sink-mute @DEFAULT_SINK@ toggle";

          # Full screen screenshot
          "Print" = "exec grim ~/Pictures/screenshot-$(date +%Y%m%d-%H%M%S).png";

          # Select area screenshot
          "${modifier}+Print" = "exec grim -g \"$(slurp)\" ~/Pictures/screenshot-$(date +%Y%m%d-%H%M%S).png";

          # Screenshot to clipboard
          "${modifier}+Shift+Print" = "exec grim -g \"$(slurp)\" - | wl-copy";

          # Full screen to clipboard
          "Shift+Print" = "exec grim - | wl-copy";
      };

      colors = {
        focused = {
          border = "#4c7899";
          background = "#285577";
          text = "#ffffff";
          indicator = "#2e9ef4";
          childBorder = "#285577";
        };
      };

      output."*".bg = "#1e1e2e solid_color";
    };
  };

programs.i3status-rust = {
    enable = true;
    bars = {
      default = {
        icons = "awesome6";  # or "none" to disable icons
        theme = "solarized-dark";  # optional theme
        blocks = [
          {
            block = "net";
            format = " $icon "; #$ssid $signal_strength ";
            interval = 5;
          }
          {
            block = "sound";
          }
          {
            block = "battery";
            format = " $icon $percentage ";
          }
          {
            block = "time";
            format = " $icon $timestamp.datetime(f:'%a %d - %H:%M') ";
          }
        ];
      };
    };
  };

  home.packages = with pkgs; [
    theme-toggle
    light
    pulseaudio
    grim # screenshot functionality
    slurp # screenshot functionality
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    libnotify
    wofi
    autotiling
    xfce.thunar
    #xfce.thunar-volman
    gnome-disk-utility

    nerd-fonts._0xproto
    nerd-fonts.droid-sans-mono
    nerd-fonts.fira-code
    unrar
    unzip
  ];

}
