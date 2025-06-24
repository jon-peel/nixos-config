{ pkgs, ... }:

{
  # kanshi systemd service
  systemd.user.services.kanshi = {
    description = "kanshi daemon";
    environment = {
      WAYLAND_DISPLAY="wayland-1";
      DISPLAY = ":0";
    };
    serviceConfig = {
      Type = "simple";
      ExecStart = ''${pkgs.kanshi}/bin/kanshi -c kanshi_config_file'';
    };
  };

  services.udisks2.enable = true;
  services.gvfs.enable = true;
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = ''
          ${pkgs.greetd.tuigreet}/bin/tuigreet \
            --time \
            --asterisks \
            --remember \
            --theme "border=blue;text=cyan;prompt=green" \
            --greeting "Welcome to NixOS" \
            --cmd "sway --unsupported-gpu" 
	'';
        user = "greeter";
      };
    };
  };
}
