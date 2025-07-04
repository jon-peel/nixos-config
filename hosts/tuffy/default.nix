# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, inputs, ... }:
let
  tex = import ./tex.nix { inherit pkgs; };
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix      
      #./gnome.nix
      ./sway.nix
      #./qtile.nix
      # ./xfce.nix
      # ./i3.nix
      # ./exwm.nix
      # ./mlvwm.nix
      # ./fvwm.nix
      # ./twm.nix
      # ./openbox.nix
      # ./windowmaker.nix
      # ./kde.nix
    ];

  security.polkit.enable = true;
  
  
  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  boot.supportedFilesystems = [ "ntfs" ];
  boot.loader.efi.canTouchEfiVariables = false;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.theme = pkgs.minimal-grub-theme;
  boot = {
    plymouth = {
      enable = true;
      theme = "breeze";
      # Available themes: bgrt, breeze, charge, fade-in, glow, script, solar, spinfinity, spinner
    };
    kernelModules = ["snd_hda_intel"];
    # Make sure kernel supports splash
    kernelParams = [ "quiet" "splash" "rd.systemd.show_status=false" "rd.udev.log_level=3" "vt.global_cursor_default=0"
                       "intel_pstate=active" # For Intel CPUs
                       "pcie_aspm=force"
                       "transparent_hugepage=always"
                       "usbcore.autosuspend=1"
                       "snd-intel-dspcfg.dsp_driver=1"
                   ];
  };

  nix.settings.experimental-features = [ "nix-command" "flakes" ];  

  networking.hostName = "tuffy-wuffy"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.
  #networking.interfaces.eth0 = {
  #  dhcp = false;
  #  address = "192.168.0.1";
  #  prefixLength = 24;
  #};

  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

#  services.kmscon = {
#    enable = true;
#    fonts = [{ name = "JetBrainsMono Nerd Font"; package = pkgs.nerd-fonts.jetbrains-mono; }];
    # extraConfig = "font-name=JetBrainsMono Nerd Font font-size=14";
#    hwRender = true;  # Enable hardware rendering
    # autologinSessions = [ "tty2" "tty3" "tty4" "tty5" "tty6" ];
#    useXkbConfig = true;
#    extraOptions = "--term xterm-256color";
  #  };
 

  
  #Adjust screen brightness automatically
  services.autorandr.enable = true;
  programs.light.enable = true;

  
  xdg.portal.enable = true;
  xdg.portal.wlr.enable = true;
  services.flatpak.enable = true;
  systemd.services.flatpak-repo = {
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.flatpak ];
    script = ''
      flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    '';
  };
  
  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];



  # Configure keymap in X11
  services.xserver.xkb.layout = "us";
  services.xserver.xkbOptions = "ctrl:nocaps";

  # Battery charge and power limits
  services.power-profiles-daemon.enable = false;
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
      
      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
      
      CPU_MIN_PERF_ON_AC = 0;
      CPU_MAX_PERF_ON_AC = 100;
      CPU_MIN_PERF_ON_BAT = 0;
      CPU_MAX_PERF_ON_BAT = 60;
      
      #Optional helps save long term battery health
      START_CHARGE_THRESH_BAT0 = 40; # 40 and below it starts to charge
      STOP_CHARGE_THRESH_BAT0 = 90; # 80 and above it stops charging
    };
  };
   
  hardware.graphics.enable = true;
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;

    prime = {
      #offload = {
      #  enable = true;
      #  enableOffloadCmd = true;
      #};
      intelBusId = "PCI:0:2:0";
      nvidiaBusId = "PCI:01:0:0";
    };
  }; 

  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [ gutenprint canon-cups-ufr2 cups-filters cnijfilter2 canon-capt cups-bjnp carps-cups ];

  security.rtkit.enable = true;
  # Enable sound.
  #hardware.pulseaudio.enable = true;
  # OR
 services.pipewire = {
    enable = true;
    alsa.enable = true;
    #alsa.support32Bit = true;
    pulse.enable = true;
    #jack.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  virtualisation.containers.enable = true;
  virtualisation.libvirtd.enable = true;
  # virtualisation.spiceUSBRedirection.enable = true;
virtualisation = {
  #podman = {
  #  enable = true;
  #  # Create a `docker` alias for podman, to use it as a drop-in replacement
  #  dockerCompat = true;
  #  # Required for containers under podman-compose to be able to talk to each other.
  #  defaultNetwork.settings.dns_enabled = true;
  #};
  docker = {
    enable = true;
  };
}; 



  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.me = {
    description = "Jonathan Peel";
    isNormalUser = true;
    extraGroups = [ "wheel" "podman" "docker" "libvirtd" "video" ];
  };

home-manager = {
  # useGlobalPkgs = true;
  # useUserPackages = true;
  backupFileExtension = "backup";
  extraSpecialArgs = { inherit inputs; };
};

  services.onedrive.enable = true;
  programs.firefox.enable = true;
  programs.virt-manager.enable = true;
  nixpkgs.config.allowUnfree = true;
  # List packages installed in system profile. To search, run:
  # $ nix search wget

  environment.systemPackages = with pkgs; [
    pamixer
    pavucontrol
      icu
     font-awesome
	fsharp
     material-icons
     material-design-icons
     nerd-fonts.fira-code
     nerd-fonts.droid-sans-mono
     nerd-fonts.jetbrains-mono
     emacs-all-the-icons-fonts

    # (jetbrains.plugins.addPlugins jetbrains.rider ["github-copilot"])
    distrobox
    home-manager
    ripgrep
    libreoffice
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    vlc
    wget
    ditaa
     tex
     killall
    pulseaudio
  ];



  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
    localNetworkGameTransfers.openFirewall = true; # Open ports in the firewall for Steam Local Network Game Transfers
  };

  services.atftpd.enable = true;
  services.atftpd.root = "/srv/tftp";
    
  
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?

}

