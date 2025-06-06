{
  description = "NixOS and Home-Manager configuration";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # eyeBrowse = {
    #  url = "path:/home/me/nixos-config/programs/EyeBrowse";
    #  inputs.nixpkgs.follows = "nixpkgs"; # Share the nixpkgs input
    #};
  };

  
  outputs = { self, nixpkgs, home-manager, ... }@inputs: {
    
    nixpkgs.overlays = [ (import self.inputs.emacs-overlay) ];
    
    nixosConfigurations = {
      tuffy = nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ./hosts/tuffy
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          #  home-manager.users.me = import ./home/me;
          }
        ];
        specialArgs = { inherit inputs; };
      };
      
    };


    ## > home-manager switch --flake ~/nixos-config#me@tuffy
    homeConfigurations = {
      "me@tuffy" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        extraSpecialArgs = { inherit inputs; };
        modules = [
          ./home/me
        ];
      };
    };
  };
}
