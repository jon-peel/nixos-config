{
  description = "EyeBrowse - A flake for my legacy binary";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # You may need to adjust these dependencies based on what your binary requires
        runtimeDeps = with pkgs; [
          # Add runtime dependencies here, for example:
          # glibc
          # zlib
          # openssl
          # etc.
        ];
      in
      {
        packages = {
          default = self.packages.${system}.EyeBrowse;
          
          EyeBrowse = pkgs.stdenv.mkDerivation {
            pname = "EyeBrowse";
            version = "1.0.0";  # Adjust as needed
            
            # Source is just your binary file
            src = ./EyeBrowse; # Assumes the binary is in the same directory as flake.nix
            
            # If you have the binary elsewhere, you can use:
            # src = /path/to/directory/containing/binary;
            
            # Skip phases that don't apply to pre-built binaries
            dontBuild = true;
            dontConfigure = true;
            
            # The binary needs to be executable
            installPhase = ''
              mkdir -p $out/bin
              cp ./EyeBrowse $out/bin/EyeBrowse
              chmod +x $out/bin/Eyebrowse
            '';
            
            # Add runtime dependencies to the binary
            runtimeDependencies = runtimeDeps;
            
            # For dynamically linked binaries, ensure libraries are found
            preFixup = ''
              patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $out/bin/Eyebrowse
              
              # If the binary needs specific shared libraries, you can add them:
              # patchelf --set-rpath "${pkgs.lib.makeLibraryPath runtimeDeps}" $out/bin/Eyebrowse
            '';
            
            meta = {
              description = "Eyebrowse - My legacy binary package";
              license = pkgs.lib.licenses.unfree; # Adjust as appropriate
            };
          };
        };
        
        # Add app so it can be run with `nix run`
        apps = {
          default = self.apps.${system}.EyeBrowse;
          
          EyeBrowse = {
            type = "app";
            program = "${self.packages.${system}.EyeBrowse}/bin/Eyebrowse";
          };
        };
      }
    );
}
