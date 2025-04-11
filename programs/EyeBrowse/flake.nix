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
        runtimeDeps = with pkgs; [
          glibc
          zlib
          stdenv.cc.cc
        ];
      in
      {
        packages = {
          default = self.packages.${system}.EyeBrowse;
          
          EyeBrowse = pkgs.stdenv.mkDerivation {
            pname = "EyeBrowse";
            version = "1.0.3";  # Adjust as needed
            
            dontUnpack = true;
            dontBuild = true;
            dontConfigure = true;

            # Use nativeBuildInputs for build-time dependencies
            nativeBuildInputs = [ pkgs.autoPatchelfHook ];

            # Use buildInputs for run-time dependencies
            buildInputs = runtimeDeps;
            
            # The binary needs to be executable
            installPhase = ''
              mkdir -p $out/bin
              cp ${./EyeBrowse} $out/bin/EyeBrowse
              chmod +x $out/bin/EyeBrowse
            '';
            
            # For dynamically linked binaries, ensure libraries are found
            #preFixup = ''
            #  patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $out/bin/EyeBrowse
            #  
            #  # If the binary needs specific shared libraries, you can add them:
            #  # patchelf --set-rpath "${pkgs.lib.makeLibraryPath runtimeDeps}" $out/bin/EyeBrowse
            #'';
            
            meta = {
              description = "EyeBrowse - My legacy binary package";
              license = pkgs.lib.licenses.mit; # Adjust as appropriate
            };
          };
        };
        
        # Add app so it can be run with `nix run`
        apps = {
          default = self.apps.${system}.EyeBrowse;
          
          EyeBrowse = {
            type = "app";
            program = "${self.packages.${system}.EyeBrowse}/bin/EyeBrowse";
          };
        };
      }
    );
}
