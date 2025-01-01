{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];

      perSystem = { self', pkgs, lib, config, ... }:
        let
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./bin
              ./src
              ./specs
              ./aether.cabal
            ];
          });
          otherFiles = [ ];
          configurationFlags = [ "--ghc-options=-O2" ];
          buildInputs = with pkgs; [
            pkg-config
          ];

          devPackages = with pkgs; [
            just
            nodemon
          ];
        in {
          haskellProjects.default = {
            inherit projectRoot;

            packages = {};
            settings = {
              aether = {
                check = true;
                deadCodeElimination = true;
                staticLibraries = true;
                strip = true;
                custom = drv:
                  (pkgs.haskell.lib.compose.appendConfigureFlags configurationFlags drv).overrideAttrs (old: {
                    preBuild = ''
                      ${toString (map (f: ''cp -r ${f.source} ${f.target};'') otherFiles)}
                    '';
                  })
                ;
              };
            };

            devShell = {
              hlsCheck.enable = true;
            };

            autoWire = [ "packages" "apps" "checks" ];
          };

          packages.default = self'.packages.aether;
          apps.default = self'.apps.aether;

          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            packages = devPackages;
            inherit buildInputs;

            LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";
          };
        };
    };
}
