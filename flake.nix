{
  inputs.alirenix.url = "github:atalii/alirenix";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-25.05";

  outputs =
    {
      self,
      alirenix,
      nixpkgs,
    }:
    let
      systems = nixpkgs.lib.systems.flakeExposed;
      forAllSystems = nixpkgs.lib.genAttrs systems;
      define = f: forAllSystems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      packages = define (pkgs: {
        default =
          alirenix.lib.buildAlireCrate
            ({
              inherit pkgs;
            })
            {
              src = ./.;
              pname = "passel";
              version = "0.1.0";
              index = alirenix.packages.x86_64-linux.community-index;
              alire = alirenix.packages.x86_64-linux.alire;
              depsHash = "sha256-GFUrwFQTINHNd/wBRREMOwRr//XzBhACQrXyXeBl5lE=";
            };
      });

      devShells.default = define (
        pkgs:
        pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            alire
            gnat14
            gnat14Packages.gprbuild
          ];
        }
      );
    }
    // {
      lib = {
        buildSite =
          {
            src,
            passel,
            pname ? "passel-site",
            version,
            stdenv,
          }:
          stdenv.mkDerivation {
            inherit pname version src;

            buildPhase = ''
              passel $src -d $out
            '';

            buildInputs = [ passel ];
          };
      };
    };
}
