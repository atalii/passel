{
  inputs.alirenix.url = "github:atalii/alirenix";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-24.11";

  outputs = { self, alirenix, nixpkgs }:
    let
      systems = nixpkgs.lib.systems.flakeExposed;
      forAllSystems = nixpkgs.lib.genAttrs systems;
      define = f: forAllSystems (system: f nixpkgs.legacyPackages.${system});
    in {
      packages = define (pkgs: {
        default = alirenix.lib.buildAlireCrate ({
          inherit pkgs;
        }) {
          src = ./.;
          pname = "passel";
          version = "0.1.0";
          index = alirenix.packages.x86_64-linux.community-index;
          alire = alirenix.packages.x86_64-linux.alire;
          depsHash = "sha256-uljt5kgnMzzvT9rSl+vnQnNpKx0YSX9ZAVijV2puP8s=";
        };

        test-site = pkgs.callPackage self.lib.buildSite {
          src = ./res;
          passel = self.packages.${pkgs.system}.default;
          version = "0.1.0";
        };
      });

      devShells.default = define (pkgs: pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          alire gnat14 gnat14Packages.gprbuild
        ];
      });
    } // {
      lib = {
        buildSite =
          { src
          , passel
          , pname ? "passel-site"
          , version
          , stdenv
          }: stdenv.mkDerivation {
            inherit pname version src;

            buildPhase = ''
              passel .
            '';

            installPhase = ''
              cp -r /tmp/target $out
            '';

            buildInputs = [ passel ];
        };
      };
    };
}
