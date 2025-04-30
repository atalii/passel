{
  inputs.alirenix.url = "github:atalii/alirenix";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, alirenix, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages =
        let pkgs = nixpkgs.legacyPackages.${system};
        in {
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
            passel = self.packages.${system}.default;
            version = "0.1.0";
          };
        };
      devShells.default =
        let pkgs = nixpkgs.legacyPackages.${system};
	in pkgs.mkShell {
	  nativeBuildInputs = with pkgs; [
	    alire gnat14 gnat14Packages.gprbuild
	  ];
	};
    }) // {
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
