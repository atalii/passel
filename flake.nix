{
  inputs.alirenix.url = "github:atalii/alirenix";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-24.05";

  outputs = { self, alirenix, nixpkgs }: {
    packages.x86_64-linux =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in {
        default = alirenix.lib.buildAlireCrate ({
          inherit pkgs;
        }) {
          src = ./.;
          pname = "passel";
          version = "0.1.0";
          index = alirenix.packages.x86_64-linux.community-index;
          alire = alirenix.packages.x86_64-linux.alire;
          depsHash = "sha256-OW/RDzX+76poy6K9LJqcSoTHaDjAbvuBw6K0KCR0BtQ=";
        };

        test-site = pkgs.callPackage self.lib.buildSite {
          src = ./res;
          passel = self.packages.x86_64-linux.default;
          version = "0.1.0";
        };
      };

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
