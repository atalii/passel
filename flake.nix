{
  inputs.alirenix.url = "github:atalii/alirenix";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-24.05";

  outputs = { self, alirenix, nixpkgs }: {
    packages.x86_64-linux.default =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux;
      in (alirenix.lib.buildAlireCrate {
        inherit pkgs;
      }) {
        src = ./.;
        pname = "passel";
        version = "0.1.0";
        index = alirenix.packages.x86_64-linux.community-index;
        alire = alirenix.packages.x86_64-linux.alire;
        depsHash = "sha256-OW/RDzX+76poy6K9LJqcSoTHaDjAbvuBw6K0KCR0BtQ=";
      };
  };
}
