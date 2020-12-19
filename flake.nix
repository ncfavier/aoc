{
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, utils, nixpkgs }: utils.lib.simpleFlake {
    inherit self nixpkgs;
    name = "";
    shell = { pkgs }: pkgs.mkShell {
      nativeBuildInputs = [
        (pkgs.haskellPackages.ghcWithHoogle (p: with p; [
          arithmoi
          comonad
          containers
          fingertree
          kan-extensions
          lens
          linear
          megaparsec
          microlens-platform
          mtl
          profunctors
          split
          vector
        ]))
      ];
    };
  };
}
