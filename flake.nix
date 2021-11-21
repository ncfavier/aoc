{
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, utils, nixpkgs }: utils.lib.simpleFlake {
    inherit self nixpkgs;
    name = "";
    shell = { pkgs }: pkgs.mkShell {
      nativeBuildInputs = with pkgs; [
        curl
        htmlq
        pandoc
        xsel
        (haskellPackages.ghcWithPackages (p: with p; [
          arithmoi
          array
          comonad
          containers
          fingertree
          kan-extensions
          lattices
          lens
          linear
          megaparsec
          microlens-platform
          mtl
          profunctors
          safe
          split
          unordered-containers
          vector
        ]))
      ];
    };
  };
}
