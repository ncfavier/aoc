{
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, utils, nixpkgs }: utils.lib.simpleFlake {
    inherit self nixpkgs;
    name = "";
    shell = { pkgs }: pkgs.mkShell {
      nativeBuildInputs = with pkgs; [
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
          monad-dijkstra
          mtl
          profunctors
          safe
          split
          unordered-containers
          vector
        ]))
        curl
        htmlq
        pandoc
        xsel
      ];
    };
  };
}
