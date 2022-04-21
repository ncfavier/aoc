{
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, utils, nixpkgs }: utils.lib.simpleFlake {
    inherit self nixpkgs;
    name = "";
    shell = { pkgs }: pkgs.mkShell {
      packages = with pkgs; [
        (haskellPackages.ghcWithPackages (p: with p; [
          arithmoi
          array
          comonad
          composition
          containers
          fingertree
          foldl
          kan-extensions
          lattices
          lens
          linear
          megaparsec
          MemoTrie
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
