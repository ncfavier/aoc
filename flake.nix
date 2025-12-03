{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
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
          data-clist
          data-interval
          extra
          fingertree
          foldl
          integer-logarithms
          kan-extensions
          lattices
          lens
          linear
          megaparsec
          MemoTrie
          modular-arithmetic
          monad-dijkstra
          mtl
          profunctors
          regex-tdfa
          safe
          split
          unordered-containers
          vector
        ]))
        haskell-language-server
        curl
        htmlq
        pandoc
        xsel
      ];
    };
  };
}
