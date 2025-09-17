{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghcWithPkgs = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
          colour
          directory
          extra
          JuicyPixels
          lucid2
          optparse-generic
          pretty-simple
          shake
          text
          unix
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghcWithPkgs
            fourmolu
            haskell-language-server
          ];
        };
      } // {
        packages = pkgs.lib.listToAttrs (map
          (name: {
            inherit name;
            value =
              let module = pkgs.lib.strings.toUpper (pkgs.lib.substring 0 1 name) + pkgs.lib.substring 1 (-1) name;
              in pkgs.stdenv.mkDerivation {
                inherit name;
                src = ./.;
                buildInputs = [ ghcWithPkgs ];
                buildPhase = ''
                  ghc -O2 -o ${name} -main-is ${module} ${module}.hs
                '';
                installPhase = ''
                  mkdir -p $out/bin
                  cp ${name} $out/bin/
                '';
              };
          }) [
          "hello"
          "mandelbrot"
        ]);
      });
}
