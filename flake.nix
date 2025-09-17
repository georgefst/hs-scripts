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
        mandelbrot = pkgs.stdenv.mkDerivation {
          name = "mandelbrot";
          src = ./.;
          buildInputs = [ ghcWithPkgs ];
          buildPhase = ''
            ghc -O2 -o mandelbrot -main-is Mandelbrot Mandelbrot.hs
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp mandelbrot $out/bin/
          '';
        };
      in
      {
        packages.default = mandelbrot;
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghcWithPkgs
            fourmolu
            haskell-language-server
          ];
        };
      });
}
