{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ((pkgs.haskellPackages.override {
              overrides = self: super: { };
            }).ghcWithPackages (hpkgs: with hpkgs; [
              colour
              directory
              extra
              JuicyPixels
              lucid2
              pretty-simple
              shake
              text
              unix
            ]))
            fourmolu
            haskell-language-server
          ];
        };
      });
}
