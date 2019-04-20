with import <nixpkgs> {};

let
  drv = haskellPackages.callCabal2nix "hbrightness" ./. {};
in if lib.inNixShell then drv.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [ haskellPackages.ghcid cabal-install ];
}) else drv
