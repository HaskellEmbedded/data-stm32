{ compiler ? "default", ... }:

let
  itn = import ./nix/itn.nix { inherit compiler; };
  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  itn.pkgs.haskell.lib.dontCheck
    (itn.ivorypkgs.callCabal2nix "data-stm32" src {})
