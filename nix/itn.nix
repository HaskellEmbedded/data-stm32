{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "417bfe890364ceb8d320346df3804375dff8f669";
    sha256 = "11ig2i7b1arzvm9hvcy15gfyw44bqm0yr501qasfyqkh4sz6v5mq";
  };
in
  import itnSrc { inherit compiler; }
