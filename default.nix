{ pkgs ? import <nixpkgs> {} }:

let
  ocamlPackages_ber = pkgs.ocamlPackages.overrideScope' (self: super: {
    ocaml = pkgs.ber_metaocaml;
  });
  batteries_master =
    pkgs.stdenv.mkDerivation {
      name = "batteries-master";
      src = pkgs.fetchFromGitHub {
        owner = "ocaml-batteries-team";
        repo = "batteries-included";
        rev = "43068b3d3eea4354f8bcef3d7a042e7138e15edc";
        sha256 = "1a868zmkdncqyq18a9x9cwha6w57hwbhwbh8i73w31a3p7f9zznm";
      };
      buildInputs = with pkgs; [
        ber_metaocaml
        ocamlPackages_ber.findlib
        ocamlPackages_ber.ocamlbuild
        ocamlPackages_ber.qtest
      ];
      propagatedBuildInputs = [ ocamlPackages_ber.num ];
      createFindlibDestdir = true;
      meta = {
        description = "OCaml Batteries Included (master branch)";
      };
    };
  in

pkgs.mkShell {
  inputsFrom = [ pkgs.ber_metaocaml ];
  buildInputs = with pkgs; [
    ber_metaocaml
    batteries_master
    ocamlPackages_ber.findlib
    ocamlPackages_ber.stdint
    ocamlPackages_ber.qtest
  ];
  # I like to know this to customize prompts etc:
  shellHook = ''
    export NIX_SHELL=1
  '';
}
