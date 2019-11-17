{ stdenv, fetchFromGitHub, ocaml, findlib, batteries, stdint }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-dessser";
  version = "0.0.5";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "dessser";
    rev = "v${version}";
    sha256 = "1wxnw0jlpiax0407s7c5nln3l9npvf19i6z7rhgg3q8g9s0w7hhl";
  };

  buildInputs = [
    ocaml findlib batteries stdint
  ];

  createFindlibDestdir = true;

  meta = with stdenv.lib; {
    homepage = https://github.com/rixed/dessser;
    description = "(de)serializer generator";
    platforms = ocaml.meta.platforms or [];
    maintainers = [ maintainers.rixed ];
  };
}
