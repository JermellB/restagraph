with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "restagraph";

    buildInputs = [
        # General utilities
        pkgs.bash
        # Neo4j
        pkgs.neo4j
        # Lisp env
        pkgs.gcc_multi
        pkgs.gcc
        pkgs.libyaml
        pkgs.openssl
        pkgs.sbcl
        # Python env
        pkgs.python36Packages.requests
        pkgs.python36Packages.pylint
        pkgs.python3
    ];

    env = buildEnv {
        name = name;
        paths = buildInputs;
    };

    LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
        pkgs.openssl
        pkgs.libyaml
    ];

}
