with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "restagraph";

    buildInputs = [
        # General utilities
        pkgs.bash
        # Neo4j
        pkgs.neo4j
        # Lisp env
        pkgs.libyaml
        pkgs.openssl
        pkgs.sbcl
        # Python env
        pkgs.python37Full
        pkgs.python37Packages.pip
        pkgs.python37Packages.virtualenv
    ];

    env = buildEnv {
        name = name;
        paths = buildInputs;
    };

    LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [
        pkgs.openssl
        pkgs.libyaml
    ];

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix restagraph] \\w\\$\\[\\033[00m\\] '";

}
