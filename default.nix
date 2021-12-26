with import <nixpkgs> {};

stdenv.mkDerivation rec {
    name = "restagraph";

    buildInputs = [
        # General utilities
        pkgs.bash
        # Lisp env
        #pkgs.libyaml
        pkgs.openssl
        pkgs.sbcl_2_1_10
        # Python env
        pkgs.python39Full
        pkgs.python39Packages.pip
        pkgs.python39Packages.virtualenv
    ];

    env = buildEnv {
        name = name;
        paths = buildInputs;
    };

    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
        pkgs.openssl
        #pkgs.libyaml
    ];

    shellHook = "export PS1='\n\\[\\033[01;32m\\][nix restagraph] \\w\\$\\[\\033[00m\\] ';\
                 export PYTHONPATH=$PWD/test/venv/lib/python3.7/site-packages/:$PYTHONPATH;\
                 unset SOURCE_DATE_EPOCH";

}
