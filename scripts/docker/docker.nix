with import <nixpkgs> {};

let
    restagraph_deriv = stdenv.mkDerivation rec {
        name = "restagraph";
        builder = "${bash}/bin/bash";
        args = [ ./nix-builder.sh ];
        inherit coreutils openssl;
        system = builtins.currentSystem;
        templatepath = ../../src/templates;
        restagraphpath = ./restagraph;
    };

    ld_path = pkgs.lib.makeLibraryPath [
        pkgs.openssl
    ];

    entrypoint = writeScript "entrypoint.sh" ''
    #!${stdenv.shell}
    export LD_LIBRARY_PATH=${ld_path}
    exec $@
    '';

in
pkgs.dockerTools.buildImage {
    name = "equill/restagraph";
    tag = "0.8.4b1";
    created = "now";

    contents = [
      # Required for normal operation
      restagraph_deriv
      file
      coreutils
      #
      # Optional, for diagnostics
      #bash
      #which
    ];

    config = {
        Cmd = [ "restagraph" ];
        Entrypoint = [ entrypoint ];
        ExposedPorts = {
            "4949/tcp" = {};
        };
        WorkingDir = "/opt";
    };
}
