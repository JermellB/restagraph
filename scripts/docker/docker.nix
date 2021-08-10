with import <nixpkgs> {};

let
    restagraph_deriv = stdenv.mkDerivation rec {
        name = "restagraph";
        builder = "${bash}/bin/bash";
        args = [ ./nix-builder.sh ];
        inherit coreutils openssl;
        system = builtins.currentSystem;
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
    tag = "0.7.0a15";
    created = "now";

    contents = [
      restagraph_deriv
      #bash
      #coreutils
      #file
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
