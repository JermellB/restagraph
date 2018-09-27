export PATH="$coreutils/bin"
mkdir -p $out/bin
cp -r $schemapath $out/schemas
cp $restagraphpath $out/bin/restagraph
