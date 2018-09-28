export PATH="$coreutils/bin"
mkdir -p $out/bin
mkdir -p $out/schemas
cp -r $schemapath $out/schemas
cp $schemapath/*.yaml $out/schemas/
cp $restagraphpath $out/bin/restagraph
