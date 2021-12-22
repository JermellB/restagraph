export PATH="$coreutils/bin"
mkdir -p $out/bin
cp $restagraphpath $out/bin/restagraph
mkdir -p $out/templates
cp $templatepath/* $out/templates/
