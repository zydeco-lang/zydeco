#! /usr/bin/env zsh
ZYDECO=`cargo build --release --bin=zydeco --message-format=json | jq -r 'select(.reason=="compiler-artifact" and .executable!=null) | .executable'`

for f in **/*.zy; do
    echo "Building $f"
    # remove .zy extension in the filename
    b=$(basename $f .zy)
    $ZYDECO run ./proj.toml --bin=.build -- < ${b}.zy > ${b}.md
done
