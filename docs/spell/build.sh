#! /usr/bin/env zsh
zydeco_bin=$(cargo build --release --bin=zydeco --message-format=json |
  jq -r 'select(.reason=="compiler-artifact" and .executable!=null) | .executable')

for source in [0-8]-*.zy; do
    echo "Building $source"
    chapter=${source%.zy}
    "$zydeco_bin" run ./.build.zy < "$source" > "$chapter.md"
done
