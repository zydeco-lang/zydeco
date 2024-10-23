#! /usr/bin/env zsh
for f in **/*.zy; do
    echo "Building $f"
    cargo run --bin zydeco -- run ./proj.toml --bin=.build -- < ${f} > ${f}.md
done
