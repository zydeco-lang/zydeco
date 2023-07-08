#! /usr/bin/env zsh
for f in **/*.zy; do
    echo "Building $f"
    cargo run --bin zydeco -- run .build.zy -- < ${f} > ${f}.md
done
