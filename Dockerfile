FROM rust:latest
# Node 24 runs the WebAssembly host in end-to-end tests; nasm assembles AMD64 output.
COPY --from=node:24-bookworm-slim /usr/local/bin/node /usr/local/bin/node
RUN apt-get update \
    && apt-get install -y --no-install-recommends binutils nasm \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /usr/src/zydeco
COPY . .

RUN cargo build --release
RUN cargo test-all --release
RUN cargo test-doc-all --release

ENV PATH="/usr/src/zydeco/target/release:$PATH"
