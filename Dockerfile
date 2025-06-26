FROM rust:latest
RUN apt update && apt install -y jq

WORKDIR /usr/src/zydeco
COPY . .

RUN cargo build --release
RUN cargo test --release

ENV PATH="/usr/src/zydeco/target/release:$PATH"
