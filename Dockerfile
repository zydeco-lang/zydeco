FROM rust:latest

WORKDIR /usr/src/app
COPY . .

RUN cargo build --release
RUN cargo test --release
