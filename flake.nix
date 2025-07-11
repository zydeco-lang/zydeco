{
  description = "Rust 2024 project with pinned toolchain";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    # Fenix gives pre-built official toolchains
    fenix.url = "github:nix-community/fenix";
  };

  outputs = { self, nixpkgs, fenix, ... }:
    let
      system = "x86_64-linux";
      pkgs   = import nixpkgs { inherit system; };
      rust   = fenix.packages.${system}.stable.toolchain;   # Rust 1.85 (Edition 2024)
    in {
      packages.${system}.zydeco =
        pkgs.rustPlatform.buildRustPackage {
          pname = "zydeco";
          version = "0.2.2";
          src = ./.;
          cargoLock.lockFile = ./Cargo.lock;
        };

      devShells.${system}.default = pkgs.mkShell {
        packages = [
          rust
          pkgs.cargo-nextest
          pkgs.rust-analyzer
          pkgs.clippy
          pkgs.pkg-config pkgs.openssl
        ];
        RUST_BACKTRACE = "1";
      };

      apps.${system}.zydeco = {
        type = "app";
        program = "${self.packages.${system}.zydeco}/bin/zydeco";
      };
    };
}
