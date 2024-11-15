# Document for Developers

## Prepare Development Environment

### Install Rust with rustup

#### Install additional cargo tools.

```sh
cargo install clippy cargo-dist cargo-release
```

### Install IDE

### Clone Repository

## How to Debug


## How to bump release

```sh
cargo release 2.0.0-alpha2 --execute
```

If tagged commit is pushed to github, `cargo-dist` automatically publish binary on a github release.

