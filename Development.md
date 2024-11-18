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


## How to bump release (for the core maintainer)

Merge `dev` branch into `main` on your local repository, and execute the command like below.

```sh
cargo release 2.0.0-alpha2 --execute
```

The version should follow SemVer rule and do not require `v` prefix.

Note that this command will modify the version in the root `Cargo.toml` and make a commit and tag for them, and pushes it onto the remote.

Also it internally executes `cargo publish` to upload crates into crate.io, so make sure you have a permission to publish.

If tagged commit is pushed to github, `cargo-dist` automatically publish binary on a github release.

Do not forget re-merge commits on `main` into `dev` branch

