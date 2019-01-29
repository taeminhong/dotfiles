
# Cargo is a package manager for Rust
addpath ~/.cargo/bin

if command -v yarn >/dev/null 2>&1; then
    addpath "`yarn global bin`"
fi
