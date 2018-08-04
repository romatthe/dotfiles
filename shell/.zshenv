# ZPlug configuration
export ZPLUG_HOME=$HOME/.zplug

# Add the user-binaries directory (e.g. /home/romatthe/.local/bin)to $PATH
PATH="$HOME/.local/bin:$PATH"

# What OS am I using?
if [[ `uname` ==  'Linux' ]]; then
    export SYSTEM="Linux"
elif [[ `uname` == 'Darwin' ]]; then
    export SYSTEM="Darwin"
else
    export SYSTEM=""
fi

# Set up Rust environment
if [[ -d $HOME/.cargo ]]; then
    # Add Cargo's directory to the PATH
    source $HOME/.cargo/env
    export PATH="$HOME/.cargo/bin:$PATH"
    export CARGO_HOME="$HOME/.cargo/"
    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
    export DYLD_LIBRARY_PATH="$(rustc --print sysroot)/lib"
fi

# Add dotnet tools directory to the PATH
export PATH="$HOME/.dotnet/tools:$PATH"

# Setup SDKMAN
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
