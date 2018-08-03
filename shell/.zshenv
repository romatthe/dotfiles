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

if [[ -d $HOME/.cargo ]]; then
    # Add Cargo's directory to the PATH
    export PATH="$HOME/.cargo/bin:$PATH"
    source $HOME/.cargo/env

    # Set the right RUST_SRC directory or Linux or Mac
    if [[ $SYSTEM == 'Linux' ]]; then
	export RUST_SRC_PATH=${HOME}/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src
    elif [[ $SYSTEM == 'Darwin' ]]; then
	export RUST_SRC_PATH=${HOME}/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src
    fi
fi

# Add dotnet tools directory to the PATH
export PATH="$HOME/.dotnet/tools:$PATH"

# Setup SDKMAN
export SDKMAN_DIR="/Users/romatthe/.sdkman"
[[ -s "/Users/romatthe/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/romatthe/.sdkman/bin/sdkman-init.sh"
