# Add the user-binaries directory (e.g. /home/romatthe/.local/bin)to $PATH
PATH="$(systemd-path user-binaries):$PATH"

# ZPlug configuration
export ZPLUG_HOME=$HOME/.zplug

# Add Cargo's bin directory to the PATH
export PATH="$HOME/.cargo/bin:$PATH"
