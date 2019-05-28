# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install

zstyle :compinstall filename '/home/romatthe/.zshrc'

# Powerline support
if [[ $SYSTEM == 'Linux' ]]; then
    . /usr/share/powerline/zsh/powerline.zsh
fi

# Enable compinit and allow bash-completions
autoload -Uz compinit && compinit
autoload -U +X bashcompinit && bashcompinit

if [[ ! -d $ZPLUG_HOME ]]; then
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
fi
source $ZPLUG_HOME/init.zsh

alias ll='ls -l'

exists() {
  command -v $1 > /dev/null 2>&1
}

prompt_custom_docker() {
    # Check if a Dockerfile or docker-compose file present
    [[ -f $COMPOSE_FILE || -f Dockerfile || -f docker-compose.yml ]] || return
    # Check if the docker binary is available
    exists docker || return

    # If docker daemon isn't running you'll get an error saying it can't connect
    local docker_version=$(docker version -f "{{.Server.Version}}" | awk -F- '{print $1}' 2> /dev/null)
    [[ -z $docker_version ]] && return

    if [[ -n $DOCKER_MACHINE_NAME ]]; then
        docker_version+=" via ($DOCKER_MACHINE_NAME)"
    fi

    # Print the prompt
    echo -n "🐳 $docker_version"
}

prompt_custom_golang() {
    # Extended globbing is required for the (#q) pattern in `test -n`
    setopt local_options EXTENDED_GLOB

    # Check if any Go specific files are present, or current directory is under the GOPATH
    [[ -d Godeps || -f glide.yaml || -n *.go(#qN^/) || -f Gopkg.yml || -f Gopkg.lock || ( $GOPATH && $PWD =~ $GOPATH ) ]] || return

    # Check if `go` binary is available
    exists go || return

    local go_version=$(go version | awk '{print substr($3, 3)}')

    # Print the prompt
    echo -n "🐹 $go_version"
}

prompt_custom_haskell() {
    # Extended globbing is required for the (#q) pattern in `test -n`
    setopt local_options EXTENDED_GLOB

    # The command is stack, so do not change this to haskell.
    exists stack || return

    # If there are stack files in current directory
    [[ -f stack.yaml || -n *.hs(#qN^/) ]] || return

    # Print the prompt
    echo -n "λ"
}

prompt_custom_node() {
    # Extended globbing is required for the (#q) pattern in `test -n`
    setopt local_options EXTENDED_GLOB

     # Show Node status only for JS-specific folders
    [[ -f package.json || -d node_modules || -n *.js(#qN^/) ]] || return

    # Check if NVM is available
    exists nvm || return

    # Print the prompt
    local node_version=$(nvm current 2>/dev/null)
    echo -n  "⬢ $node_version"
}

prompt_custom_rust() {
    # Extended globbing is required for the (#q) pattern in `test -n`
    setopt local_options EXTENDED_GLOB	
    
    # Check if any Rust files or a Cargo TOML are present
    [[ -f Cargo.toml || -n *.rs(#qN^/) ]] || return
    # Check if the rustc compiler is available
    exists rustc || return

    # Print the prompt
    local rust_version=$(rustc --version | grep --colour=never -oE '[[:digit:]]+\.[[:digit:]]+\.[[:digit:]]')
    echo -n "𝗥 $rust_version"
}

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(custom_prompt_start status host dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(custom_rust_version custom_golang_version custom_haskell_version custom_node_version custom_docker_version root_indicator time)
POWERLEVEL9K_HOME_ICON=''
POWERLEVEL9K_HOME_SUB_ICON=''
POWERLEVEL9K_FOLDER_ICON=''
POWERLEVEL9K_OS_ICON_BACKGROUND='236'
POWERLEVEL9K_STATUS_OK=true
POWERLEVEL9K_STATUS_CROSS=true
POWERLEVEL9K_HIDE_BRANCH_ICON=true
POWERLEVEL9K_TIME_BACKGROUND="black"
POWERLEVEL9K_TIME_FOREGROUND="white"
POWERLEVEL9K_CUSTOM_PROMPT_START="echo '\U1F608'"
POWERLEVEL9K_CUSTOM_PROMPT_START_BACKGROUND="236"
POWERLEVEL9K_CUSTOM_DOCKER_VERSION="prompt_custom_docker"
POWERLEVEL9K_CUSTOM_DOCKER_VERSION_BACKGROUND="231"
POWERLEVEL9K_CUSTOM_GOLANG_VERSION="prompt_custom_golang"
POWERLEVEL9K_CUSTOM_GOLANG_VERSION_BACKGROUND="blue"
POWERLEVEL9K_CUSTOM_HASKELL_VERSION="prompt_custom_haskell"
POWERLEVEL9K_CUSTOM_HASKELL_VERSION_BACKGROUND="green"
POWERLEVEL9K_CUSTOM_NODE_VERSION="prompt_custom_node"
POWERLEVEL9K_CUSTOM_NODE_VERSION_BACKGROUND="green"
POWERLEVEL9K_CUSTOM_RUST_VERSION="prompt_custom_rust"
POWERLEVEL9K_CUSTOM_RUST_VERSION_BACKGROUND="208"

# Plugins
zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug "zsh-users/zsh-syntax-highlighting"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "jimeh/zsh-peco-history"
zplug "lukechilds/zsh-nvm"
# Only enable Powerlevel9k when we're not inside and emacs terminal
[[ -z $EMACS ]] && zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme

# Install packages that have not been installed yet
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi

zplug load
