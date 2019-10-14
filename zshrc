# env
export LANG=en_CA.UTF-8
export EDITOR="nvim"
export PAGER="less"
export GOPATH="$HOME/Documents/go"
export FZF_DEFAULT_COMMAND='rg --files --no-ignore-vcs --hidden'
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export fpath=("$HOME/.zfuntions", $fpath)

# zsh options
autoload -Uz colors && colors
autoload -Uz compinit && compinit

export extended_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history
setopt hist_ignore_all_dups
setopt hist_no_store
setopt hist_ignore_space
export HISTSIZE=100000
export SAVEHIST=$HISTSIZE
export HISTFILE=~/.zsh_history
setopt auto_pushd
setopt extended_glob
setopt no_beep
setopt complete_aliases
setopt no_clobber
setopt short_loops
setopt check_jobs
setopt notify
setopt auto_menu
setopt autolist
setopt list_types
setopt no_list_ambiguous
setopt no_auto_remove_slash
setopt auto_param_keys
setopt prompt_subst

#           normal  bright
#  black    0       8
#  red      1       9
#  green    2       10
#  yellow   3       11
#  blue     4       12
#  magenta  5       13
#  cyan     6       14
#  white    7       15
autoload -Uz vcs_info

zstyle ':vcs_info:*' formats '%F{8} %F{7}%b%f'
zstyle ':vcs_info:*' actionformats '%F{8} %F{7}%b|%a%f'

function prompt() {
    PCOLOR="%F{15}"
    PSYMBOLGOOD="•"
    PSYMBOLBAD="✗"
    if [[ -f "$HOME/.zsh.prompt" ]]; then
        source ~/.zsh.prompt
    fi

    local PROMPT=''
    # status of last command
    PROMPT+="%(0?.${PCOLOR}${PSYMBOLGOOD}.%B%F{1}${PSYMBOLBAD}%b)%f"
    # root
    PROMPT+="%(!.%F{1} root%f.)"
    # hostname
    PROMPT+=" ${PCOLOR}%m%f "
    # pwd
    PROMPT+="%F{12}%~%f"
    # git
    PROMPT+="${vcs_info_msg_0_}"
    if [[ -n "$(git status --porcelain 2>/dev/null)" ]]; then
        PROMPT+="%F{1}*%f"
    fi
    # end
    PROMPT+=" %F{13}❯%f "

    echo "$PROMPT"
}

precmd () { vcs_info }
export PS1='$(prompt)'

# keybinds
bindkey -e
bindkey '\e[H' beginning-of-line
bindkey '\e[F' end-of-line
bindkey '\e[5~' history-beginning-search-backward
bindkey '\e[6~' history-beginning-search-forward
bindkey '\e[1;3D' emacs-backward-word
bindkey '\e[1;3C' emacs-forward-word
bindkey '\e[3~' delete-char
bindkey '\e[3;3~' delete-word
bindkey -s '\e[' '\\\C-v['
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line

export WORDCHARS="${WORDCHARS/\/}"

if [[ `uname` == 'Darwin' ]]; then
    local gnubin="/usr/local/opt/coreutils/libexec/gnubin" 
    local findbin="/usr/local/opt/findutils/libexec/gnubin" 
    if [[ -d $gnubin ]]; then
        export PATH="${gnubin}:$PATH"
    fi
    if [[ -d $findbin ]]; then
        export PATH="${findbin}:$PATH"
    fi
    export PATH="/usr/local/bin:$PATH"
fi


if [[ `ls --color=auto 2>/dev/null` ]]; then
    if [[ ! -z "${WSL_DISTRO_NAME}" ]]; then
        alias ls="LC_COLLATE=POSIX ls --group-directories-first --color=auto 2>/dev/null"
    else
        alias ls="LC_COLLATE=POSIX ls --group-directories-first --color=auto"
    fi

    alias ll="ls -lh"
    alias la="la -a"
fi

if type fdfind &>/dev/null && ! type fd &>/dev/null; then
    alias fd=fdfind
fi

if [[ $(command dircolors) && -f "$HOME/.dir_colors" ]]; then
    eval $(dircolors "$HOME/.dir_colors")
fi

# must go at end of file
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

# auto-generated stuff

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if type pyenv > /dev/null; then
    eval "$(pyenv init -)"
fi

if type jenv > /dev/null; then
    eval "$(jenv init -)"
fi
