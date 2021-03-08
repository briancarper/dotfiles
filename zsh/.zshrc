#           normal  bright
#  black    0       8
#  red      1       9
#  green    2       10
#  yellow   3       11
#  blue     4       12
#  magenta  5       13
#  cyan     6       14
#  white    7       15

# macos brew-installed coreutils
if [[ `uname` == 'Darwin' ]]; then
    local gnubin="/usr/local/opt/coreutils/libexec/gnubin" 
    [[ -d $gnubin  ]] && export PATH="${gnubin}:$PATH"

    export PATH="/usr/local/bin:$PATH"
fi

# clone p10k into cache dir so it doesn't end up in dotfiles repo
P10K_DIR="$XDG_CACHE_HOME/powerlevel10k"
if [[ ! -d "$P10K_DIR" ]]; then
      git clone --depth=1 https://github.com/romkatv/powerlevel10k.git "$P10K_DIR"
fi
source "$P10K_DIR/powerlevel10k.zsh-theme"

autoload -Uz colors && colors
autoload -Uz compinit && compinit

ZCOMP_DIR="$XDG_CACHE_HOME/zsh/"
compinit -d "$ZCOMP_DIR/zcompdump-$ZSH_VERSION"

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

typeset -aU path

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
# source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

# auto-generated stuff

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# if type nvim > /dev/null; then
#     alias vim=nvim
# fi


if [ -d "$HOME/.pyenv" ]; then
    export PATH="$HOME/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
    if [ -f "$(pyenv root)/completions/pyenv.zsh" ]; then
        source "$(pyenv root)/completions/pyenv.zsh"
    fi
fi

# if type jenv > /dev/null; then
#     eval "$(jenv init -)"
# fi

# if type rbenv > /dev/null; then
#     eval "$(rbenv init -)"
# fi

alias vim=nvim

export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git --exclude '*~'"

[[ -f "$XDG_CONFIG_HOME/.aliases" ]] && source "$XDG_CONFIG_HOME/.aliases"

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ -f ~/.config/zsh/.p10k.zsh ]] && source ~/.config/zsh/.p10k.zsh
