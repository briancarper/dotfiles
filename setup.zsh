#!/bin/zsh

autoload -U colors && colors

err()   { echo "$fg[red]$@$reset_color" }
note()  { echo "$fg[cyan]$@$reset_color" }
log()   { echo "$fg[green]$@$reset_color" }
goget() { go get "$@" 2>/dev/null || err "Failed" }
lns()   { ln -Tfs "$1" "$2" }

DOTFILES="${0:a:h}"

log "Linking..."
mkdir -p ~/.config/kitty/
# lns "$DOTFILES/zfunctions/dircolors-solarized/dircolors.ansi-light"     $HOME/.dir_colors
mkdir -p "$HOME/.vim/backup"
mkdir -p "$HOME/.vim/undo"
lns "$DOTFILES/gitconfig"               "$HOME/.gitconfig"
lns "$DOTFILES/gitignore"               "$HOME/.gitignore"
lns "$DOTFILES/ignore"                  "$HOME/.ignore"
lns "$DOTFILES/spacemacs"               "$HOME/.spacemacs"
lns "$DOTFILES/tmux.conf"               "$HOME/.tmux.conf"
lns "$DOTFILES/vim"                     "$HOME/.vim"
lns "$DOTFILES/vimrc"                   "$HOME/.vimrc"
lns "$DOTFILES/zfunctions"              "$HOME/.zfunctions"
lns "$DOTFILES/zshrc"                   "$HOME/.zshrc"
lns "$DOTFILES/config/kitty/kitty.conf" "$HOME/.config/kitty/kitty.conf"
lns "$DOTFILES/config/nvim"             "$HOME/.config/nvim"
lns "$DOTFILES/LS_COLORS/LS_COLORS"     "$HOME/.dir_colors"

tic -o ~/.terminfo xterm-256color-italic.terminfo
tic -o ~/.terminfo tmux-256color.terminfo

log "Sourcing zshrc..."
source ~/.zshrc

log "Setting up go (GOPATH=$GOPATH)..."
if [ -x "$(command -v go)" ]; then
    mkdir -p $GOPATH
    goget github.com/stamblerre/gocode
    goget github.com/rogpeppe/godef
    goget golang.org/x/tools/cmd/guru
    goget golang.org/x/tools/cmd/gorename
    goget golang.org/x/tools/cmd/goimports
fi

log "Fetching Vim plugins..."
vim +"PlugInstall | qall"
log "* coc"
vim +"CocInstall -sync coc-css coc-html coc-json \
      coc-python coc-git coc-emmet coc-yank \
      coc-emmet coc-yaml coc-vimlsp coc-snippets | qall"
log "* python"
python3 -m pip install --user --upgrade pynvim
log "* node"
npm install -g neovim

EMACS="$HOME/.emacs.d"
if [[ -e $EMACS ]]; then
    log "Updating Spacemacs..."
    git -C $EMACS pull
else
    log "Cloning Spacemacs..."
    git clone -b develop https://github.com/syl20bnr/spacemacs $EMACS
fi

if [[ `uname` == 'Darwin' ]]; then
    note "You might want to go to https://brew.sh/ and install homebrew."
    note 'Then `brew bundle`.'
fi

if [[ $SHELL != '/bin/zsh' ]]; then
    err 'Do you need to change your shell to `zsh`?'
fi

log Done.
