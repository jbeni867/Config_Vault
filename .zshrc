# Opening tmux on terminal startup
# if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#   exec tmux
# fi

export BUN_INSTALL="$HOME/.bun"

# Setting path varibale to include additional folders
export PATH=$PATH:$HOME/.spicetify:$HOME/.dotnet/tools:$HOME/.emacs.d/bin:$HOME/bin:/Users/jordy/.local/bin:/usr/local/opt/tcl-tk/bin:$BUN_INSTALL/bin:$PATH

export DOOMDIR=$HOME/.config/doom/

# Setting default editor for terminal
export EDITOR=nvim

# Additional Aliases
alias fzf='fzf --preview "bat --style=numbers --color=always {}"'
alias nv='nvim'
alias nvfzf='nvim $(fzf --preview="bat --style=numbers --color=always {}")'

alias odin='ssh jordybenitez@odin.unomaha.edu'
alias firefoxdev='/Applications/Firefox.app/Contents/MacOS/firefox -start-debugger-server'

# Setting up pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# Added by Antigravity CLI installer
export PATH="/home/jbenitez/.local/bin:$PATH"
