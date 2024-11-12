# Opening tmux on terminal startup
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi

# Setting path varibale to include additional folders
export PATH=$PATH:$HOME/.spicetify:$HOME/.dotnet/tools:$HOME/.emacs.d/bin:$HOME/bin:/Users/jordy/.local/bin
export DOOMDIR=$HOME/.config/doom/

# Setting default editor for terminal
export EDITOR=nvim

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Additional Aliases
alias fzf='fzf --preview "bat --style=numbers --color=always {}"'
alias nv='nvim'
alias nvfzf='nvim $(fzf --preview="bat --style=numbers --color=always {}")'
