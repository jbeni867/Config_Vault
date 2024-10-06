if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  exec tmux
fi

export PATH=$PATH:/Users/jordy/.spicetify:/Users/jordy/.dotnet/tools

export EDITOR=nvim

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

alias fzf='fzf --preview "bat --style=numbers --color=always {}"'
alias nv='nvim'
alias nvfzf='nvim $(fzf --preview="bat --style=numbers --color=always {}")'
