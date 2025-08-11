# Opening tmux on terminal startup
# if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#   exec tmux
# fi

# Setting path varibale to include additional folders
export PATH=$PATH:$HOME/.spicetify:$HOME/.dotnet/tools:$HOME/.emacs.d/bin:$HOME/bin:/Users/jordy/.local/bin:/usr/local/opt/tcl-tk/bin
export DOOMDIR=$HOME/.config/doom/

# Setting default editor for terminal
export EDITOR=nvim

# Additional Aliases
alias fzf='fzf --preview "bat --style=numbers --color=always {}"'
alias nv='nvim'
alias nvfzf='nvim $(fzf --preview="bat --style=numbers --color=always {}")'

alias odin='ssh jordybenitez@odin.unomaha.edu'

# Setting up pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

function gccodin() {
    if [[ -z "$1" ]]; then
        echo "Usage: compile_c <filename.c>"
        return 1
    fi

    if [[ "$1" != *.c ]]; then
        echo "Error: Filename must have a .c extension"
        return 1
    fi

    local filename="${1%.c}"  # Remove .c extension for output name

    gcc -Wall -Wstrict-prototypes -Wmissing-prototypes -o "$filename" "$1"


    if [[ $? -eq 0 ]]; then
        echo "Compilation successful: ./$filename"
    else
        echo "Compilation failed."
    fi
}

