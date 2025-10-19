#!/usr/bin/env bash

# Update and install essential packages
sudo dnf in neovim emacs dotnet-sdk-8.0 dotnet-sdk-9.0 stow ripgrep fd-find cmake fzf nextcloud kitty texlive texlive-latex libtool libvterm-devel tmux mscore-fonts tldr btop -y
sudo dnf in make gcc patch zlib-devel bzip2 bzip2-devel readline-devel sqlite sqlite-devel openssl-devel tk-devel libffi-devel xz-devel libuuid-devel gdbm-libs libnsl2 -y
sudo dnf in @development-tools zlib-devel bzip2 bzip2-devel readline-devel sqlite sqlite-devel openssl-devel xz-devel libffi-devel findutils tk-devel tcl-devel -y

# Intstall lf file manager
sudo dnf copr enable pennbauman/ports
sudo dnf in lf -y

# Clone and apply dotfiles from Config_Vault
git clone https://github.com/jbeni867/Config_Vault ~/Config_Vault
cd ~/Config_Vault
stow --adopt .
git restore .

# Install pyenv and configure Python
curl -fsSL https://pyenv.run | bash
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
pyenv install 3.13 && pyenv global 3.13

# Install NVM and Node.js
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion
nvm install node && nvm use node

# Install Tailscale
curl -fsSL https://tailscale.com/install.sh | sh

# Install Tmux Plugin Manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Create Dev Folder
mkdir ~/Development\

# Setup Git-Credential-Manager
dotnet tool install -g git-credential-manager
git config --global credential.credentialStore secretservice
git-credential-manager github login
git-credential-manager configure
git config --global user.name "jbenitez"
git config --global user.email "jordybeni867@gmail.com"
