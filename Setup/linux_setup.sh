#!/usr/bin/env bash

# Update and install essential packages
sudo apt-get update && sudo apt-get upgrade -y
sudo apt-get install git dotnet-sdk-8.0 stow emacs ripgrep fd-find make cmake fzf lf nextcloud-desktop kitty build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev texlive dvipng texlive-latex-extra libtool libvterm-dev software-properties-common tmux ttf-mscorefonts-installer -y

# Clone and apply dotfiles from Config_Vault
git clone https://github.com/jbeni867/Config_Vault ~/Config_Vault
cd ~/Config_Vault
stow --adopt .
git restore .

# Install pyenv and configure Python
curl -fsSL https://pyenv.run | bash
bash --login -i -c 'pyenv install 3.12.0 && pyenv global 3.12.0'

# Install NVM and Node.js
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
bash --login -i -c 'nvm install node && nvm use node'

# Install Neovim
sudo add-apt-repository ppa:neovim-ppa/unstable -y
sudo apt-get update
sudo apt-get install neovim -y

# Install Tailscale
curl -fsSL https://tailscale.com/install.sh | sh

# Install Doom Emacs
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
~/.emacs.d/bin/doom sync

# Install Tmux Plugin Manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Install NordVPN
sh <(curl -sSf https://downloads.nordcdn.com/apps/linux/install.sh)

# Create Dev Folder
mkdir ~/Development
