#!/usr/bin/env bash

sudo apt-get update && sudo apt-get upgrade -y
sudo apt-get install git dotnet-sdk-8.0 stow emacs ripgrep fd-find make cmake fzf lf nextcloud-desktop kitty build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev texlive dvipng texlive-latex-extra libtool libvterm-dev software-properties-common tmux -y

curl -fsSL https://tailscale.com/install.sh | sh

sudo add-apt-repository ppa:neovim-ppa/unstable
sudo apt-get update
sudo apt-get install neovim -y

curl -fsSL https://pyenv.run | bash
pyenv install 3.12.0
pyenv global 3.12.0

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
nvm install node
nvm use node

git clone https://github.com/jbeni867/Config_Vault ~/Config_Vault
cd ~/Config_Vault
stow --adopt .
git restore .

git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
doom sync

git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
