-- Global Hotkeys
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Search Tweaks
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.scrolloff = 20

-- Ease of Use
vim.opt.number = true
vim.opt.relativenumber = true

-- Nvim-Tree Keymaps
vim.keymap.set('n', '<Leader>;', '<cmd>:NvimTreeToggle<cr>')
vim.keymap.set('n', '<s-esc>', '<cmd>:NvimTreeClose<cr>')
