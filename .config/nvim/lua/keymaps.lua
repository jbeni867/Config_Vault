-- Global Hotkeys
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.keymap.set({'', '!'}, '<c-g>', '<Esc>')

-- Vim-Tmux-Navigation
--  In order to switch from pane to pane with ctrl
vim.keymap.set('n', '<c-k>', ':wincmd k<CR>')
vim.keymap.set('n', '<c-j>', ':wincmd j<CR>')
vim.keymap.set('n', '<c-h>', ':wincmd h<CR>')
vim.keymap.set('n', '<c-l>', ':wincmd l<CR>')


-- Search Tweaks
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.scrolloff = 20

-- Ease of Use
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.sidescrolloff = 20

-- Tab Formatting
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.wrap = false
