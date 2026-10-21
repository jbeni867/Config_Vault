vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.tmux_navigator_no_mappings = 1

vim.opt.termguicolors = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.scrolloff = 20
vim.opt.sidescrolloff = 20
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.wrap = false
vim.opt.undofile = true
vim.opt.signcolumn = "yes"
vim.opt.updatetime = 250
vim.opt.timeoutlen = 400
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.completeopt = { "menu", "menuone", "noselect" }

vim.diagnostic.config({ severity_sort = true, float = { border = "rounded" } })
