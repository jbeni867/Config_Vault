-- disable netrw at the very start of your init.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- optionally enable 24-bit colour
vim.opt.termguicolors = true

-- Nvim-Tree Keymaps
vim.keymap.set("n", "<Leader>;", "<cmd>:NvimTreeFocus<cr>")
vim.keymap.set("n", "<Leader><ESC>", "<cmd>:NvimTreeClose<cr>")

-- setup with some options
require("nvim-tree").setup({
	sort = {
		sorter = "case_sensitive",
	},
	view = {
		width = 45,
		side = "right",
	},
	renderer = {
		group_empty = true,
	},
	filters = {
		dotfiles = false,
	},
})
