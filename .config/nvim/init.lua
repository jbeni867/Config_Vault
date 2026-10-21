-- Set leaders before plugins or mappings are loaded.
vim.g.mapleader = " "
vim.g.maplocalleader = " "

if vim.fn.has("nvim-0.12") == 0 then
    error("This configuration requires Neovim 0.12 or newer")
end

require("options")
require("plugins")
require("keymaps")

-- Optional machine-local settings (database connections, etc.); never commit secrets.
local local_config = vim.fn.stdpath("config") .. "/local.lua"
if vim.uv.fs_stat(local_config) then
    dofile(local_config)
end
