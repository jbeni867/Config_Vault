local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
    local output = vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "--branch=stable",
        "https://github.com/folke/lazy.nvim.git",
        lazypath,
    })
    if vim.v.shell_error ~= 0 then
        error("Could not bootstrap lazy.nvim:\n" .. output)
    end
end
vim.opt.rtp:prepend(lazypath)

-- These pure-Lua libraries keep modules at the repository root, not lua/.
local function lua_library(plugin)
    package.path = plugin.dir .. "/?.lua;" .. plugin.dir .. "/?/init.lua;" .. package.path
end

require("lazy").setup({
    {
        "navarasu/onedark.nvim",
        priority = 1000,
        config = function()
            require("configs.onedark_config")
        end,
    },
    {
        "nvim-lualine/lualine.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function()
            require("configs.lualine_config")
        end,
    },
    {
        "nvim-tree/nvim-tree.lua",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = function()
            require("configs.nvim-tree_config")
        end,
    },
    {
        "nvim-treesitter/nvim-treesitter",
        branch = "main",
        lazy = false,
        build = ":TSUpdate",
        config = function()
            require("configs.nvim-treesitter_config")
        end,
    },
    {
        "nvim-telescope/telescope.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-telescope/telescope-ui-select.nvim",
        },
        config = function()
            require("configs.telescope_config")
        end,
    },
    {
        "saghen/blink.cmp",
        version = "1.*",
        dependencies = {
            "L3MON4D3/LuaSnip",
            "rafamadriz/friendly-snippets",
            "kristijanhusak/vim-dadbod-completion",
        },
        config = function()
            require("configs.completions_config")
        end,
    },
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "mason-org/mason.nvim",
            "mason-org/mason-lspconfig.nvim",
            "saghen/blink.cmp",
        },
        config = function()
            require("configs.lsp_config")
        end,
    },
    {
        "stevearc/conform.nvim",
        config = function()
            require("configs.formatting")
        end,
    },
    {
        "mfussenegger/nvim-lint",
        config = function()
            require("configs.linting")
        end,
    },
    {
        "windwp/nvim-autopairs",
        config = function()
            require("configs.autopairs_config")
        end,
    },
    { url = "https://forge.barrettruth.com/barrettruth/live-server.nvim", name = "live-server.nvim" },
    { "kristijanhusak/vim-dadbod-ui", dependencies = { "tpope/vim-dadbod" } },
    {
        "folke/noice.nvim",
        dependencies = { "MunifTanjim/nui.nvim" },
        config = function()
            require("configs.noice_config")
        end,
    },
    { "christoomey/vim-tmux-navigator" },
    {
        "folke/which-key.nvim",
        opts = {
            preset = "modern",
            spec = {
                { "<leader>b", group = "buffers" },
                { "<leader>f", group = "files" },
                { "<leader>g", group = "git" },
                { "<leader>o", group = "open" },
                { "<leader>p", group = "project" },
                { "<leader>ps", group = "search" },
                { "<leader>P", group = "project (alias)" },
                { "<leader>Ps", group = "search" },
                { "<leader>w", group = "windows" },
                { "<leader><Tab>", group = "tabs" },
                { "<leader>l", group = "lsp" },
                { "<leader>lg", group = "goto" },
                { "<leader>lr", group = "refactor" },
                { "<leader>la", group = "actions" },
                { "<leader>t", group = "toggles" },
                { "<leader>k", group = "docs / comments" },
                { "<leader>m", group = "sql / databases" },
                { "<leader>r", group = "http requests" },
                { "<leader>c", group = "code" },
            },
        },
    },
    {
        "NeogitOrg/neogit",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "sindrets/diffview.nvim",
            "nvim-telescope/telescope.nvim",
        },
        opts = {},
    },
    { "lewis6991/gitsigns.nvim", opts = {} },
    {
        "rest-nvim/rest.nvim",
        ft = "http",
        cmd = "Rest",
        config = function()
            require("configs.rest_config")
        end,
        dependencies = {
            "nvim-neotest/nvim-nio",
            { "j-hui/fidget.nvim", opts = {} },
            { "lunarmodules/lua-mimetypes", config = lua_library },
            { "manoelcampos/xml2lua", config = lua_library },
        },
    },
}, {
    -- lazy resets packpath, so old Packer start packages are not loaded a second time.
    performance = { reset_packpath = true, rtp = { disabled_plugins = { "netrwPlugin" } } },
    -- rest.nvim's Lua libraries are pinned Git dependencies; parsers use Tree-sitter.
    rocks = { enabled = false },
    checker = { enabled = false },
    change_detection = { notify = false },
})
