require("mason").setup()

-- One Python server, rather than pyright + basedpyright + Jedi competing.
local servers = { "lua_ls", "csharp_ls", "basedpyright", "html", "cssls", "eslint", "yamlls", "sqlls", "ts_ls" }
vim.lsp.config("*", { capabilities = require("blink.cmp").get_lsp_capabilities() })
vim.lsp.config("lua_ls", {
    settings = {
        Lua = {
            diagnostics = { globals = { "vim" } },
            workspace = { library = { vim.env.VIMRUNTIME }, checkThirdParty = false },
        },
    },
})
require("mason-lspconfig").setup({ ensure_installed = servers, automatic_enable = servers })

vim.api.nvim_create_autocmd("LspAttach", {
    group = vim.api.nvim_create_augroup("UserLspMappings", { clear = true }),
    callback = function(event)
        local function map(lhs, rhs, desc)
            vim.keymap.set("n", lhs, rhs, { buffer = event.buf, silent = true, desc = desc })
        end
        map("gd", vim.lsp.buf.definition, "Definition")
        map("gi", vim.lsp.buf.implementation, "Implementation")
        map("gr", require("telescope.builtin").lsp_references, "References")
        map("K", vim.lsp.buf.hover, "Hover documentation")
    end,
})
