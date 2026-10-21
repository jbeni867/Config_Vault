require("mason").setup()

require("mason-lspconfig").setup({
	ensure_installed = { "lua_ls", "omnisharp", "pyright", "html", "cssls", "eslint", "yamlls", "sqlls", "basedpyright"},
})

local on_attach = function(_, _)
	vim.keymap.set("n", "<leader>rr", vim.lsp.buf.rename, {})
	vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, {})

	vim.keymap.set("n", "gd", vim.lsp.buf.definition, {})
	vim.keymap.set("n", "gi", vim.lsp.buf.implementation, {})
	vim.keymap.set("n", "gr", require("telescope.builtin").lsp_references, {})
	vim.keymap.set("n", "<c-k><c-i>", vim.lsp.buf.hover, {})
end

local capabilities = require("cmp_nvim_lsp").default_capabilities()

vim.lsp.config("lua_ls", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("omnisharp", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("pyright", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("basedpyright", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("html", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("cssls", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("eslint", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("yamlls", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("sqlls", { on_attach = on_attach, capabilities = capabilities })
vim.lsp.config("jedi_language_server", { on_attach = on_attach, capabilities = capabilities })
