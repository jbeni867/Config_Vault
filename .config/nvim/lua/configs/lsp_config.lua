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

require("lspconfig").lua_ls.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").omnisharp.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").pyright.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").basedpyright.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").html.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").cssls.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").eslint.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").yamlls.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").sqlls.setup({ on_attach = on_attach, capabilities = capabilities })
require("lspconfig").jedi_language_server.setup({ on_attach = on_attach, capabilities = capabilities })
