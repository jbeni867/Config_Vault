local null_ls = require("null-ls")

null_ls.setup({
    sources = {
        null_ls.builtins.formatting.stylua,
        null_ls.builtins.formatting.prettier,
        null_ls.builtins.formatting.csharpier,
        null_ls.builtins.diagnostics.trivy,
    },
})

vim.keymap.set("n", "<C-k><C-d>", vim.lsp.buf.format, {})
