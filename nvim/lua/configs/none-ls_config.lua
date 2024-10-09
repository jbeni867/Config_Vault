local null_ls = require("null-ls")

null_ls.setup({
    sources = {
        null_ls.builtins.formatting.stylua,
        null_ls.builtins.formatting.prettier,
        null_ls.builtins.formatting.csharpier,
        null_ls.builtins.formatting.pyink,
        null_ls.builtins.diagnostics.trivy,
        null_ls.builtins.diagnostics.markuplint,
        null_ls.builtins.diagnostics.stylelint,
    },
})

vim.keymap.set("n", "<C-k><C-d>", vim.lsp.buf.format, {})
