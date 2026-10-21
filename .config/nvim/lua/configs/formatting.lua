require("conform").setup({
    formatters_by_ft = {
        lua = { "stylua" },
        python = { "pyink" },
        cs = { "csharpier" },
        javascript = { "prettier" },
        javascriptreact = { "prettier" },
        typescript = { "prettier" },
        typescriptreact = { "prettier" },
        html = { "prettier" },
        css = { "prettier" },
        scss = { "prettier" },
        json = { "prettier" },
        jsonc = { "prettier" },
        yaml = { "prettier" },
        markdown = { "prettier" },
        sql = { "sqlfluff" },
    },
    formatters = { sqlfluff = { append_args = { "--dialect", "tsql" } } },
    default_format_opts = { lsp_format = "fallback", timeout_ms = 3000 },
    -- Keep the old manual-format behavior. Opt in to format-on-save in local.lua.
})
