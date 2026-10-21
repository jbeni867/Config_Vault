require("luasnip.loaders.from_vscode").lazy_load()
require("blink.cmp").setup({
    snippets = { preset = "luasnip" },
    -- Lua implementation avoids platform-specific binary downloads/builds.
    fuzzy = { implementation = "lua" },
    keymap = {
        preset = "default",
        ["<Tab>"] = { "select_and_accept", "snippet_forward", "fallback" },
        ["<S-Tab>"] = { "snippet_backward", "fallback" },
        ["<C-k>"] = false, -- Do not shadow window navigation / legacy hover chord.
    },
    completion = {
        menu = { border = "rounded" },
        documentation = { auto_show = true, window = { border = "rounded" } },
        list = { selection = { preselect = false, auto_insert = false } },
    },
    sources = {
        default = { "lsp", "path", "snippets", "buffer" },
        per_filetype = {
            sql = { "dadbod", "lsp", "snippets", "buffer" },
            mysql = { "dadbod", "lsp", "snippets", "buffer" },
            plsql = { "dadbod", "lsp", "snippets", "buffer" },
        },
        providers = {
            dadbod = { name = "DB", module = "vim_dadbod_completion.blink" },
        },
    },
})
