local ts = require("nvim-treesitter")
ts.setup({})
local parsers = {
    "c",
    "lua",
    "vim",
    "vimdoc",
    "query",
    "bash",
    "python",
    "xml",
    "c_sharp",
    "html",
    "css",
    "json",
    "sql",
    "regex",
    "javascript",
    "typescript",
    "tsx",
    "yaml",
    "markdown",
    "markdown_inline",
}
-- Installation is asynchronous; highlighting gracefully falls back until ready.
if vim.fn.executable("tree-sitter") == 1 then
    ts.install(parsers)
end
vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("UserTreesitter", { clear = true }),
    callback = function(event)
        local name = vim.api.nvim_buf_get_name(event.buf)
        local stat = vim.uv.fs_stat(name)
        if stat and stat.size > 100 * 1024 then
            return
        end
        if pcall(vim.treesitter.start, event.buf) then
            vim.bo[event.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end
    end,
})
