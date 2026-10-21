local lint = require("lint")
lint.linters_by_ft = {
    html = { "markuplint" },
    css = { "stylelint" },
    scss = { "stylelint" },
    dockerfile = { "trivy" },
    terraform = { "trivy" },
    yaml = { "trivy" },
}
local function run()
    if vim.bo.buftype ~= "" then
        return
    end
    local available = {}
    for _, name in ipairs(lint.linters_by_ft[vim.bo.filetype] or {}) do
        local def = lint.linters[name]
        if type(def) == "function" then
            def = def()
        end
        if def and type(def.cmd) == "string" and vim.fn.executable(def.cmd) == 1 then
            available[#available + 1] = name
        end
    end
    if #available > 0 then
        lint.try_lint(available)
    end
end
vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost", "InsertLeave" }, {
    group = vim.api.nvim_create_augroup("UserLint", { clear = true }),
    callback = run,
})
vim.api.nvim_create_user_command("Lint", run, { desc = "Run available linters for this file" })
