-- Run after installing plugins: nvim --headless '+luafile ~/.config/nvim/tests/smoke.lua'
local function check(condition, message)
    assert(condition, message)
end

local function main()
    -- Vimscript plugins use silent! for optional cleanup, which can leave v:errmsg set.
    -- Startup errors are checked separately in the headless startup log.
    vim.v.errmsg = ""
    for _, path in ipairs(vim.fn.globpath(vim.fn.stdpath("config"), "**/*.lua", false, true)) do
        check(loadfile(path), "Invalid Lua: " .. path)
    end
    for _, key in ipairs({ "bb", "bd", "ff", "fs", "gg", "ot", "pp", "pss", "lf", "lrr", "ki", "kc", "mo", "rr" }) do
        local mapping = vim.fn.maparg(" " .. key, "n", false, true)
        check(mapping.desc and mapping.desc ~= "", "Missing described mapping: SPC " .. key)
    end
    check(vim.fn.maparg("<C-h>", "n"):find("TmuxNavigateLeft", 1, true), "Broken tmux navigation")
    check(vim.fn.exists(":DBUI") == 2, "Dadbod UI not loaded")
    check(vim.fn.exists(":LiveServerStart") == 2, "Live server not loaded")
    check(vim.fn.exists(":Lint") == 2, "Lint command not loaded")
    check(
        require("blink.cmp").get_lsp_capabilities().textDocument.completion.completionItem.snippetSupport,
        "Missing snippet capabilities"
    )
    check(require("vim_dadbod_completion.blink"), "SQL completion provider unavailable")
    check(require("conform").list_formatters_for_buffer, "Conform unavailable")
    check(require("which-key").show, "which-key unavailable")
    check(
        not package.loaded.packer and not package.loaded.cmp and not package.loaded["null-ls"],
        "Legacy plugins loaded"
    )

    vim.cmd("edit " .. vim.fn.fnameescape(vim.fn.stdpath("config") .. "/init.lua"))
    vim.api.nvim_exec_autocmds("LspAttach", { buffer = 0, data = { client_id = 0 } })
    check(vim.fn.maparg("gd", "n", false, true).buffer == 1, "LSP mapping is not buffer-local")
    vim.cmd("enew")
    vim.bo.filetype = "sql"
    check(require("blink.cmp.config").sources.per_filetype.sql[1] == "dadbod", "SQL completion not configured")
    check(vim.v.errmsg == "", "Runtime error: " .. vim.v.errmsg)
    print("Neovim smoke tests passed")
end

local ok, err = xpcall(main, debug.traceback)
if not ok then
    vim.api.nvim_err_writeln(err)
    vim.cmd("cquit 1")
end
vim.cmd("qa!")
