-- Scope q to result panes, not editable .http files (where q records macros).
vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("UserRestResults", { clear = true }),
    pattern = "rest_nvim_result",
    callback = function(event)
        vim.keymap.set("n", "q", function()
            if vim.fn.winnr("$") > 1 then
                vim.cmd.close()
                return
            end
            -- :Rest open can replace the buffer in the only window.
            -- Return to its previous buffer instead of quitting Neovim.
            local previous = vim.fn.bufnr("#")
            if
                previous > 0
                and vim.api.nvim_buf_is_valid(previous)
                and vim.b[previous].__pane_group ~= "rest_nvim_result"
            then
                vim.api.nvim_set_current_buf(previous)
            else
                vim.cmd.enew()
            end
        end, {
            buffer = event.buf,
            silent = true,
            desc = "Close HTTP response",
        })
    end,
})
