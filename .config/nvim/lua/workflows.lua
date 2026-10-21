local M = {}

function M.root()
    return vim.fs.root(0, { ".git", "angular.json", "package.json", "pyproject.toml" }) or vim.fn.getcwd()
end

function M.project_files()
    require("telescope.builtin").find_files({ cwd = M.root() })
end

function M.project_grep()
    require("telescope.builtin").live_grep({ cwd = M.root() })
end

function M.projects()
    local roots = { [M.root()] = true }
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        local name = vim.api.nvim_buf_get_name(buf)
        local root = name ~= "" and vim.fs.root(name, { ".git", "package.json", "pyproject.toml" })
        if root then
            roots[root] = true
        end
    end
    local development = vim.fn.expand("~/Development")
    if vim.fn.isdirectory(development) == 1 then
        for name, kind in vim.fs.dir(development) do
            if kind == "directory" then
                roots[development .. "/" .. name] = true
            end
        end
    end
    local choices = vim.tbl_keys(roots)
    table.sort(choices)
    vim.ui.select(choices, { prompt = "Switch project" }, function(root)
        if root then
            vim.api.nvim_set_current_dir(root)
            require("telescope.builtin").find_files({ cwd = root })
        end
    end)
end

function M.save_as()
    vim.ui.input({ prompt = "Save as: ", default = vim.api.nvim_buf_get_name(0), completion = "file" }, function(path)
        if path and path ~= "" then
            vim.cmd("saveas " .. vim.fn.fnameescape(path))
        end
    end)
end

function M.clone()
    vim.ui.input({ prompt = "Git clone URL: " }, function(url)
        if not url or url == "" then
            return
        end
        vim.ui.input({ prompt = "Destination: ", completion = "dir" }, function(path)
            if not path or path == "" then
                return
            end
            vim.system({ "git", "clone", "--", url, vim.fn.expand(path) }, { text = true }, function(result)
                vim.schedule(function()
                    vim.notify(
                        result.code == 0 and "Repository cloned" or result.stderr,
                        result.code == 0 and vim.log.levels.INFO or vim.log.levels.ERROR
                    )
                end)
            end)
        end)
    end)
end

function M.sqlcmd()
    if vim.fn.executable("sqlcmd") == 0 then
        vim.notify("Install sqlcmd and configure your connection flags in the terminal", vim.log.levels.WARN)
        return
    end
    vim.cmd("split | terminal sqlcmd")
    vim.cmd.startinsert()
end

return M
