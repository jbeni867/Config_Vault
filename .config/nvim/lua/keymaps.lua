local function map(mode, lhs, rhs, desc, extra)
    vim.keymap.set(mode, lhs, rhs, vim.tbl_extend("force", { silent = true, desc = desc }, extra or {}))
end
local function telescope(name)
    return function()
        require("telescope.builtin")[name]()
    end
end
local function workflow(name)
    return function()
        require("workflows")[name]()
    end
end
local function format()
    require("conform").format({ async = true })
end

map({ "n", "v", "o", "i", "c" }, "<C-g>", "<Esc>", "Normal mode / cancel")
map("t", "<C-g>", "<C-\\><C-n>", "Normal mode")
map("i", "<C-h>", "<BS>", "Delete backward")
map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", "Next visual line", { expr = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", "Previous visual line", { expr = true })
-- Emacs's global leader in normal/visual mode; insert C-Space stays completion.
map({ "n", "x" }, "<C-Space>", "<Space>", "Leader", { remap = true })
for key, direction in pairs({ h = "Left", j = "Down", k = "Up", l = "Right" }) do
    map("n", "<C-" .. key .. ">", "<cmd>TmuxNavigate" .. direction .. "<CR>", "Navigate " .. direction)
end

map("n", "<leader>bb", telescope("buffers"), "Switch buffer")
map("n", "<leader>bi", telescope("buffers"), "List buffers")
map("n", "<leader>bd", "<cmd>bdelete<CR>", "Kill buffer")
map("n", "<leader>bn", "<cmd>bnext<CR>", "Next buffer")
map("n", "<leader>bp", "<cmd>bprevious<CR>", "Previous buffer")
map("n", "<leader>br", "<cmd>edit<CR>", "Reload buffer (protect unsaved changes)")
map("n", "<leader>bs", "<cmd>write<CR>", "Save buffer")
map("n", "<leader>ff", telescope("find_files"), "Find file")
map("n", "<leader>fr", telescope("oldfiles"), "Recent files")
map("n", "<leader>fd", "<cmd>NvimTreeFindFile<CR>", "File browser")
map("n", "<leader>fs", "<cmd>write<CR>", "Save file")
map("n", "<leader>fS", workflow("save_as"), "Save file as")
map("n", "<leader>fg", telescope("live_grep"), "Search text (legacy)")
map("n", "<leader>fb", telescope("buffers"), "Buffers (legacy)")
map("n", "<leader>fh", telescope("help_tags"), "Help tags")

map("n", "<leader>gg", "<cmd>Neogit<CR>", "Git status")
map("n", "<leader>gb", function()
    require("gitsigns").blame_line({ full = true })
end, "Blame line")
map("n", "<leader>gd", function()
    require("gitsigns").diffthis()
end, "Diff file")
map("n", "<leader>gl", telescope("git_bcommits"), "Log file")
map("n", "<leader>gL", telescope("git_commits"), "Log branch")
map("n", "<leader>gc", workflow("clone"), "Clone repository")
map("n", "<leader>gG", function()
    require("gitsigns").toggle_signs()
end, "Toggle Git signs")

map("n", "<leader>ot", "<cmd>NvimTreeToggle<CR>", "Toggle tree")
map("n", "<leader>op", function()
    require("nvim-tree.api").tree.open({ path = require("workflows").root() })
end, "Project tree")
map("n", "<leader>os", "<cmd>NvimTreeFocus<CR>", "Focus tree")
map("n", "<leader>;", "<cmd>NvimTreeFocus<CR>", "Focus tree (legacy)")
map("n", "<leader><Esc>", "<cmd>NvimTreeClose<CR>", "Close tree (legacy)")
for _, prefix in ipairs({ "p", "P" }) do
    map("n", "<leader>" .. prefix .. "p", workflow("projects"), "Switch project")
    map("n", "<leader>" .. prefix .. "f", workflow("project_files"), "Find project file")
    map("n", "<leader>" .. prefix .. "ss", workflow("project_grep"), "Search project")
    map("n", "<leader>" .. prefix .. "b", telescope("buffers"), "Switch buffer (all projects)")
    map("n", "<leader>" .. prefix .. "d", function()
        require("nvim-tree.api").tree.open({ path = require("workflows").root() })
    end, "Browse project")
end
for key, desc in pairs({
    h = "Focus left",
    j = "Focus below",
    k = "Focus above",
    l = "Focus right",
    s = "Horizontal split",
    v = "Vertical split",
    c = "Close window",
    o = "Only window",
    w = "Next window",
    ["="] = "Equalize windows",
}) do
    map("n", "<leader>w" .. key, "<C-w>" .. key, desc)
end
map("n", "<leader><Tab>n", "<cmd>tabnew<CR>", "New tab")
map("n", "<leader><Tab>c", "<cmd>tabclose<CR>", "Close tab")
map("n", "<leader><Tab>o", "<cmd>tabonly<CR>", "Only tab")
map("n", "<leader><Tab><Tab>", "<cmd>tabnext<CR>", "Next tab")
map("n", "<leader><Tab>p", "<cmd>tabprevious<CR>", "Previous tab")

map({ "n", "x" }, "<leader>lf", format, "Format buffer / selection")
map("n", "<leader>lrr", vim.lsp.buf.rename, "Rename symbol")
map({ "n", "x" }, "<leader>laa", vim.lsp.buf.code_action, "Code action")
map({ "n", "x" }, "<leader>ca", vim.lsp.buf.code_action, "Code action (legacy)")
map("n", "<leader>lgd", vim.lsp.buf.definition, "Definition")
map("n", "<leader>lgi", vim.lsp.buf.implementation, "Implementation")
map("n", "<leader>lgr", telescope("lsp_references"), "References")
map("n", "<leader>lgt", vim.lsp.buf.type_definition, "Type definition")
map("n", "<leader>lge", telescope("diagnostics"), "List diagnostics")
map("n", "<leader>ls", telescope("lsp_document_symbols"), "Document symbols")
map("n", "<leader>ki", vim.lsp.buf.hover, "Hover documentation")
map("n", "<C-k><C-i>", vim.lsp.buf.hover, "Hover documentation (legacy)")
map({ "n", "x" }, "<C-k><C-d>", format, "Format (legacy)")
map("n", "<leader>kc", "gcc", "Comment line", { remap = true })
map("x", "<leader>kc", "gc", "Comment selection", { remap = true })
map("n", "<M-/>", "gcc", "Comment line", { remap = true })
map("x", "<M-/>", "gc", "Comment selection", { remap = true })
map("n", "<leader>tt", telescope("colorscheme"), "Choose theme")
map("n", "<leader>tw", function()
    vim.wo.wrap = not vim.wo.wrap
end, "Toggle wrap")
map("n", "<leader>td", function()
    vim.diagnostic.enable(not vim.diagnostic.is_enabled())
end, "Toggle diagnostics")
map("n", "<leader>tl", "<cmd>LiveServerStart<CR>", "Start live server")
map("n", "<leader>tL", "<cmd>LiveServerStop<CR>", "Stop live server")
map("n", "<leader>?", function()
    require("which-key").show({ global = false })
end, "Buffer keymaps")

map("n", "<leader>mo", "<cmd>DBUIToggle<CR>", "Database explorer")
map("n", "<leader>mc", "<cmd>DBUI<CR>", "Choose database connection")
map("n", "<leader>mb", "<cmd>%DB<CR>", "Execute SQL buffer")
map("x", "<leader>mr", ":DB<CR>", "Execute SQL selection")
map("n", "<leader>ms", workflow("sqlcmd"), "Open sqlcmd")
map("n", "<leader>rr", vim.lsp.buf.rename, "Rename symbol (legacy)")
