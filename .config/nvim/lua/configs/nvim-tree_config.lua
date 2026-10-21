require("nvim-tree").setup({
    hijack_netrw = false, -- netrw is already disabled in options.lua
    sort = {
        sorter = "case_sensitive",
    },
    view = {
        width = 45,
        side = "right",
    },
    renderer = {
        group_empty = true,
    },
    filters = {
        dotfiles = false,
    },
})
