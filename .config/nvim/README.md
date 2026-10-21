# Neovim

## Requirements and first launch

- **Neovim 0.12+** (the current nvim-treesitter `main` API requires it).
- Git, ripgrep (`rg`), `curl`, `tar`, a C compiler, and **tree-sitter CLI 0.26.1+**.
  Install the CLI through your OS package manager or Cargo, not npm.
- Node/npm for web language servers; Python/venv support for Python tools;
  .NET SDK for C# tooling. A Nerd Font is recommended for icons.

Deploy this directory to `~/.config/nvim` using your usual dotfile setup, then
start `nvim`. lazy.nvim bootstraps itself. Run `:Lazy sync` and let `:Mason`
finish installing servers. Restart once initial parsers and servers finish.
Internet access is required for installation, not normal editing.

Use `:checkhealth`, `:Mason`, `:ConformInfo`, and `:Lazy` for troubleshooting.
The committed `lazy-lock.json` records tested plugin revisions; use `:Lazy restore`
to restore them and `:Lazy update` when intentionally upgrading.

### Migration from Packer

Packer is no longer used. lazy.nvim resets `packpath` to avoid loading old
`site/pack/packer/start` plugins alongside the new plugins. The old ignored
`plugin/packer_compiled.lua` was removed here; **remove it from any other deployed
copy too**, or it will try to load the obsolete configuration. Once happy with the
migration, the old `~/.local/share/nvim/site/pack/packer` directory can be removed.
Do not delete `site/parser` (Tree-sitter uses it).

## Preserved features and modern replacements

| Previous | Current |
| --- | --- |
| Packer | lazy.nvim with lockfile |
| nvim-cmp and cmp source adapters | blink.cmp with native Dadbod source |
| none-ls formatting | Conform (same formatters, plus SQLFluff) |
| none-ls diagnostics | nvim-lint (Trivy, markuplint, Stylelint) |
| old Tree-sitter setup API | current `main` API + native highlighting |
| williamboman Mason repositories | mason-org repositories + native `vim.lsp.config`/enable |
| GitHub live-server mirror / npm server | maintained Forgejo live-server (pure Lua) |

One Dark darker/transparent styling, lualine, right-side file tree (45 columns),
Telescope and UI-select, Noice, LuaSnip/friendly-snippets, autopairs, database UI,
and tmux navigation are retained. Modern Tree-sitter adds JS/TS/TSX and Markdown
while retaining the old parsers and the 100 KB highlighting limit. Native `gc`/`gcc`
provide commenting. which-key, Neogit (Magit equivalent), and Gitsigns (diff-hl
equivalent) make the Emacs-style workflows discoverable.

Python uses **basedpyright only**, avoiding competing Pyright/Jedi diagnostics.
C# retains OmniSharp. Lua, HTML, CSS, ESLint, YAML, SQL, and TypeScript servers
are installed/enabled through Mason. SQL remains `sqlls` + Dadbod, **not** Emacs's
SQLToolsService/MSSQL LSP. Existing DB driver/CLI prerequisites still apply.

### External formatting and lint tools

Install only the tools you use, via `:Mason` where available or their own installers:

- `stylua`, `prettier`, `csharpier`, `pyink`, `sqlfluff`
- `trivy`, `markuplint`, `stylelint`

Conform prefers project-local Prettier. Formatting is manual, with LSP fallback;
there is no unexpected format-on-save. SQLFluff uses the T-SQL dialect. Linters
run on read/save/insert-leave only when their executables are available; `:Lint`
runs them manually. Project-specific Stylelint/markuplint configuration is still
needed. Trivy is assigned to Dockerfile, Terraform, and YAML files.

## Emacs-style bindings

**Space is leader** in normal mode; pause after Space or a group prefix for
which-key. `Ctrl-Space` is an alternate leader in normal/visual mode and triggers
completion in insert mode. Leader commands are primarily normal-mode commands;
formatting, commenting, code actions, and SQL selection execution also work in
visual mode. `Ctrl-G` cancels/exits insert, command-line, visual, or terminal mode.

All entries below start with **Space**:

| Prefix | Bindings |
| --- | --- |
| `b` buffers | `bb`/`bi` list, `bd` delete, `bn`/`bp` next/previous, `br` reload, `bs` save |
| `f` files | `ff` find, `fr` recent, `fd` browser, `fs` save, `fS` save as |
| `g` Git | `gg` Neogit, `gb` blame line, `gd` diff file, `gl` file log, `gL` branch log, `gc` clone, `gG` toggle signs |
| `o` open | `ot` toggle tree, `op` project tree, `os` focus tree |
| `p` / `P` project | `pp` switch, `pf` files, `pss` grep, `pb` buffers, `pd` tree (`P` aliases `p`) |
| `w` windows | `wh/j/k/l` move, `ws` split, `wv` vertical split, `wc` close, `wo` only, `ww` next, `w=` equalize |
| `Tab` tabs | `Tab n` new, `Tab c` close, `Tab o` only, `Tab Tab` next, `Tab p` previous |
| `l` LSP | `lf` format, `lrr` rename, `laa` action, `lgd` definition, `lgi` implementation, `lgr` references, `lgt` type, `lge` diagnostics, `ls` symbols |
| `k` docs/comments | `ki` hover, `kc` comment line/selection |
| `t` toggles | `tt` theme, `tw` wrap, `td` diagnostics, `tl` start live server, `tL` stop |
| `m` SQL | `mo` explorer, `mc` choose connection, `mb` execute buffer, visual `mr` execute selection, `ms` sqlcmd terminal |
| `r` HTTP | `rr` send request under cursor, `ro` open response, `re` select environment |
| `?` | Show buffer-local mappings |

Project switching lists open-buffer Git/package/Python roots and immediate
subdirectories of `~/Development`. This is intentionally lighter than Projectile;
`pb` currently lists all buffers. `p` and `P` share one implementation rather than
two separate project systems. Neovim tabs contain window layouts, not Emacs buffers.

### Other bindings and compatibility

- `gd`, `gi`, `gr`, `K`: buffer-local LSP navigation/hover after a server attaches.
- `Ctrl-h/j/k/l`: correct tmux-aware navigation; insert `Ctrl-h` deletes backward.
- `j`/`k`: visual-line motions unless a count is supplied, matching Evil.
- `Alt-/`, `gcc`, visual `gc`: comment/uncomment.
- Completion: `Tab` accepts a candidate (or advances a snippet), `Shift-Tab`
  jumps backward, `Ctrl-y` accepts, `Ctrl-e` cancels, `Ctrl-b/f` scroll docs.
- Preserved aliases: `fg` grep, `fb` buffers, `fh` help, `;` tree focus,
  `Space Esc` tree close, `ca` code action, `Ctrl-k Ctrl-i` hover,
  `Ctrl-k Ctrl-d` format. Ctrl-k chords delay upward pane navigation until timeout.
- `Space r r` now sends HTTP requests, matching Emacs restclient. Rename is
  available at `Space l r r` instead of the old `Space r r` alias.
- **Intentional Emacs exceptions:** Emacs Org/RSS, Markdown conversion/CSS selection, font scaling,
  Sharper, Angular-specific LSP toggles, and MSSQL-specific connect/cancel/session
  commands are not ported. These were not features of the old Neovim config.
  SQL `mc` selects a Dadbod connection, not an MSSQL LSP connection.

## HTTP requests

[rest.nvim](https://github.com/rest-nvim/rest.nvim) provides a curl-backed HTTP
client for `.http` files. Run `:Lazy sync`, then restart Neovim so the HTTP parser
and dependencies are available. If needed, run `:TSInstall http` and wait for it
to finish. No API account, Python provider, or system LuaRocks install is needed;
the pure-Lua dependencies are managed by lazy.nvim and pinned in the lockfile.

Open `examples/requests.http` from this directory, adjust the base URL to your API,
and put the cursor on a request or its body:

- `Space r r`: send **only the request under the cursor**.
- `Space r o`: reopen the response pane (body/status, headers, cookies, statistics).
- `Space r e`: select a `.env` file for request variables.
- `q` in a response pane: close the split, or restore the previous buffer if it
  occupies the only window. Editable `.http` files retain normal macro recording.
- `Space l r r`: rename a code symbol (unchanged).

Separate requests with `###`. GET, POST, headers, JSON bodies, and `{{variables}}`
are supported. See the example for inline variables; for private tokens use an
untracked `.env` file selected with `Space r e`. Keep credentials out of committed
request examples. Only run trusted `.http` files: rest.nvim also supports scripted
request hooks. Requests are sent only when explicitly invoked.

## Private/local settings

`~/.config/nvim/local.lua` is loaded last and ignored by Git. For example:

```lua
-- Prefer environment variables; do not commit connection credentials.
vim.g.dbs = { development = vim.env.DEV_DATABASE_URL }
-- Optional format-on-save:
require("conform").setup({ format_on_save = { timeout_ms = 3000, lsp_format = "fallback" } })
```

For `sqlcmd`, provide connection options/environment in its terminal. Existing
tmux-side navigator configuration must still be installed separately.

## Smoke test

After installing plugins:

```sh
nvim --headless '+luafile ~/.config/nvim/tests/smoke.lua'
```

Checks Lua syntax, described leader mappings, legacy-plugin exclusion, SQL
completion, native LSP mapping scope, and plugin/command availability. It does
not contact databases, exercise tmux panes, or verify every external server/tool.

An additional HTTP integration test requires `python3` and uses only a temporary
loopback server (GET, POST, cursor-selected requests, variable substitution,
and closing response panes with `q`):

```sh
nvim --headless '+luafile ~/.config/nvim/tests/http.lua'
```
