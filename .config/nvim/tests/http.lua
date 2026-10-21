-- Requires python3; only contacts a temporary loopback server.
-- nvim --headless '+luafile ~/.config/nvim/tests/http.lua'
local server
local directory = vim.fn.tempname()
local function main()
    vim.fn.mkdir(directory, "p")
    local port
    server = vim.fn.jobstart({
        "python3",
        "-u",
        "-c",
        [[
from http.server import BaseHTTPRequestHandler, HTTPServer
class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        body = b'{"method":"GET"}'
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(body)
    def do_POST(self):
        body = self.rfile.read(int(self.headers.get('Content-Length', 0)))
        self.send_response(201)
        self.send_header('Content-Type', 'application/json')
        self.end_headers()
        self.wfile.write(body)
    def log_message(self, *args):
        pass
server = HTTPServer(('127.0.0.1', 0), Handler)
print(server.server_port, flush=True)
server.serve_forever()
]],
    }, {
        on_stdout = function(_, lines)
            for _, line in ipairs(lines) do
                port = tonumber(line) or port
            end
        end,
    })
    assert(server > 0, "Could not start Python HTTP server")
    assert(
        vim.wait(5000, function()
            return port ~= nil
        end),
        "HTTP server did not start"
    )
    local path = directory .. "/requests.http"
    vim.fn.writefile({
        "@base_url = http://127.0.0.1:" .. port,
        "### GET",
        "GET {{base_url}}/health",
        "",
        "### POST",
        "POST {{base_url}}/items",
        "Content-Type: application/json",
        "",
        '{"message":"http-test"}',
    }, path)
    vim.cmd("edit " .. vim.fn.fnameescape(path))
    require("nvim-treesitter").install({ "http" }):wait(60000)
    vim.treesitter.get_parser(0, "http"):parse()
    local response
    vim.api.nvim_create_autocmd("User", {
        pattern = "RestResponse",
        callback = function()
            response = vim.deepcopy(_G.rest_response)
        end,
    })
    local request_buffer = vim.api.nvim_get_current_buf()
    local function send(line, command)
        vim.api.nvim_set_current_buf(request_buffer)
        vim.api.nvim_win_set_cursor(0, { line, 0 })
        response = nil
        vim.cmd(command)
        assert(
            vim.wait(10000, function()
                return response ~= nil
            end),
            "No HTTP response received"
        )
        return response
    end
    local get = send(3, "Rest run")
    assert(get.status.code == 200 and get.body:find('"GET"', 1, true), "GET failed")
    local post = send(9, "Rest run")
    assert(post.status.code == 201 and post.body:find("http-test", 1, true), "POST/cursor selection failed")
    -- Every response tab (body/headers/cookies/statistics) needs the local q mapping.
    local result_buffers = {}
    for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
        if vim.b[buffer].__pane_group == "rest_nvim_result" then
            result_buffers[#result_buffers + 1] = buffer
            vim.api.nvim_buf_call(buffer, function()
                local mapping = vim.fn.maparg("q", "n", false, true)
                assert(mapping.buffer == 1 and mapping.desc == "Close HTTP response", "Missing result q mapping")
            end)
        end
    end
    assert(#result_buffers == 4, "Expected four HTTP result panes")
    local result_window
    for _, window in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        if vim.b[vim.api.nvim_win_get_buf(window)].__pane_group == "rest_nvim_result" then
            result_window = window
        end
    end
    assert(result_window, "HTTP response window was not opened")
    vim.api.nvim_set_current_win(result_window)
    vim.cmd("normal q")
    assert(not vim.api.nvim_win_is_valid(result_window), "q did not close the result split")
    assert(vim.api.nvim_get_current_buf() == request_buffer, "q did not return to the request file")
    assert(vim.fn.maparg("q", "n") == "", "q mapping leaked into editable request file")

    -- Reopening in the sole window should also be dismissible without exiting.
    vim.cmd.only()
    vim.cmd("Rest open")
    vim.cmd("normal q")
    assert(vim.api.nvim_get_current_buf() == request_buffer, "q did not restore the previous buffer")
    assert(vim.api.nvim_buf_is_valid(request_buffer), "Request buffer was deleted")
    print("HTTP request and response-close tests passed")
end

local ok, err = xpcall(main, debug.traceback)
if server and server > 0 then
    vim.fn.jobstop(server)
end
vim.fn.delete(directory, "rf")
if not ok then
    vim.api.nvim_err_writeln(err)
    vim.cmd("cquit 1")
end
vim.cmd("qa!")
