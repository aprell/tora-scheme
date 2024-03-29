#!/usr/bin/env lua

local core = require "src.core"
local read, eval, println = core.read, core.eval, core.println

local usage = [[
  :bindings, :b   Show all bindings in the global environment
  :help, :h       Print this help text
  :quit, :q       Exit REPL
]]

local cmds
cmds = {
    [":bindings"] = function () print(core.builtin) end,
    [":help"]     = function () io.write(usage) end,
    [":quit"]     = function () os.exit() end,
    [":b"]        = function () cmds[":bindings"]() end,
    [":h"]        = function () cmds[":help"]() end,
    [":q"]        = function () cmds[":quit"]() end,
}

setmetatable(cmds, {__index = function (_, k)
    io.write("Unrecognized command ", k, "\n")
    return function () end
end})

-- Simple read-eval-print loop
local function repl(prompt)
    prompt = prompt or "λ> "
    while true do
        io.write(prompt)
        local inp = io.read()
        if not inp then io.write("\n"); break end
        if inp:find("^:") then cmds[inp](); goto continue end
        if #inp > 0 then
            local ok, err = pcall(function () println(eval(read(inp))) end)
            if not ok then print(err) end
        end
        ::continue::
    end
end

local function main(...)
    eval(read([[(load "src/macro.scm")]]))
    if #arg > 0 then
        if arg[1] == "-l" then
            table.remove(arg, 1)
            assert(arg[1], "This option requires a file")
            eval(read(([[(define argv '(%s))]]):format(table.concat(arg, " "))))
            eval(read([[(load "]] .. arg[1] .. [[")]]))
            repl()
        else
            eval(read(([[(define argv '(%s))]]):format(table.concat(arg, " "))))
            eval(read([[(load "]] .. arg[1] .. [[")]]))
        end
    else
        repl()
    end
end

main(...)
