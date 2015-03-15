#!/usr/bin/env lua

package.path = package.path .. ";tora/?.lua"

local core = require "core"
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
		if #inp > 0 then println(eval(read(inp))) end
		::continue::
	end
end

local function main(...)
	local args = {...}
	if #args > 0 then
		eval(read([[(load "]] .. args[1] .. [[")]]))
	else
		repl()
	end
end

main(...)
