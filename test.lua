#!/usr/bin/env lua

package.path = package.path .. ";tora/?.lua"

local core = require "core"
local Env = require "env"

local diff = [[
diff --old-line-format="F" --new-line-format="" --unchanged-line-format="." \
     __test__.expect __test__.out
]]

local tests = {"test/test"}
for i = 1, #tests do
	local test = tests[i]
	local out = assert(io.open(test..".out", "w"))
	io.output(out)
	core.interpret(test..".scm", Env.new(core.builtin))
	out:flush()
	--io.stdout:write(test, ": ")
	--io.stdout:flush()
	os.execute(diff:gsub("__test__", test)); print()
	out:close()
end
io.output(io.stdout)
