#!/usr/bin/env lua

local core = require "tora.core"
local Env = require "tora.env"

local diff = [[
diff --old-line-format="F" --new-line-format="" --unchanged-line-format="." \
     __test__.expect __test__.out
]]

core.eval(core.read([[(load "tora/macro.scm")]]))

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
