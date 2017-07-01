local Env = require "tora.env"
local util = require "tora.util"
local map, slice, raise = util.map, util.slice, util.raise
local mt = {__index = util}

local function equal(a, b)
	if type(a) == "table" and type(b) == "table" then
		if #a ~= #b then return false end
		for i = 1, #a do
			if not equal(a[i], b[i]) then return false end
		end
		return true
	else
		return a == b
	end
end

local function core_tostring(a)
	if type(a) == "table" then
		return "(" .. table.concat(map(a, core_tostring), " ") .. ")"
	elseif type(a) == "string" and a:match("^\"") then
		return a:sub(2, -2)
	elseif a == true or a == false then
		return a == true and "#t" or "#f"
	else
		return tostring(a)
	end
end

local builtin = Env.new()

for sym, val in pairs {

	["+"]        = function (a, b) return a + b end,
	["-"]        = function (a, b) return a - b end,
	["*"]        = function (a, b) return a * b end,
	["/"]        = function (a, b) return a / b end,

	["="]        = function (a, b) return a == b end,
	[">"]        = function (a, b) return a > b end,
	["<"]        = function (a, b) return a < b end,
	[">="]       = function (a, b) return a >= b end,
	["<="]       = function (a, b) return a <= b end,

	["not"]      = function (a) return not a end,

	["newline"]  = function ( ) io.write("\n") end,
	["nl"]       = "\n",

	["list"]     = function (...) return setmetatable({...}, mt) end,
	["cons"]     = function (x, xs) return setmetatable({x, unpack(xs)}, mt) end,
	["car"]      = function (xs) return xs[1] end,
	["cdr"]      = function (xs) return slice(xs, 2) end,
	["length"]   = function (xs) return #xs end,

	["number?"]  = function (a) return type(a) == "number" end,
	["boolean?"] = function (a) return type(a) == "boolean" end,
	["string?"]  = function (a) return type(a) == "string" and a:find("^\"") ~= nil end,
	["symbol?"]  = function (a) return type(a) == "string" and a:find("^\"") == nil end,
	["lambda?"]  = function (a) return type(a) == "function" end,

	["list?"]    = function (a) return type(a) == "table" end,
	["null?"]    = function (a) return type(a) == "table" and #a == 0 or false end,
	["pair?"]    = function (a) return type(a) == "table" and #a ~= 0 or false end,
	["equal?"]   = equal,

	["even?"]    = function (a) return a % 2 == 0 end,
	["odd?"]     = function (a) return a % 2 ~= 0 end,

	["show"]     = function (a) return ("%q"):format(core_tostring(a)) end,

} do Env.add(builtin, sym, val) end

builtin.core = {
	show = core_tostring
}

return builtin
