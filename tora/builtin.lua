local Env = require "env"
local util = require "util"
local slice = util.slice

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

	["and"]      = function (a, b) if a then return b else return a end end,
	["or"]       = function (a, b) if a then return a else return b end end,
	["not"]      = function (a) return not a end,
	["else"]     = true, -- else-clause is always true

	["newline"]  = function ( ) io.write("\n") end,
	["nl"]       = "\n",

	["list"]     = function (...) return {...} end,
	["cons"]     = function (x, xs) return {x, unpack(xs)} end,
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

} do Env.add(builtin, sym, val) end

return builtin
