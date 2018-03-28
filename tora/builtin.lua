local Env = require "tora.env"
local util = require "tora.util"
local map, slice, raise = util.map, util.slice, util.raise
local unpack = unpack or table.unpack
local mt = {__index = util}

function tora_string(s)
	return ("%q"):format(s)
end

function lua_string(s)
	return s:sub(2, -2)
end

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

local function show(a)
	if type(a) == "table" then
		return "(" .. table.concat(map(a, show), " ") .. ")"
	elseif type(a) == "string" and a:match("^\"") then
		return lua_string(a)
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

	["show"]     = function (a) return tora_string(show(a)) end,

	["string-append"] = function (...)
		return tora_string(table.concat(map({...}, show)))
	end,

	["error"]    = function (msg) raise(lua_string(msg)) end,

} do Env.add(builtin, sym, val) end

builtin.core = {
	show = show
}

return builtin
