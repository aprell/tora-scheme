local builtin = require "builtin"
local Env = require "env"
local util = require "util"
local map, slice = util.map, util.slice

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

local TOKENS = {
	number   = "[%+%-]?%d+%.*%d*",
	string   = "\".-\"",
	boolean  = "#[tf]",
	lparen   = "%(",
	rparen   = "%)",
	quote    = "'",
	operator = "([%+%-%*/=<>]+) ",
	symbol   = "[_%a][_%-%w]*%p?",
	lambda   = "λ",
}

local function next_token(inp)
	-- Skip whitespace
	inp = inp:gsub("^%s*", "")
	-- Identify next token
	for tok, pat in pairs(TOKENS) do
		local val = inp:match("^"..pat)
		if val ~= nil then
			return tok, val, inp:sub(#val+1)
		end
	end
	if #inp == 0 then return "<end>" end
	error("next_token: unknown token")
end

-- Tokenizer coroutine
local function tokenize(inp)
	return coroutine.wrap(function ()
		-- 1) Strip comments
		inp = inp:gsub(";.-\n", "")
		-- 2) Surround parentheses and quotations with spaces
		inp = inp:gsub("[%(%)']", " %0 ")
		-- 3) Get tokens
		local tok, val, rest = next_token(inp)
		while tok ~= "<end>" do
			if tok == "number" or tok == "boolean" then
				val = tonumber(val) or val == "#t"
			end
			coroutine.yield(tok, val)
			tok, val, rest = next_token(rest)
		end
	end)
end

local parse

local function parse_quote(tokens)
	local ast = {"quote"}
	-- Get next token
	local tok, val = tokens()
	if tok == "operator" or tok == "symbol" then
		ast[#ast+1] = val
		return ast
	end
	assert(tok == "lparen")
	ast[#ast+1] = parse(tokens)
	return ast
end

local function parse_define(tokens)
	local ast = {"define"}
	local tok, val = tokens()
	-- Allows operators to be redefined!
	if tok == "operator" or tok == "symbol" then
		ast[#ast+1] = val
		return ast
	end
	assert(tok == "lparen")
	ast[#ast+1] = select(2, tokens())
	local lam = {"lambda", parse(tokens)}
	tok, val = tokens()
	if tok == "lparen" then lam[#lam+1] = parse(tokens)
	elseif tok == "quote" then lam[#lam+1] = parse_quote(tokens)
	else lam[#lam+1] = val end
	ast[#ast+1] = lam
	return ast
end

-- Parse token stream into AST
parse = function (tokens)
	local ast = {}
	setmetatable(ast, {__index = util})
	for tok, val in tokens do
		if tok == "lparen" then ast[#ast+1] = parse(tokens)
		elseif tok == "rparen" then return ast
		elseif tok == "quote" then ast[#ast+1] = parse_quote(tokens)
		elseif tok == "symbol" and val == "define" then
			for _, a in ipairs(parse_define(tokens)) do
				ast[#ast+1] = a
			end
		else -- number, string, boolean, operator, symbol, lambda
			ast[#ast+1] = val
		end
	end
	return ast
end

local function read(inp)
	return unpack(parse(tokenize(inp)))
end

local function symbol(x)
	return type(x) == "string" and not x:find("^\"")
end

local eval_atom, eval_list

-- Evaluate parsed expression exp in environment env
local function eval(exp, env)
	env = env or builtin
	if type(exp) == "table" then
		return eval_list(exp, env)
	else
		return eval_atom(exp, env)
	end
end

-- Evaluate atom a in environment env
eval_atom = function (a, env)
	if not symbol(a) then return a end
	return Env.lookup(env, a)
end

-- Evaluate list x in environment env
-- Handles special forms as well as function calls
eval_list = function (x, env)
	if x[1] == "quote" then
		return x[2]
	elseif x[1] == "define" then
		local var, val = x[2], eval(x[3], env)
		Env.add(env, var, val)
		return var .. ": " .. core_tostring(val)
	elseif x[1] == "set!" then
		local var, val = x[2], eval(x[3], env)
		if not Env.update(env, var, val) then
			error("set! of undefined variable " .. var)
		end
		return var .. ": " .. core_tostring(val)
	elseif x[1] == "cond" then
		for i = 2, #x do
			local test_x, then_x = x[i][1], x[i][2]
			if test_x == "else" and i ~= #x then
				error("else must be the last cond-clause")
			end
			if eval(test_x, env) == true then
				return eval(then_x, env)
			end
		end
		return nil
	elseif x[1] == "if" then
		local test_x, then_x, else_x = x[2], x[3], x[4]
		if eval(test_x, env) == true then
			return eval(then_x, env)
		else
			return else_x and eval(else_x, env)
		end
	elseif x[1] == "begin" then
		-- 1) Evaluate all expressions in sequence
		x = slice(x, 2):map(function (exp) return eval(exp, env) end)
		-- 2) Return value of last expression
		return x[#x]
	elseif x[1] == "let" or x[1] == "letrec" then
		local binds, body = x[2], x[3]
		local scope = Env.new(env)
		for i = 1, #binds do
			local id, exp = binds[i][1], binds[i][2]
			if x[1] == "let" then
				Env.add(scope, id, eval(exp, env))
			else -- letrec
				Env.add(scope, id, eval(exp, scope))
			end
		end
		return eval(body, scope)
	elseif x[1] == "lambda" or x[1] == "λ" then
		return function (...)
			local params, body = x[2], x[3]
			local args = {...}
			local scope = Env.new(env)
			assert(type(params) == "table")
			if #args ~= #params then
				error("Number of arguments doesn't match number of formal parameters")
			end
			-- 1) Bind parameters to values
			-- Note that arguments behave like local variables
			for i = 1, #args do
				assert(symbol(params[i]))
				Env.add(scope, params[i], args[i])
			end
			-- 2) Evaluate body of lambda in new scope
			return eval(body, scope)
		end
	else -- Treat as function call
		-- 1) Evaluate arguments
		x = x:map(function (exp) return eval(exp, env) end)
		-- 2) Apply function to arguments
		local fn, args = x[1], slice(x, 2)
		return fn(unpack(args))
	end
end

local function interpret(filename, env)
	local file = assert(io.open(filename))
	local inp = file:read("*all")
	if #inp > 0 then
		local code = parse(tokenize(inp))
		code:map(function (exp) eval(exp, env or builtin) end)
	end
	file:close()
	return filename:gsub("%.[^%.]*$", "") .. " loaded"
end

for sym, val in pairs {

	["read"]  = function (inp) return read(inp:sub(2, -2)) end,
	["eval"]  = eval,
	["print"] = function (exp) io.write(core_tostring(exp), "\n") end,
	["load"]  = function (fln) return interpret(fln:sub(2, -2)) end,

} do Env.add(builtin, sym, val) end

return {
	read = read,
	eval = eval,
	println = builtin.print,
	interpret = interpret,
	builtin = builtin
}
