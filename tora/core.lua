local builtin = require "builtin"
local Env = require "env"
local util = require "util"
local map, slice = util.map, util.slice
local mt = {__index = util}

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
	["number"]           = "[%+%-]?%d+%.*%d*",
	["string"]           = "\".-\"",
	["boolean"]          = "#[tf]",
	["lparen"]           = "%(",
	["rparen"]           = "%)",
	["quote"]            = "'",
	["quasiquote"]       = "`",
	["unquote"]          = ",[^@]",
	["unquote-splicing"] = ",@",
	["operator"]         = "([%+%-%*/=<>]+) ",
	["symbol"]           = "[_%a][_%-%w]*%p?",
	["lambda"]           = "λ",
}

local function next_token(inp)
	-- Skip whitespace
	inp = inp:gsub("^%s*", "")
	-- Identify next token
	for tok, pat in pairs(TOKENS) do
		local val = inp:match("^"..pat)
		if val ~= nil then
			if tok == "unquote" then val = "," end
			return tok, val, inp:sub(#val+1)
		end
	end
	if #inp == 0 then return "<end>" end
	error("next_token: unknown token\n" .. inp .. "\n^")
end

-- Tokenizer coroutine
local function tokenize(inp)
	return coroutine.wrap(function ()
		-- 1) Strip comments
		inp = inp:gsub(";.-\n", "")
		-- 2) Surround parentheses with spaces
		inp = inp:gsub("[%(%)]", " %0 ")
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

local function parse_quote(quote, tokens, depth)
	local ast = setmetatable({quote}, mt)
	if quote == "quasiquote" then
		depth = (depth or 0) + 1
		ast.depth = depth
	elseif quote:match("unquote") then
		assert(depth > 0, "parse_quote: unquote outside of quasiquote")
		depth = depth - 1
		ast.depth = depth
	end
	local tok, val = tokens()
	if tok == "lparen" then ast[#ast+1] = parse(tokens, depth)
	elseif tok:match("quote") then ast[#ast+1] = parse_quote(tok, tokens, depth)
	else ast[#ast+1] = val end
	return ast
end

local function parse_define(tokens)
	local ast = setmetatable({"define"}, mt)
	local tok, val = tokens()
	-- Allows operators to be redefined!
	if tok == "operator" or tok == "symbol" then
		ast[#ast+1] = val
		return ast
	end
	assert(tok == "lparen", "parse_define: unexpected " .. tok)
	ast[#ast+1] = select(2, tokens())
	local lam = setmetatable({"lambda", parse(tokens)}, mt)
	tok, val = tokens()
	if tok == "lparen" then lam[#lam+1] = parse(tokens)
	elseif tok:match("quote") then lam[#lam+1] = parse_quote(tok, tokens)
	else lam[#lam+1] = val end
	ast[#ast+1] = lam
	return ast
end

-- Parse token stream into AST
parse = function (tokens, depth)
	local ast = setmetatable({}, mt)
	for tok, val in tokens do
		if tok == "lparen" then ast[#ast+1] = parse(tokens, depth)
		elseif tok == "rparen" then return ast
		elseif tok:match("quote") then ast[#ast+1] = parse_quote(tok, tokens, depth)
		elseif tok == "symbol" and val == "define" then
			for _, a in ipairs(parse_define(tokens)) do
				ast[#ast+1] = a
			end
		else -- number, string, boolean, operator, symbol, lambda
			ast[#ast+1] = val
			if tok == "symbol" and val == "quasiquote" then
				depth = (depth or 0) + 1;
				ast.depth = depth
			elseif tok == "symbol" and val:match("unquote") then
				assert(depth > 0, "parse: unquote outside of quasiquote")
				depth = depth - 1
				ast.depth = depth
			end
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

-- Evaluate expression exp if it appears unquoted at the same nesting level as
-- the outermost quasiquote
local function eval_unquote(exp, env)
	if type(exp) ~= "table" then return exp end
	if exp[1] == "unquote" and exp.depth == 0 then
		-- unquote is limited to one argument
		return eval(exp[2], env)
	elseif exp[1] == "unquote-splicing" and exp.depth == 0 then
		-- Evaluate now, splice later
		return builtin.cons("@", eval(exp[2], env))
	end
	return exp:map(function (exp) return eval_unquote(exp, env) end)
end

local function splice(l)
	if type(l) ~= "table" then return l end
	local s = setmetatable({}, getmetatable(l))
	for i = 1, #l do
		assert(l[i] ~= "@", "splice: no list to splice into")
		if type(l[i]) == "table" and l[i][1] == "@" then
			for j = 2, #l[i] do
				s[#s+1] = splice(l[i][j])
			end
		else
			s[#s+1] = splice(l[i])
		end
	end
	return s
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
	elseif x[1] == "quasiquote" then
		return splice(eval_unquote(x[2], env))
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
