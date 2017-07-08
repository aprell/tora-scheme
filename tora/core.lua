local builtin = require "tora.builtin"
local Env = require "tora.env"
local util = require "tora.util"
local map, slice, raise = util.map, util.slice, util.raise
local show = builtin.core.show

local lpeg = require "lpeg"
local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Cc, Ct = lpeg.C, lpeg.Cc, lpeg.Ct

local expand_macro

local function parse()
	local alpha = R ("AZ", "az")
	local num = R "09"
	local alphanum = alpha + num
	local space = S " \t\n"
	local function skip(tok) return space ^ 0 * tok * space ^ 0 end

	local depth = 0
	local function inc() depth = depth + 1; return depth end
	local function dec() depth = depth - 1; return depth end

	local mt = {__index = util}
	local function mk(ast) return setmetatable(ast, mt) end

	local number = C (S "+-" ^ -1 * num ^ 1 * (P "." * num ^ 0) ^ -1) / tonumber
	local string = C (P '"' * (1 - P '"') ^ 0 * P '"')
	local boolean = C (P "#" * S "tf") / function (tok) return tok == "#t" end
	local operator = C (S "+-*/=" + S "<>" * P "=" ^ -1)
	local ident = C ((P "_" + alpha) ^ 1 * (S "_->/" + alphanum) ^ 0 * S "?!=" ^ -1)
	local lambda = C (P "λ")
	local symbol = ident + lambda

	return P { "program",
		program = Ct ((V "sexpr" + V "comment") ^ 0) / mk,
		sexpr = V "atom" + V "list" + V "quote" + V "quasiquote" + V "unquote",
		atom = number + string + boolean + operator + symbol,
		list = Ct (skip "(" * (V "sexpr" * space ^ 0 + V "comment") ^ 0 * skip ")") /
			function (ast)
				if ast[1] == "define" or ast[1] == "define-macro" then
					if type(ast[2]) == "table" then
						-- Desugar: (define (f a b) (...))
						----------> (define f (lambda (a b) (...)))
						local lambda = mk {"lambda", ast[2], ast[3]}
						ast[2] = table.remove(ast[2], 1)
						ast[3] = lambda
					end
				elseif ast[1] == "lambda" or ast[1] == "λ" then
					if type(ast[2]) ~= "table" then
						raise "parse: lambda expects a parameter list"
					end
				end
				return mk(ast)
			end,
		quote = Ct (skip "'" * Cc "quote" * V "sexpr") / mk,
		quasiquote = Ct (skip "`" * Cc "quasiquote" *
			(Cc () / inc) * V "sexpr") /
			function (ast)
				dec(); ast.depth = table.remove(ast, 2); return mk(ast)
			end,
		unquote = Ct ((skip ("," - P ",@") * Cc "unquote" +
			skip ",@" * Cc "unquote-splicing") *
			(Cc () / dec) * V "sexpr") /
			function (ast)
				if depth < 0 then raise "parse: unquote outside of quasiquote" end
				inc(); ast.depth = table.remove(ast, 2); return mk(ast)
			end,
		comment = skip ";" * (1 - P "\n") ^ 0,
	} / function (ast) return ast:map(expand_macro) end
end

local function read(inp)
	return unpack(parse():match(inp))
end

local function symbol(x)
	return type(x) == "string" and not x:find("^\"")
end

-- Macros live in a separate environment
local macro = Env.new()

local function macrocall(x)
	return symbol(x) and Env.lookup(macro, x) ~= nil
end

-- Recursively expand macros in ast
expand_macro = function (ast)
	if type(ast) ~= "table" then return ast end
	if macrocall(ast[1]) then
		local macro, args = Env.lookup(macro, ast[1]), slice(ast, 2)
		return expand_macro(macro(unpack(expand_macro(args))))
	else
		return ast:map(expand_macro)
	end
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
	if exp[1] == "unquote" then
		assert(exp.depth ~= nil, "eval_unquote: missing depth")
	end
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
	-- Keep quasiquotation depth if present
	s.depth = l.depth
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
	elseif x[1] == "define" or x[1] == "define-macro" then
		local var, val = x[2], eval(x[3], env)
		if x[1] == "define" then
			if type(val) == "string" and val:match("^\"lua%s*%b()") then
				val = load("return function" .. lua_string(val):gsub("^lua", ""))()
			end
			Env.add(env, var, val)
			return var .. ": " .. show(val)
		else -- define-macro
			Env.add(macro, var, val)
			return var .. ": " .. show(val):gsub("function", "macro")
		end
	elseif x[1] == "set!" then
		local var, val = x[2], eval(x[3], env)
		if Env.update(env, var, val) == nil then
			raise("eval: set! of undefined variable " ..
			      string.format("'%s'", show(var)))
		end
		return var .. ": " .. show(val)
	elseif x[1] == "cond" then
		for i = 2, #x do
			local test_x, then_x = x[i][1], x[i][2]
			if test_x == "else" and i ~= #x then
				raise("eval: else must be last cond-clause")
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
			if #args ~= #params then
				raise("eval: number of arguments doesn't match number of formal parameters")
			end
			-- 1) Bind parameters to values
			-- Note that arguments behave like local variables
			for i = 1, #args do
				if (not symbol(params[i])) then
					raise(string.format("eval: cannot bind %s to '%s'",
					      show(args[i]), show(params[i])))
				end
				Env.add(scope, params[i], args[i])
			end
			-- 2) Evaluate body of lambda in new scope
			return eval(body, scope)
		end
	else -- Treat as function call
		if macrocall(x[1]) then
			raise("eval: unexpanded macro " ..
			      string.format("'%s'", show(x[1])))
		end
		-- 1) Evaluate function
		local fn = eval(x[1], env)
		if fn == nil then
			raise("eval: undefined function " ..
			      string.format("'%s'", show(x[1])))
		elseif type(fn) ~= "function" then
			raise("eval: attempt to call " ..
			      string.format("'%s' (a %s value)", show(x[1]), type(x[1])))
		end
		-- 2) Evaluate arguments
		local args = slice(x, 2):map(function (exp) return eval(exp, env) end)
		-- 3) Apply function to arguments
		return fn(unpack(args))
	end
end

local function interpret(filename, env)
	local file = assert(io.open(filename))
	local inp = file:read("*all")
	if #inp > 0 then
		local code = parse():match(inp)
		local ok, err = pcall(function ()
			code:map(function (exp) eval(exp, env or builtin) end)
		end)
		if not ok then
			print(err)
			os.exit(1)
		end
	end
	file:close()
	return filename:gsub("%.[^%.]*$", "") .. " loaded"
end

for sym, val in pairs {

	["read"]  = function (inp) return read(lua_string(inp)) end,
	["eval"]  = eval,
	["print"] = function (exp) io.write(show(exp), "\n") end,
	["load"]  = function (fln) return interpret(lua_string(fln)) end,

} do Env.add(builtin, sym, val) end

return {
	read = read,
	eval = eval,
	println = builtin.print,
	interpret = interpret,
	builtin = builtin
}
