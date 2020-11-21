local env = {}

local function env_tostring(env)
	local t = {}
	for n in pairs(env) do t[#t+1] = n end
	table.sort(t)
	local s = "{\n"
	for i = 1, #t do
		s = s .. "  " .. t[i] .. " = " .. tostring(env[t[i]]) .. "\n"
	end
	s = s .. "}"
	return s
end

function env.new(outer)
	-- Top-level environment is empty (no globals)
	outer = outer or {}
	return setmetatable({}, {
		outer = outer,
		__index = outer,
		__tostring = env_tostring
	})
end

function env.add(env, var, val)
	env[var] = val
end

function env.update(env, var, val)
	while env ~= nil do
		local v = rawget(env, var)
		if v ~= nil then rawset(env, var, val); return val end
		local mt = getmetatable(env)
		if not mt then return nil end
		env = mt.outer
	end
	return nil
end

function env.lookup(env, var)
	return env[var]
end

if not debug.getinfo(4) then
	e1 = env.new()
	env.add(e1, "x", 1)
	env.add(e1, "y", 2)
	e2 = env.new(e1)
	env.add(e2, "z", 3)
	e3 = env.new(e2)
	env.add(e3, "x", 10)
	env.add(e3, "z", 42)

	print(env.lookup(e1, "x"))
	print(env.lookup(e1, "y"))
	print(env.lookup(e1, "z"))
	print(env.lookup(e2, "x"))
	print(env.lookup(e2, "y"))
	print(env.lookup(e2, "z"))
	print(env.lookup(e3, "x"))
	print(env.lookup(e3, "y"))
	print(env.lookup(e3, "z"))
else
	return env
end
