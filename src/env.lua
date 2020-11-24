local Env = {}

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

local class_mt = {
	__call = function (_, env)
		-- Top-level environment is empty (no globals)
		env = env or {}
		local mt = {
			outer = env,
			__index = env,
			__tostring = env_tostring
		}
		return setmetatable({}, mt)
	end
}

function Env:add(var, val)
	self[var] = val
end

function Env:lookup(var)
	return self[var]
end

function Env:update(var, new_val)
	local env = self
	while env ~= nil do
		local val = rawget(env, var)
		if val ~= nil then
			rawset(env, var, new_val)
			return new_val
		end
		local mt = getmetatable(env)
		if not mt then return nil end
		env = mt.outer
	end
	return nil
end

return setmetatable(Env, class_mt)
