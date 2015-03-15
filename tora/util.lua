local util = {}

-- Applies a function to every element of an array
-- Shares metatable with original array
function util.map(t, fn)
	local m = {}
	for _, v in ipairs(t) do
		table.insert(m, (fn(v)))
	end
	return setmetatable(m, getmetatable(t))
end

-- Returns a slice (copy) of an array or subarray
-- Shares metatable with original array
function util.slice(t, i, j)
	i = i or 1
	j = j or #t
	if j < 0 then j = j + #t + 1 end
	if i < 1 or j > #t or i > j then
		return setmetatable({}, getmetatable(t))
	end
	local slice = {}
	for k = i, j do
		table.insert(slice, t[k])
	end
	return setmetatable(slice, getmetatable(t))
end

return util
