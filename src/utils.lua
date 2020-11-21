local utils = {}

-- Applies a function to every element of an array
-- Shares metatable with original array
function utils.map(t, fn)
	local m = {}
	for _, v in ipairs(t) do
		table.insert(m, (fn(v)))
	end
	-- Keep quasiquotation depth if present
	m.depth = t.depth
	return setmetatable(m, getmetatable(t))
end

-- Returns a slice (copy) of an array or subarray
-- Shares metatable with original array
function utils.slice(t, i, j)
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

-- Raises an error with message err_msg
function utils.raise(err_msg)
	local err_mt = {__tostring = function (err) return err.reason end}
	return error(setmetatable({reason = err_msg}, err_mt))
end

return utils
