do
	local function sub2d(a, b)
		return { x = (a.x - b.x), y = (a.y - b.y) }
	end

	local a = { x = 3, y = 3 }
	local b = { x = 1, y = -4 }

	setmetatable(a, { __sub = sub2d })
	c = a - b
	assert(c.x == 2)
	assert(c.y == 7)

	setmetatable(a, nil)

	setmetatable(b, { __sub = sub2d })
	c = a - b
	assert(c.x == 2)
	assert(c.y == 7)
	--TODO negative tests
end
