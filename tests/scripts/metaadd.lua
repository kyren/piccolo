do
	local function add2d(a, b)
		return { x = (a.x + b.x), y = (a.y + b.y) }
	end

	local a = { x = 3, y = 3 }
	local b = { x = 1, y = -4 }

	setmetatable(a, { __add = add2d })
	c = a + b
	assert(c.x == 4)
	assert(c.y == -1)

	setmetatable(a, nil)

	setmetatable(b, { __add = add2d })
	c = a + b
	assert(c.x == 4)
	assert(c.y == -1)
	--TODO negative tests
end
