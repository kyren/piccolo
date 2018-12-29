local function test1()
    local function test1a()
	test1var = 1
    end
    local function test1b()
	return test1var == 1
    end
    test1a()
    return test1b()
end

return test1()