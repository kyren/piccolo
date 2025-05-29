
local function with_len(t, len)
    return setmetatable(t, { __len = function() return len end })
end

do
    -- table.insert overflow behavior
    local res, val

    res, val = pcall(function()
        local t = with_len({}, math.maxinteger)
        table.insert(t, true)
        return t
    end)

    -- PRLua wraps the index and inserts at math.mininteger;
    -- piccolo throws an error instead
    if _VERSION == "piccolo" then
        assert(not res, "insert at maxinteger + 1 wrapped instead of erroring")
    else
        assert(
            res and val[math.mininteger] == true,
            "expected PRLua behavior for insert at maxinteger + 1"
        )
    end

    res, val = pcall(function()
        local t = with_len({ 1, 2, 3 }, math.maxinteger)
        table.insert(t, 2, true)
        return t
    end)
    -- PRLua wraps the end index and silently doesn't shift interior elements
    -- piccolo throws an error instead
    if _VERSION == "piccolo" then
        assert(not res, "insert with length maxinteger wrapped instead of erroring")
    else
        assert(
            res and val[1] == 1 and val[2] == true and val[3] == 3 and val[4] == nil,
            "expected PRLua behavior for insert in table with length maxinteger"
        )
    end

    res, _ = pcall(function()
        local t = with_len({}, math.maxinteger - 1)
        table.insert(t, true)
    end)
    assert(res, "implicit insert at maxinteger failed")

    res, _ = pcall(function()
        local t = with_len({}, math.maxinteger - 1)
        table.insert(t, math.maxinteger, true)
    end)
    assert(res, "insert at maxinteger failed")
end

do
    -- table.remove overflow behavior
    do
        local t = with_len({ [math.maxinteger] = 15 }, math.maxinteger)
        local v = table.remove(t)
        assert(v == 15)
    end

    do
        local t = with_len({ }, math.maxinteger - 1)
        local v = table.remove(t, math.maxinteger)
        assert(v == nil)
    end

    if _VERSION == "piccolo" then
        -- This hangs PRLua, but should always error (the index is negative...)
        local res, _ = pcall(function()
            local t = with_len({ [math.maxinteger] = 15 }, math.maxinteger)
            local v = table.remove(t, math.mininteger)
            return v
        end)
        assert(not res)
    end
end
