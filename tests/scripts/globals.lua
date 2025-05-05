
do
    assert(type(_G) == "table")

    assert(_G._G == _G)

    a = {}
    assert(_G.a == a)

    number = 15
    assert(_G.number == 15)
end

-- Load uses the global context
do
    local res = load("return _G")()
    assert(res == _G)
end

-- global context is used, even if _G is modified
-- (_G is not used internally)
do
    old_G = _G
    _G = nil
    local res = load("return old_G")()
    assert(res == old_G)
    _G = old_G
end

-- _G can be modified
do
    _G = nil
    assert(_G == nil)
end
