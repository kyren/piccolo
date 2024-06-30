
do
    assert(type(_G) == "table")
    assert(_G == _ENV)
    assert(_G._G == _G)

    a = {}
    assert(_G.a == a)

    number = 15
    assert(_G.number == 15)
end

-- _G can be modified
do
    old_g = _G
    _G = nil
    assert(_G == nil)

    b = {}
    assert(_G == nil and old_g.b == b)
    _G = old_g
end
