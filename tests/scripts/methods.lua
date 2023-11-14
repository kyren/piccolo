do
    local t = {}

    function t:add(a)
        self.field = self.field + a
    end

    t.field = 3
    t:add(10)

    assert(t.field == 13)
end

do
    local function local_scope()
        t = {}
        function t:method(a)
            return a
        end
    end
    local_scope()

    assert(t:method(42) == 42)
end

do
    local t = {}

    function t:multi_return()
        return 1, 2, 3
    end

    local a, b, c = t:multi_return()
    assert(a == 1 and b == 2 and c == 3)

    local a, b, c = (function()
        return t:multi_return(), 4, 5
    end)()
    assert(a == 1 and b == 4 and c == 5)

    local a, b, c, d, e = (function()
        return -1, 0, t:multi_return()
    end)()
    assert(a == -1 and b == 0 and c == 1 and d == 2 and e == 3)
end
