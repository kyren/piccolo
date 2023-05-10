function test1()
    local t = {}

    function t:add(a)
        self.field = self.field + a
    end

    t.field = 3
    t:add(10)

    return t.field == 13
end

function test2()
    local function local_scope()
        t = {}
        function t:method(a)
            return a
        end
    end
    local_scope()

    return t:method(42) == 42
end

assert(
    test1() and
    test2()
)
