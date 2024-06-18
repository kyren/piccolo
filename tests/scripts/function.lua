do
    local f = nil

    local function f()
        return "yes"
    end

    
    assert(f() == "yes")
end

do
    local f = nil

    local function go()
        function f()
            return "yes"
        end
    end

    go()
    assert(f() == "yes")
end

do
    local x, y
    local function abc(a, b, c)
        x = b
        y = c
    end

    abc({})
    assert(x == nil and y == nil)
end

do
    local x, y
    local function test()
        local function abc(a, b, c)
            x = b
            y = c
        end
        return abc({})
    end

    test()
    assert(x == nil and y == nil)
end
