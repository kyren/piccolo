do
    local f = nil

    function f()
        return "yes"
    end

    
    assert(f() == "yes")
end

do
    local f = nil

    function go()
        function f()
            return "yes"
        end
    end

    go()
    assert(f() == "yes")
end
