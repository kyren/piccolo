do
    local function test_recurse(i)
        if i > 0 then
            return test_recurse(i - 1)
        else
            return 12
        end
    end

    assert(test_recurse(1000) == 12)
end
