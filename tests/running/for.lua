function test_iterator()
    local function inc(s, c)
        if c == 10 then
            return nil
        else
            return c + 1, c + 11
        end
    end
    return inc, nil, 0
end

local sum = 0
for i in test_iterator() do
    sum = sum + i
end

return sum == 55
