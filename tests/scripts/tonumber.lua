function is_err(f)
    return pcall(f) == false
end

function roundtrips(orig)
    local str = tostring(orig)
    local num = tonumber(str)
    return num == orig and tostring(num) == str
end

do
    assert(tonumber("3.4e0") == 3.4)
    assert(tonumber("-3.4") == -3.4)
    assert(tonumber("3.4e1") == 34)
    assert(tonumber("3e.1") == nil)
    assert(tonumber("3.") == 3)
    assert(tonumber("   .4") == 0.4)
    assert(tonumber("3e3") == 3000)
    assert(tonumber("3..") == nil)
    assert(tonumber("3ee") == nil)
    assert(tonumber("3f") == nil)
    assert(tonumber("3.4", 10) == nil)
    assert(tonumber("a2", 16) == 162)
    assert(tonumber("+A2", 16) == 162)
    assert(tonumber("Z0", 36) == 36 * 35)
    assert(tonumber("-ff", 16) == -255)
    assert(tonumber("") == nil)
    assert(tonumber("+") == nil)
    assert(tonumber("+42.3") == 42.3)
    assert(tonumber("+42", 10) == 42)
    assert(tonumber("2", 2) == nil)
    assert(tonumber("-") == nil)
    assert(tonumber("1 1") == nil)
    assert(tonumber({}) == nil)
    assert(tonumber(nil) == nil)
    assert(tonumber("ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ", 36) == -1)
    assert(tonumber("-ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZAZZ", 36) == 32401)
    assert(tonumber("-3.51234567e7") == -35123456.7)
    assert(is_err(function() tonumber(3, 4) end))
    assert(is_err(function() tonumber(3., 4) end))
    assert(is_err(function() tonumber(nil, 4) end))

    assert(tonumber("15", nil) == 15)
end

-- Boundary cases
do
    assert(roundtrips(math.maxinteger))
    assert(roundtrips(math.mininteger))

    assert(tonumber("-9223372036854775808") == -9223372036854775807 - 1)
    -- Use integer wrapping to check that it returned an integer
    assert(tonumber("-9223372036854775808") - 1 == math.maxinteger)

    -- Value too large, converted to float
    assert(tostring(tonumber("-9223372036854775809")) ~= "-9223372036854775809")

    assert(tonumber("8000000000000000", 16) == math.mininteger)
    assert(tonumber("-8000000000000000", 16) == math.mininteger)
    -- Use integer wrapping to check that it returned an integer
    assert(tonumber("8000000000000000", 16) - 1 == math.maxinteger)
    assert(tonumber("-8000000000000000", 16) - 1 == math.maxinteger)
end
