
do
    assert(1 << 1 == 2)
    assert(9223372036854775807 << 0 == 9223372036854775807)
    assert((1 << 63) - 1 == 9223372036854775807)

    -- Shift right is logical (unsigned)
    assert(-1 >> 1 == 9223372036854775807)
end

do
    -- Check overflow cases
    assert(1 << 64 == 0)
    assert(-1 >> 64 == 0)

    assert(1 << (1 << 32) == 0)
    assert(1 >> (1 << 32) == 0)
end

do
    assert(0 ~= 1 + 1)
end

-- Impl bugs caught by PUC-Rio Lua's test suite:
do
    local i, j = -16, 3
    assert(i // j == math.floor(i / j))
end

do
    local two_27 = 134217729
    local two_52 = 4503599627370496
    local two_53 = 9007199254740992
    local two_54_plus_2 = 18014398509481986
    assert(two_53 + "1" ~= two_53)
    assert(two_53 + "1" == two_53 + 1)
    assert(two_53 + 2 - "1" ~= two_53)
    assert(two_53 + 2 - "1" == two_53 + 2 - 1)
    assert(two_27 * "134217729" ~= 18014398777917440)
    assert(two_27 * "134217729" == two_27 * two_27)
    assert(two_54_plus_2 // "2" ~= two_53)
    assert(two_54_plus_2 // "2" == two_54_plus_2 // 2)
    assert(two_54_plus_2 % "5" == 1)
end
