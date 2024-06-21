
do
    assert(1 << 1 == 2)
    assert(9223372036854775807 << 0 == 9223372036854775807)
    assert((1 << 63) - 1 == 9223372036854775807)

    -- Shift right is arithmetic (unsigned)
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
