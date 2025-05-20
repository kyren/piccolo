
local function dprint(...)
    print(string.format("%q", ...))
end

local function assert_eq(val, exp)
    -- dprint(val)
    if val ~= exp then
        error(string.format("assertion failed; expected %q but found %q", exp, val))
    end
end


do
    -- A subnormal float with 1 more bit of precision than is representable
    -- TODO: The lexer currently incorrectly parses this as 0
    local mismatch, alt = 0x1.fffffffffffffp-1023, 2.2250738585072013e-308
    assert_eq(string.format("%a %g", mismatch, mismatch), "0x1p-1022 2.22507e-308")
    assert_eq(string.format("%a %g", alt, alt), "0x1p-1022 2.22507e-308")
end

do
    -- Fixing this requires rewriting the %f case of format_float to
    -- either not use Rust's float formatter, or to have it write into
    -- a (large!) intermediate buffer, and then edit parts of the
    -- generated float.
    assert_eq(string.format("'%#.0f'", 1), "'1.'")
end
