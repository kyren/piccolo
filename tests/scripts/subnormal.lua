
local function dprint(...)
    print(string.format("%q", ...))
end

local function assert_eq(val, exp)
    -- dprint(val)
    if val ~= exp then
        error(string.format("assertion failed; expected %q but found %q", exp, val))
    end
end

local a = 0x1.fffffffffffffp-1023
assert_eq(string.format("%a", a, a), "0x1p-1022")
local b = 0x1.ffffffffffffep-1023
assert_eq(string.format("%a", b, b), "0x0.fffffffffffffp-1022")
print("a")
local c = 0x1.ffffffffffffdp-1023
assert_eq(string.format("%a", c, c), "0x0.ffffffffffffep-1022")
print("b")
local d = 0x1.ffffffffffffcp-1023
assert_eq(string.format("%a", d, d), "0x0.ffffffffffffep-1022")
local d = 0x1.ffffffffffff0p-1026
assert_eq(string.format("%a", d, d), "0x0.1ffffffffffffp-1022")
local d = 0x1.ffffffffffff7p-1026
assert_eq(string.format("%a", d, d), "0x0.1ffffffffffffp-1022")
local d = 0x1.ffffffffffff8p-1026
assert_eq(string.format("%a", d, d), "0x0.2p-1022")


local a = -0x1.fffffffffffffp-1023
assert_eq(string.format("%a", a, a), "-0x1p-1022")
local b = -0x1.ffffffffffffep-1023
assert_eq(string.format("%a", b, b), "-0x0.fffffffffffffp-1022")
local c = -0x1.ffffffffffffdp-1023
assert_eq(string.format("%a", c, c), "-0x0.ffffffffffffep-1022")
local d = -0x1.ffffffffffffcp-1023
assert_eq(string.format("%a", d, d), "-0x0.ffffffffffffep-1022")

local mismatch, alt = 0x1.fffffffffffffp-1023, 2.2250738585072013e-308
assert_eq(string.format("%a %g", mismatch, mismatch), "0x1p-1022 2.22507e-308")
assert_eq(string.format("%a %g", alt, alt), "0x1p-1022 2.22507e-308")

local a, b = 0x1.ffffffffffffep-1023, 2.22507385850720088902e-308
assert_eq(string.format("%a %g", a, a), "0x0.fffffffffffffp-1022 2.22507e-308")
assert_eq(string.format("%a %g", b, b), "0x0.fffffffffffffp-1022 2.22507e-308")
