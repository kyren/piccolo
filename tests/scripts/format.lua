
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
    assert_eq(string.format("abc%sdef", "iii"), "abciiidef")

    assert_eq(string.format("%s__", 123), "123__")
    assert_eq(string.format("__%s", 321), "__321")

    assert_eq(string.format("%d %d %d", 1, 2, 3), "1 2 3")
end


do -- string width, truncating
    assert_eq(string.format("%s", "example"), "example")
    assert_eq(string.format("%5s", "a"), "    a")
    assert_eq(string.format("%-5s", "a"), "a    ")
    assert_eq(string.format("%-s", "example"), "example")

    assert_eq(string.format("%.3s", "example"), "exa")
    assert_eq(string.format("%.s", "example"), "")
    assert_eq(string.format("%5.2s", "example"), "   ex")
    assert_eq(string.format("%-5.2s", "example"), "ex   ")
    assert_eq(string.format("%2.s", "example"), "  ")

    assert_eq(string.format("%.10s", "example"), "example")
    assert_eq(string.format("%10.10s", "example"), "   example")
    assert_eq(string.format("%10.3s", "example"), "       exa")

    assert_eq(string.format("%4s", ""), "    ")
    assert_eq(string.format("%4.2s", ""), "    ")
    assert_eq(string.format("%4.10s", ""), "    ")

    assert_eq(string.format("'%8d'", -3), "'      -3'")
    assert_eq(string.format("'%8.4d'", 3), "'    0003'")
    assert_eq(string.format("'%8.4d'", -3), "'   -0003'")
    assert_eq(string.format("'%+8.4d'", 3), "'   +0003'")
    assert_eq(string.format("'% 8.4d'", 3), "'    0003'")
    assert_eq(string.format("'%08.4d'", 3), "'    0003'")
    assert_eq(string.format("'%+08.4d'", 3), "'   +0003'")
    assert_eq(string.format("'%-08.4d'", 3), "'0003    '")
    assert_eq(string.format("'%-+08.4d'", 3), "'+0003   '")
    assert_eq(string.format("'%+08.4d'", 123456789), "'+123456789'")

    assert_eq(string.format("'% 1.4d'", 1234), "' 1234'")
    assert_eq(string.format("'% 5.4d'", 1234), "' 1234'")

    -- '+' takes precedence over ' '
    assert_eq(string.format("'% +d'", 1), "'+1'")
    assert_eq(string.format("'%+ d'", 1), "'+1'")
    assert_eq(string.format("'%+ d'", -1), "'-1'")

    assert_eq(string.format("'%+08d'", 3), "'+0000003'")
    assert_eq(string.format("'% 08d'", 3), "' 0000003'")
    assert_eq(string.format("'%08d'", 3), "'00000003'")
    assert_eq(string.format("'%+08d'", -3), "'-0000003'")
    assert_eq(string.format("'% 08d'", -3), "'-0000003'")
    assert_eq(string.format("'%08d'", -3), "'-0000003'")

    -- dprint("unsigned")

    assert_eq(string.format("'%8.4u'", 3), "'    0003'")
    assert_eq(string.format("'%8.4u'", -3), "'18446744073709551613'")
    assert_eq(string.format("'%08.4u'", 3), "'    0003'")
    assert_eq(string.format("'%-08.4u'", 3), "'0003    '")

    assert_eq(string.format("'%1.4u'", 1234), "'1234'")
    assert_eq(string.format("'%5.4u'", 1234), "' 1234'")

    assert_eq(string.format("'%08u'", 3), "'00000003'")
    assert_eq(string.format("'%08u'", -3), "'18446744073709551613'")
    assert_eq(string.format("'%8u'", 3), "'       3'")

    assert_eq(string.format("'%-8u'", 3), "'3       '")
    assert_eq(string.format("'%-08u'", 3), "'3       '")

    assert_eq(string.format("%08d",  1), "00000001")
    assert_eq(string.format("%08d", -1), "-0000001")
    assert_eq(string.format("%.8d",  1), "00000001")
    assert_eq(string.format("%.8d", -1), "-00000001")

    assert_eq(string.format("%+08d",  1), "+0000001")
    assert_eq(string.format("%+08d", -1), "-0000001")
    assert_eq(string.format("%+.8d",  1), "+00000001")
    assert_eq(string.format("%+.8d", -1), "-00000001")

    assert_eq(string.format("%#16.8x", 235678), "      0x0003989e")


    assert_eq(string.format("%g", 0), "0")
    assert_eq(string.format("%g", -0.0), "-0")
    assert_eq(string.format("%+g", 0), "+0")
    assert_eq(string.format("% g", 0), " 0")

    assert_eq(string.format("%g", 1), "1")
    assert_eq(string.format("%g", -1), "-1")
    assert_eq(string.format("%+g", 1), "+1")
    assert_eq(string.format("% g", 1), " 1")

    assert_eq(string.format("%g", 1.500001), "1.5")
    assert_eq(string.format("%g", 1.50001), "1.50001")
    assert_eq(string.format("%.1g", 1.5), "2")
    assert_eq(string.format("%g", 1000), "1000")
    assert_eq(string.format("%g", 100000), "100000")
    assert_eq(string.format("%g", 1000000), "1e+06")

    assert_eq(string.format("%8g", 1), "       1")
    assert_eq(string.format("%8g", 1.500001), "     1.5")
    assert_eq(string.format("%8g", 1.50001), " 1.50001")
    assert_eq(string.format("%8.1g", 1.5), "       2")
    assert_eq(string.format("%8g", 1000), "    1000")
    assert_eq(string.format("%8g", 100000), "  100000")
    assert_eq(string.format("%8g", 1000000), "   1e+06")
    assert_eq(string.format("%8G", 1000000), "   1E+06")
    assert_eq(string.format("%8e", 1000000), "1.000000e+06")
    assert_eq(string.format("%8E", 1000000), "1.000000E+06")

    assert_eq(string.format("%g", 0/0), "-nan")
    assert_eq(string.format("%g", math.abs(0/0)), "nan")
    assert_eq(string.format("%g", 1/0), "inf")
    assert_eq(string.format("%g", -1/0), "-inf")
    assert_eq(string.format("%G", 0/0), "-NAN")
    assert_eq(string.format("%G", math.abs(0/0)), "NAN")
    assert_eq(string.format("%G", 1/0), "INF")
    assert_eq(string.format("%G", -1/0), "-INF")

    assert_eq(string.format("%05g", 0/0), " -nan")
    assert_eq(string.format("%05g", math.abs(0/0)), "  nan")
    assert_eq(string.format("%05g", 1/0), "  inf")
    assert_eq(string.format("%05g", -1/0), " -inf")

    assert_eq(string.format("%05.8g", 0/0), " -nan")
    assert_eq(string.format("%05.8g", math.abs(0/0)), "  nan")
    assert_eq(string.format("%05.8g", 1/0), "  inf")
    assert_eq(string.format("%05.8g", -1/0), " -inf")

    assert_eq(#string.format("%099.99f", 1.7976931348623158e308), 409)

    assert_eq(string.format("%0.0f", 15.1234), "15")
    assert_eq(string.format("%4.0f", 15.1234), "  15")
    assert_eq(string.format("'%-8.3f'", 15.1234), "'15.123  '")
    assert_eq(string.format("'%-+8.3f'", 15.1234), "'+15.123 '")
    assert_eq(string.format("'%0+8.3f'", 15.1234), "'+015.123'")

    -- assert_eq(string.format("'%#.0f'", 1), "'1.'") -- Can't easily fix this, relying on rust's std
    assert_eq(string.format("'%.0f'", 1), "'1'")
    assert_eq(string.format("'%g'", 1), "'1'")
    assert_eq(string.format("'%#g'", 1), "'1.00000'")
    assert_eq(string.format("'%#g'", 100000), "'100000.'")

    assert_eq(string.format("'%.0e'", 1), "'1e+00'")
    assert_eq(string.format("'%#.0e'", 1), "'1.e+00'")

    -- turns out PRLua doesn't support %F, even though it has different output (for nan/inf)
    assert_eq(string.format("'%f'", 1.0/0.0), "'inf'")
    assert_eq(string.format("'%e' '%E'", 1.0/0.0, 1.0/0.0), "'inf' 'INF'")
    assert_eq(string.format("'%g' '%G'", 1.0/0.0, 1.0/0.0), "'inf' 'INF'")

    assert_eq(string.format("'%8.4x'", 0xAB), "'    00ab'")
    assert_eq(string.format("'%#8.4x'", 0xAB), "'  0x00ab'")
    assert_eq(string.format("'%08.4x'", 0xAB), "'    00ab'")
    assert_eq(string.format("'%#08.4x'", 0xAB), "'  0x00ab'")
    assert_eq(string.format("'%08x'", 0xAB), "'000000ab'")
    assert_eq(string.format("'%#08x'", 0xAB), "'0x0000ab'")
    assert_eq(string.format("'%08.x'", 0xAB), "'      ab'")
    assert_eq(string.format("'%#08.x'", 0xAB), "'    0xab'")

    -- From glibc's manual:
    -- https://www.gnu.org/software/libc/manual/html_node/Floating_002dPoint-Conversions.html
    expected = {
        "|  0x0.0000p+0|       0.0000|   0.0000e+00|            0|",
        "|  0x1.0000p-1|       0.5000|   5.0000e-01|          0.5|",
        "|  0x1.0000p+0|       1.0000|   1.0000e+00|            1|",
        "| -0x1.0000p+0|      -1.0000|  -1.0000e+00|           -1|",
        "|  0x1.9000p+6|     100.0000|   1.0000e+02|          100|",
        "|  0x1.f400p+9|    1000.0000|   1.0000e+03|         1000|",
        "| 0x1.3880p+13|   10000.0000|   1.0000e+04|        1e+04|",
        "| 0x1.81c8p+13|   12345.0000|   1.2345e+04|    1.234e+04|",
        "| 0x1.86a0p+16|  100000.0000|   1.0000e+05|        1e+05|",
        "| 0x1.e240p+16|  123456.0000|   1.2346e+05|    1.235e+05|",
    }
    for i, v in ipairs({ 0, 0.5, 1, -1, 100, 1000, 10000, 12345, 1e5, 123456 }) do
        assert_eq(string.format("|%13.4a|%13.4f|%13.4e|%13.4g|", v, v, v, v), expected[i])
    end

    assert_eq(string.format("%13.4a", 0.0/0.0), "         -nan")
    assert_eq(string.format("%13.4a", 1.0/0.0), "          inf")
    assert_eq(string.format("%13.4a", math.pi), "  0x1.9220p+1")
    assert_eq(string.format("%13.4a", math.huge), "          inf")

    assert_eq(string.format("%13.4a", 0x1.00008p+0), "  0x1.0000p+0")
    assert_eq(string.format("%13.4a", 0x1.000081p+0), "  0x1.0001p+0")
    assert_eq(string.format("%13.4a", 0x1.00007fp+0), "  0x1.0000p+0")

    -- Round to even
    assert_eq(string.format("%13.4a", 0x1.00018p+0), "  0x1.0002p+0")
    assert_eq(string.format("%13.4a", 0x1.000181p+0), "  0x1.0002p+0")
    assert_eq(string.format("%13.4a", 0x1.00017fp+0), "  0x1.0001p+0")

    assert_eq(string.format("%13a", math.pi), "0x1.921fb54442d18p+1")
    assert_eq(string.format("%013.1a", math.pi), "0x000001.9p+1")
    assert_eq(string.format("%013.0a", math.pi), "0x00000002p+1")

    assert_eq(string.format("%#013.0a", 4.0), "0x0000001.p+2")
    assert_eq(string.format("%#013.0a", math.pi), "0x0000002.p+1")

    assert_eq(string.format("%+#15.8g", 0x1.0p+64), " +1.8446744e+19")
    assert_eq(string.format("%+#15.8g", 0x1.0p-64), " +5.4210109e-20")
end

do
    assert_eq(string.format("%q", "asdf\nabc\"def"), "\"asdf\\\nabc\\\"def\"")

    assert_eq(string.format("%q", 15.345678), "0x1.eb0fcb4f1e4b4p+3")

    stringable = setmetatable({}, { __tostring = function ()
        return "abc"
    end })

    assert_eq(string.format("'%s'", stringable), "'abc'")

end

do
    assert_eq(string.format("%a", 0x1.fffffffffffffp+0), "0x1.fffffffffffffp+0")
    assert_eq(string.format("%.7a", 0x1.fffffffffffffp+0), "0x2.0000000p+0")
    assert_eq(string.format("%.7a", 0x1.88fffffffffffp+0), "0x1.8900000p+0")

    local smallest_normal = 0x1p-1022
    local largest_subnormal = 0x0.fffffffffffffp-1022
    local largest_subnormal_equiv = 0x1.ffffffffffffep-1023
    local smallest_subnormal = 0x1p-1074

    assert_eq(string.format("%a %g", smallest_normal, smallest_normal), "0x1p-1022 2.22507e-308")
    assert_eq(string.format("%a %g", largest_subnormal, largest_subnormal), "0x0.fffffffffffffp-1022 2.22507e-308")
    assert_eq(string.format("%a %g", smallest_subnormal, smallest_subnormal), "0x0.0000000000001p-1022 4.94066e-324")

    local small = 0x0.fffffffffffffp-1020
    assert_eq(string.format("%a %g", small, small), "0x1.ffffffffffffep-1021 8.9003e-308")

    -- A subnormal float with 1 more bit of precision than is representable
    -- TODO: The lexer currently incorrectly parses this as 0
    local mismatch, alt = 0x1.fffffffffffffp-1023, 2.2250738585072013e-308
    -- assert_eq(string.format("%a %g", mismatch, mismatch), "0x1p-1022 2.22507e-308")
    assert_eq(string.format("%a %g", alt, alt), "0x1p-1022 2.22507e-308")

    local n = 0x1.888p-1022
    assert_eq(string.format("%a %g", n, n), "0x1.888p-1022 3.41149e-308")

    -- test preserve decimal
    assert_eq(string.format("%#a %g", smallest_normal, smallest_normal), "0x1.p-1022 2.22507e-308")

    -- test fixed precision
    assert_eq(string.format("%.13a %g", smallest_normal, smallest_normal), "0x1.0000000000000p-1022 2.22507e-308")
end

do
    local s = ""
    -- table.concat would be nice here...
    for i = 1, 127 do
        s = s .. string.format("%c", i)
    end
    s = string.format("%q", s)
    assert_eq(s, [["\1\2\3\4\5\6\7\8\9\
\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31 !\"]]..
[[#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`ab]]..
[[cdefghijklmnopqrstuvwxyz{|}~\127"]])
end

do
    assert_eq(string.format("%q", (0.0/0)), "(0/0)")
    assert_eq(string.format("%q", (1.0/0)), "1e9999")
    assert_eq(string.format("%q", -(0.0/0)), "(0/0)")
    assert_eq(string.format("%q", -(1.0/0)), "-1e9999")
    assert_eq(string.format("%q %q", 1.0, math.pi), "0x1p+0 0x1.921fb54442d18p+1")

    -- TODO: impl load, make sure values round-trip
    -- assert_eq(string.format("%q", math.mininteger), "0x8000000000000000")
    -- assert_eq(string.format("%q", math.mininteger), "(-9223372036854775807-1)")
end

-- Piccolo-specific tests
if piccolo then
    assert_eq(string.format("%*s", 3, "a"), "  a")
    assert_eq(string.format("%-*s", 3, "a"), "a  ")
    assert_eq(string.format("%*s", -3, "a"), "a  ")

    assert_eq(string.format("%.2S", "aa😀bb"), "aa")
    assert_eq(string.format("%.3S", "aa😀bb"), "aa😀")
    assert_eq(string.format("%S", "aa😀bb"), "aa😀bb")

    assert_eq(string.format("%C", 0x1F600), "😀")
end
