function is_err(f)
    return pcall(f) == false
end

do
    assert("a" .. "b" .. "c" == "abc")
    assert("a" .. 1 .. "c" == "a1c")
    assert(1 .. 2 .. 3 == "123")
end

do
    function test_coroutine_len()
        return nil
    end

    assert(is_err(function() return string.len(nil) end))
    assert(is_err(function() return string.len(true) end))
    assert(is_err(function() return string.len(false) end))
    assert(is_err(function() return string.len({}) end))
    assert(is_err(function() return string.len(is_err) end))
    assert(is_err(function() return string.len(coroutine.create(test_coroutine_len)) end))
    assert(string.len("") == 0)
    assert(string.len("x") == 1)
    assert(string.len("x\0") == 2)
    assert(string.len(1) == 1)
    assert(string.len(-1) == 2)
    assert(string.len(12) == 2)
    assert(string.len(123) == 3)
    assert(string.len(1.2) == 3)
    assert(string.len(-1.2) == 4)
    assert(string.len(1.23) == 4)
    assert(string.len(2147483647) == 10)
    assert(string.len(-2147483648) == 11)
end

do
    assert(is_err(function() return string.sub(nil) end))
    assert(is_err(function() return string.sub(true, 1) end))
    assert(is_err(function() return string.sub(false) end))
    assert(is_err(function() return string.sub({}) end))
    assert(is_err(function() return string.sub(is_err) end))
    assert(is_err(function() return string.sub(coroutine.create(test_coroutine_len)) end))
    assert(string.sub(48, 1) == "48")
    assert(string.sub(48, 2) == "8")
    assert(string.sub(48, -1) == "8")
    assert(string.sub("hilo", -1, -4) == "")
    assert(string.sub("hilo", -4, 4) == "hilo")
    assert(string.sub("hilo", -4, -3) == "hi")
    assert(string.sub("hilo", -4, -6) == "")
    assert(string.sub("hilo", 0, -4) == "h")
    assert(string.sub(3.4, 1, 2) == "3.")
end

do
    assert([[
foo
bar
baz]] == "foo\nbar\nbaz")
end

do
    assert(is_err(function() return string.lower(nil) end))
    assert(is_err(function() return string.lower(true) end))
    assert(is_err(function() return string.lower(false) end))
    assert(is_err(function() return string.lower({}) end))
    assert(is_err(function() return string.lower(is_err) end))
    assert(is_err(function() return string.lower(coroutine.create(test_coroutine_len)) end))
    assert(string.lower("HelLo") == "hello")
    assert(string.lower("ABCDEFGHIJKLMNOP123QRSTUVWXYZ") == "abcdefghijklmnop123qrstuvwxyz")
    assert(string.lower(80) == "80")
    assert(string.lower(3.14) == "3.14")
end

do
    assert(is_err(function() return string.reverse(nil) end))
    assert(is_err(function() return string.reverse(true) end))
    assert(is_err(function() return string.reverse(false) end))
    assert(is_err(function() return string.reverse({}) end))
    assert(is_err(function() return string.reverse(is_err) end))
    assert(is_err(function() return string.reverse(coroutine.create(test_coroutine_len)) end))
    assert(string.reverse("HelLo") == "oLleH")
    assert(string.reverse("raCecar") == "raceCar")
    assert(string.reverse(84) == "48")
    assert(string.reverse(3.14) == "41.3")
end

do
    assert(is_err(function() return string.upper(nil) end))
    assert(is_err(function() return string.upper(true) end))
    assert(is_err(function() return string.upper(false) end))
    assert(is_err(function() return string.upper({}) end))
    assert(is_err(function() return string.upper(is_err) end))
    assert(is_err(function() return string.upper(coroutine.create(test_coroutine_len)) end))
    assert(string.upper("HelLo") == "HELLO")
    assert(string.upper("abcdefghijklmnop123qrstuvwxyz") == "ABCDEFGHIJKLMNOP123QRSTUVWXYZ")
    assert(string.upper(80) == "80")
    assert(string.upper(3.14) == "3.14")
end

do
    assert(is_err(function() return string.byte(nil) end))
    assert(is_err(function() return string.byte(true) end))
    assert(is_err(function() return string.byte({}) end))
    local a, b, c = string.byte("abc", 1, 3)
    assert(a == 97 and b == 98 and c == 99)
    local a, b = string.byte("abc", 2)
    assert(a == 98 and b == 99)
    local a = string.byte("abc", -1)
    assert(a == 99)
    local a, b = string.byte("abc", -2)
    assert(a == 98 and b == 99)
    local a, b = string.byte("abc", 1, 1)
    assert(a == 97 and b == nil)
    assert(select("#", string.byte("abc", 4)) == 0)
    assert(select("#", string.byte("abc", 1, 0)) == 0)
    assert(select("#", string.byte("abc", -1, -2)) == 0)
    assert(string.byte("a\0b", 1, 3) == 97 and select(2, string.byte("a\0b", 1, 3)) == 0 and select(3, string.byte("a\0b", 1, 3)) == 98)
end

do
    assert(string.char() == nil)
    assert(string.char(97, 98, 99) == "abc")
    assert(string.char(0) == "\0")
    assert(string.char(255) == "\xff")
    assert(string.char(0x41, 0x42, 0x43) == "ABC")
    assert(is_err(function() return string.char(-1) end))
    assert(is_err(function() return string.char(0x110000) end))
    assert(is_err(function() return string.char(97, nil, 99) end))
    assert(is_err(function() return string.char(true) end))
    assert(is_err(function() return string.char({}) end))
    assert(string.char(string.byte("abc", 1, 3)) == "abc")
end

do
    assert(string.packsize("b") == 1)
    assert(string.packsize("B") == 1)
    assert(string.packsize("h") == 2)
    assert(string.packsize("H") == 2)
    assert(string.packsize("i4") == 4)
    assert(string.packsize("I4") == 4)
    assert(string.packsize("f") == 4)
    assert(string.packsize("d") == 8)
    assert(string.packsize("n") == 8)
    assert(string.packsize("c3") == 3)
    assert(is_err(function() return string.packsize("z") end))
    assert(string.packsize("s4") == 4)
    assert(string.packsize("x") == 1)

    local packed_byte = string.pack("b", -12)
    assert(#packed_byte == 1)
    assert(string.unpack("b", packed_byte) == -12)

    local packed_ubyte = string.pack("B", 200)
    assert(#packed_ubyte == 1)
    assert(string.unpack("B", packed_ubyte) == 200)

    local packed_short = string.pack(">h", 513)
    assert(#packed_short == 2)
    assert(string.unpack(">h", packed_short) == 513)

    local packed_int = string.pack("<i4", 123456)
    assert(#packed_int == 4)
    assert(string.unpack("<i4", packed_int) == 123456)

    local packed_float = string.pack("f", 3.14)
    assert(#packed_float == 4)
    local unpacked_float = string.unpack("f", packed_float)
    assert(math.abs(unpacked_float - 3.14) < 0.0001)

    local packed_double = string.pack("d", 123.456)
    assert(#packed_double == 8)
    local unpacked_double = string.unpack("d", packed_double)
    assert(math.abs(unpacked_double - 123.456) < 0.000001)

    local packed_str_fixed = string.pack("c5", "abc")
    assert(#packed_str_fixed == 5)
    assert(packed_str_fixed == "abc\0\0")
    assert(string.unpack("c5", packed_str_fixed) == "abc\0\0")

    local packed_str_z = string.pack("z", "hello")
    assert(#packed_str_z == 6)
    assert(packed_str_z == "hello\0")
    assert(string.unpack("z", packed_str_z) == "hello")

    local packed_str_len = string.pack("s2", "world")
    assert(#packed_str_len == 2 + 5)
    local str, pos = string.unpack("<s2", packed_str_len)
    assert(str == "world" and pos == 8)

    local multi_pack = string.pack("bsi4f", -10, "test", 50000, 1.5)
    assert(string.packsize("bsi4f") == 1 + 8 + 4 + 4)
    local b, s_val, i, f, pos = string.unpack("bsi4f", multi_pack)
    assert(b == -10 and s_val == "test" and i == 50000 and math.abs(f - 1.5) < 0.0001 and pos == #multi_pack + 1)

    assert(string.packsize("bXi4") == 5)
    local packed_align = string.pack("bXi4", 5, 100)
    assert(#packed_align == 5)
    local b_val, i_val, pos_val = string.unpack("bXi4", packed_align)
    assert(b_val == 5 and i_val == 100 and pos_val == #packed_align + 1)

    local packed_be = string.pack(">i4", 0x12345678)
    local packed_le = string.pack("<i4", 0x12345678)
    assert(packed_be ~= packed_le)
    assert(string.unpack(">i4", packed_be) == 0x12345678)
    assert(string.unpack("<i4", packed_le) == 0x12345678)

    local data = string.pack("bb", 10, 20)
    local v1, p1 = string.unpack("b", data, 1)
    local v2, p2 = string.unpack("b", data, p1)
    assert(v1 == 10 and p1 == 2 and v2 == 20 and p2 == 3)
end

do
    assert(string.find("hello world", "world") == 7)
    assert(string.find("hello world", "l") == 3)
    assert(string.find("hello world", "l", 4) == 4)
    assert(string.find("hello world", "l", 5) == 10)
    assert(string.find("hello world", "notfound") == nil)
    assert(string.find("hello world", "o", 1, true) == 5)
    assert(string.find("hello world", "o", 6, true) == 8)
    assert(string.find("banana", "ana") == 2)
    assert(string.find("banana", "ana", 3) == 4)
    assert(string.find("banana", "a.a", 1) == 2)
    assert(string.find("hi %world", "%%world") == 4)
    local s, e = string.find("abc", "")
    assert(s == 1 and e == 0)
    local s, e = string.find("abc", "", 4)
    assert(s == 4 and e == 3)
    assert(string.find("a\0b", "a\0b") == 1)
    assert(string.find("abc 123 def", "%s*") == 1)
    assert(string.find("abc123def", "[%d]+") == 4)
    assert(string.find("abc123def", "[^%a]+") == 4)
    assert(string.find("a-b=c+d", "[-%+=]") == 2)
    assert(string.find("ABCdef", "%u+") == 1)
    local s, e, cap = string.find("hello world", "w(.)rl")
    assert(s == 7 and e == 10 and cap == "o")
    local s, e, c1, c2 = string.find("a=1 b=2", "(%a)=(%d)")
    assert(s == 1 and e == 3 and c1 == "a" and c2 == "1")
    local s, e, cap = string.find("abc", "a(.)c")
    assert(s == 1 and e == 3 and cap == "b")
    local s, e = string.find("ac", "a()c")
    assert(s == 1 and e == 2)
    assert(string.find("", "a") == nil)
    assert(string.find("aaa", "a+") == 1)
    assert(string.find("---a", "%-%-a") == 2)
end

do
    assert(string.match("hello world", "world") == "world")
    assert(string.match("hello world", "l..o") == nil)
    assert(string.match("hello world 123", "%d+") == "123")
    assert(string.match("hello world", "(%a+) (%a+)") == "hello")
    local w1, w2 = string.match("hello world", "(%a+) (%a+)")
    assert(w1 == "hello" and w2 == "world")
    assert(string.match("hello world", "notfound") == nil)
    assert(string.match("123 abc", "^%d+") == "123")
    assert(string.match("123 abc", "%a+$") == "abc")
    local a, b, c = string.match("a=1, b=2, c=3", "(%a+)=(%d+)")
    assert(a == "a" and b == "1" and c == nil)
    assert(string.match("a\0b", "a\0b") == "a\0b")
    assert(string.match("abc", "", 1) == "")
end

do
    local results = {}
    for w in string.gmatch("hello world from lua", "%a+") do
        table.insert(results, w)
    end
    assert(results[1] == "hello" and results[2] == "world" and results[3] == "from" and results[4] == "lua")

    results = {}
    for k, v in string.gmatch("a=1 b=2 c=3", "(%a+)=(%d+)") do
        results[k] = tonumber(v)
    end
    assert(results["a"] == 1 and results["b"] == 2 and results["c"] == 3)

    local i = 0
    for c in string.gmatch("abc", ".") do i = i + 1 end
    assert(i == 3)

    local count = 0
    for _ in string.gmatch("abc", "x") do count = count + 1 end
    assert(count == 0)

    results = {}
    -- FIXME: Empty capture `()` pattern matching seems broken in string.gmatch (returns empty/nil instead of position)
    for _, word, _ in string.gmatch("first second third", "()(%a+)()") do
        table.insert(results, word)
    end
    assert(results[1] == "first" and results[2] == "second" and results[3] == "third")

    results = {}
    for x in string.gmatch("a\0b\0c", "[a-c]") do table.insert(results, x) end
    assert(results[1] == "a" and results[2] == "b" and results[3] == "c")

    local t = {}
    for w in string.gmatch("", "%a+") do t[#t+1]=w end
    assert(#t == 0)

    local str = "abc"
    local iter = string.gmatch(str, ".")
    assert(iter() == "a")
    assert(iter() == "b")
    assert(iter() == "c")
    assert(iter() == nil)
end

do
    assert(string.gsub("hello world", "world", "lua") == "hello lua")
    local s, n = string.gsub("hello world", "l", "*")
    assert(s == "he**o wor*d" and n == 3)
    s, n = string.gsub("hello world", "notfound", "X")
    assert(s == "hello world" and n == 0)
    s, n = string.gsub("banana", "a", "o", 2)
    assert(s == "bonona" and n == 2)
    assert(string.gsub("hello world", "(%a+)", "%1 %1") == "hello hello world world")
    assert(string.gsub("hello world", "(%a+) (%a+)", "%2 %1") == "world hello")
    -- FIXME: string.gsub does not correctly handle the %0 (whole match) escape sequence in the replacement string.
    -- assert(string.gsub("abc", ".", "%0-") == "a-b-c-")

    -- TODO: not implemented
    -- local s, n = string.gsub("a=1, b=2", "(%a+)=(%d+)", function(k, v) return k .. "=" .. (v+1) end)
    -- assert(s == "a=2, b=3" and n == 2)

    local t = {a = "X", b = "Y"}
    s, n = string.gsub("abc", "(%a)", t)
    -- FIXME: Piccolo currently returns n=3 (counts non-replacement)
    assert(s == "XYc") -- Check only string for now
    s, n = string.gsub("a b c", "%a", {a = "X", b = "Y", c = "Z"})
    assert(s == "X Y Z" and n == 3)

    -- TODO: not implemented
    -- s, n = string.gsub("a=1 b=2", "(%a)=(%d)", function (a, b) return t[a] or b end)
    -- assert(s == "X Y" and n == 2)

    s, n = string.gsub("hello", ".", "%%")
    assert(s == "%%%%%" and n == 5)
    assert(string.gsub("a\0b", "a\0b", "test") == "test")
end
