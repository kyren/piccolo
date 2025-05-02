function is_err(f, ...)
    local status, err = pcall(f, ...)
    return not status, err
end

function collect_codes(s)
    local results = {}
    local err_status, err_val = pcall(function()
        for p, c in utf8.codes(s) do
            table.insert(results, {p, c})
        end
    end)
    if not err_status then
        return false, err_val
    end
    return results
end

function collect_codepoints(s, i, j)
    local results = {}
    local args = {s, i, j}
    local err_status, err_val = pcall(function()
        local values = {utf8.codepoint(table.unpack(args))}
        for _, v in ipairs(values) do
            table.insert(results, v)
        end
    end)
     if not err_status then
        return false, err_val
    end
    return results
end

do
    assert(utf8.char() == "")
    assert(utf8.char(65) == "A")
    assert(utf8.char(65, 66, 67) == "ABC")
    assert(utf8.char(0x41, 0x42, 0x43) == "ABC")
    assert(utf8.char(1055, 1088, 1080, 1074, 1077, 1090) == "Привет")
    assert(utf8.char(72, 1080, 33) == "Hи!")
    assert(utf8.char(0xC2, 0xA2) == "\195\130\194\162")
    assert(utf8.char(162) == "\194\162")
    assert(utf8.char(0xE2, 0x82, 0xAC) == "\195\162\194\130\194\172")
    assert(utf8.char(8364) == "\226\130\172")
    assert(utf8.char(0xF0, 0x9F, 0x98, 0x80) == "\195\176\194\159\194\152\194\128")
    assert(utf8.char(128512) == "\240\159\152\128")
    assert(utf8.char(0) == "\0")
    assert(utf8.char(65, 0, 66) == "A\0B")
    assert(utf8.char(0x7F) == "\127")
    assert(utf8.char(0x80) == "\194\128")
    assert(utf8.char(0x7FF) == "\223\191")
    assert(utf8.char(0x800) == "\224\160\128")
    assert(utf8.char(0xFFFF) == "\239\191\191")
    assert(utf8.char(0x10000) == "\240\144\128\128")
    assert(utf8.char(0x10FFFF) == "\244\143\191\191")
    assert(is_err(utf8.char, "A"))
    assert(is_err(utf8.char, 65, "B"))
    assert(is_err(utf8.char, {}))
    assert(is_err(utf8.char, nil))
    assert(is_err(utf8.char, -1))
    assert(is_err(utf8.char, 0x110000))
    assert(is_err(utf8.char, 0xD800))
    assert(is_err(utf8.char, 0xDFFF))
    assert(is_err(utf8.char, 0x110000))
    assert(is_err(utf8.char, "not a number"))
end

do
    assert(utf8.charpattern == "[\\0-\\x7F\\xC2-\\xF4][\\x80-\\xBF]*")
end

do
    local empty_codes = collect_codes("")
    assert(type(empty_codes) == "table" and #empty_codes == 0)

    local abc_codes = collect_codes("ABC")
    assert(type(abc_codes) == "table" and #abc_codes == 3)
    assert(abc_codes[1][1] == 1 and abc_codes[1][2] == 65)
    assert(abc_codes[2][1] == 2 and abc_codes[2][2] == 66)
    assert(abc_codes[3][1] == 3 and abc_codes[3][2] == 67)

    local ab0c_codes = collect_codes("AB\0C")
    assert(type(ab0c_codes) == "table" and #ab0c_codes == 4)
    assert(ab0c_codes[1][1] == 1 and ab0c_codes[1][2] == 65)
    assert(ab0c_codes[2][1] == 2 and ab0c_codes[2][2] == 66)
    assert(ab0c_codes[3][1] == 3 and ab0c_codes[3][2] == 0)
    assert(ab0c_codes[4][1] == 4 and ab0c_codes[4][2] == 67)

    local privet = "Привет"
    local privet_codes = collect_codes(privet)
    assert(#privet_codes == 6)
    assert(privet_codes[1][1] == 1 and privet_codes[1][2] == 1055) 
    assert(privet_codes[2][1] == 3 and privet_codes[2][2] == 1088) 
    assert(privet_codes[3][1] == 5 and privet_codes[3][2] == 1080) 
    assert(privet_codes[4][1] == 7 and privet_codes[4][2] == 1074) 
    assert(privet_codes[5][1] == 9 and privet_codes[5][2] == 1077) 
    assert(privet_codes[6][1] == 11 and privet_codes[6][2] == 1090) 

    local hieuro = "Hi€!"
    local hieuro_codes = collect_codes(hieuro)
    assert(#hieuro_codes == 4)
    assert(hieuro_codes[1][1] == 1 and hieuro_codes[1][2] == 72) 
    assert(hieuro_codes[2][1] == 2 and hieuro_codes[2][2] == 105) 
    assert(hieuro_codes[3][1] == 3 and hieuro_codes[3][2] == 8364) 
    assert(hieuro_codes[4][1] == 6 and hieuro_codes[4][2] == 33) 

    local emoji = "😀"
    local emoji_codes = collect_codes(emoji)
    assert(#emoji_codes == 1)
    assert(emoji_codes[1][1] == 1 and emoji_codes[1][2] == 128512)

    assert(collect_codes("abc\xE2\x82") == false)
    assert(collect_codes("abc\xE2\x82\xFF") == false)
    assert(collect_codes("abc\xFF") == false)
    assert(collect_codes("\xC0\x80") == false)
end

do
    local s = "ABC"
    assert(table.concat(collect_codepoints(s), ",") == "65")
    assert(table.concat(collect_codepoints(s, 1), ",") == "65")
    assert(table.concat(collect_codepoints(s, 2), ",") == "66")
    assert(table.concat(collect_codepoints(s, 3), ",") == "67")
    assert(collect_codepoints(s, 4) == false)
    assert(table.concat(collect_codepoints(s, 1, 1), ",") == "65")
    assert(table.concat(collect_codepoints(s, 1, 2), ",") == "65,66")
    assert(table.concat(collect_codepoints(s, 1, 3), ",") == "65,66,67")
    assert(table.concat(collect_codepoints(s, 2, 3), ",") == "66,67")
    assert(table.concat(collect_codepoints(s, 3, 3), ",") == "67")
    assert(collect_codepoints(s, 1, 10) == false)
    assert(table.concat(collect_codepoints(s, 3, 1), ",") == "")
    assert(table.concat(collect_codepoints(s, -1), ",") == "67")
    assert(table.concat(collect_codepoints(s, -2), ",") == "66")
    assert(table.concat(collect_codepoints(s, -3), ",") == "65")
    assert(table.concat(collect_codepoints(s, -3, -1), ",") == "65,66,67")
    assert(table.concat(collect_codepoints(s, -2, -1), ",") == "66,67")
    assert(table.concat(collect_codepoints(s, -1, -1), ",") == "67")
    assert(table.concat(collect_codepoints(s, 1, -1), ",") == "65,66,67")
    assert(table.concat(collect_codepoints(s, 2, -1), ",") == "66,67")
    assert(table.concat(collect_codepoints(s, 1, -2), ",") == "65,66")
    assert(table.concat(collect_codepoints(s, -3, 3), ",") == "65,66,67")
    assert(table.concat(collect_codepoints(s, -3, 1), ",") == "65")

    local privet = "Привет"
    assert(table.concat(collect_codepoints(privet, 1), ",") == "1055")
    assert(table.concat(collect_codepoints(privet, 1), ",") == "1055")
    assert(table.concat(collect_codepoints(privet, 3), ",") == "1088")
    assert(table.concat(collect_codepoints(privet, 1, 2), ",") == "1055")
    assert(table.concat(collect_codepoints(privet, 1, 3), ",") == "1055,1088")
    assert(table.concat(collect_codepoints(privet, 1, 4), ",") == "1055,1088")
    assert(table.concat(collect_codepoints(privet, 1, 12), ",") == "1055,1088,1080,1074,1077,1090")
    assert(table.concat(collect_codepoints(privet, 3, 7), ",") == "1088,1080,1074")
    assert(table.concat(collect_codepoints(privet, -2, -1), ",") == "1090")
    assert(table.concat(collect_codepoints(privet, 11, -1), ",") == "1090")
    assert(table.concat(collect_codepoints(privet, 1, -1), ",") == "1055,1088,1080,1074,1077,1090")
    assert(collect_codepoints("", 1, 1) == false)

    local emoji = "😀"
    assert(table.concat(collect_codepoints(emoji), ",") == "128512")
    assert(table.concat(collect_codepoints(emoji, 1), ",") == "128512")
    assert(table.concat(collect_codepoints(emoji, 1), ",") == "128512")
    assert(table.concat(collect_codepoints(emoji, 1, 4), ",") == "128512")
    assert(collect_codepoints("abc\xE2\x82", 1) == false)
    assert(collect_codepoints("abc\xE2\x82\xFF", 1) == false)
    assert(collect_codepoints("abc\xFF", 1) == false)
    assert(collect_codepoints("abc\xFF", 4) == false)
    assert(collect_codepoints("abc\xE2\x82", 1, 5) == false)
    assert(collect_codepoints("abc\xE2\x20\xAC", 1, 6) == false)
end

do
    assert(utf8.len("") == 0)
    assert(utf8.len("ABC") == 3)
    assert(utf8.len("При") == 3)
    assert(utf8.len("Привет") == 6)
    assert(utf8.len("😀") == 1)
    assert(utf8.len("A😀B") == 3)
    assert(utf8.len("A\0B") == 3)

    local s = "Привет"
    assert(utf8.len(s, 1, 1) == 1)
    assert(utf8.len(s, 1, 2) == 1)
    assert(utf8.len(s, 1, 3) == 2)
    assert(utf8.len(s, 1, 4) == 2)
    assert(utf8.len(s, 3, 4) == 1)
    assert(utf8.len(s, 3, 6) == 2)
    assert(utf8.len(s, 1, 12) == 6)
    assert(utf8.len(s, 1, -1) == 6)
    assert(utf8.len(s, -12, -1) == 6)
    assert(utf8.len(s, -2, -1) == 1)
    assert(utf8.len(s, 11, 12) == 1)
    assert(utf8.len(s, 1, 6) == 3)
    assert(utf8.len(s, 7, 12) == 3)
    assert(utf8.len(s, 13, 20) == 0)
    assert(utf8.len(s, 5, 1) == 0)
    assert(utf8.len(s, 1, 11) == 6)
end

do
    local s = "Привет"
    assert(utf8.offset(s, 0) == 1)
    assert(utf8.offset(s, 1) == 1)
    assert(utf8.offset(s, 2) == 3)
    assert(utf8.offset(s, 6) == 11)
    assert(utf8.offset(s, 7) == 13)
    assert(utf8.offset(s, 8) == nil)
    assert(utf8.offset(s, -1) == 11)
    assert(utf8.offset(s, -2) == 9)
    assert(utf8.offset(s, -6) == 1)
    assert(utf8.offset(s, -7) == nil)
    assert(utf8.offset(s, 1, 1) == 1)
    assert(is_err(utf8.offset, s, 1, 2))
    assert(utf8.offset(s, 1, 3) == 3)
    assert(utf8.offset(s, 2, 3) == 5)
    assert(utf8.offset(s, 1, 11) == 11)
    assert(is_err(utf8.offset, s, 1, 12))
    assert(utf8.offset(s, 1, 13) == 13)
    assert(utf8.offset(s, 2, 11) == 13)
    assert(is_err(utf8.offset, s, 2, 12))
    assert(is_err(utf8.offset, s, -1, 12))
    assert(utf8.offset(s, -1, 11) == 9)
    assert(utf8.offset(s, -1, 3) == 1)
    assert(is_err(utf8.offset, s, -1, 2))
    assert(utf8.offset(s, -1, 1) == nil)
    assert(is_err(utf8.offset, s, -2, 12))
    assert(is_err(utf8.offset, s, -6, 12))
    assert(is_err(utf8.offset, s, -7, 12))
    assert(utf8.offset(s, -1, #s + 1) == 11)
    assert(utf8.offset(s, 0, 1) == 1)
    assert(utf8.offset(s, 0, 2) == 1)
    assert(utf8.offset(s, 0, 3) == 3)
    assert(utf8.offset(s, 0, 4) == 3)
    assert(utf8.offset(s, 0, 11) == 11)
    assert(utf8.offset(s, 0, 12) == 11)
    assert(utf8.offset(s, 0, 13) == nil)
    assert(is_err(utf8.offset, s, 0, 0))
    assert(utf8.offset(s, 0, -1) == 11)
    assert(utf8.offset(s, 0, -12) == 1)

    local ascii = "ABCDEFG"
    assert(utf8.offset(ascii, 3, 1) == 3)
    assert(utf8.offset(ascii, -3, 7) == 4)
    assert(utf8.offset(ascii, 0, 5) == 5)

    local emoji = "A😀B"
    assert(utf8.offset(emoji, 1) == 1)
    assert(utf8.offset(emoji, 2) == 2)
    assert(utf8.offset(emoji, 3) == 6)
    assert(utf8.offset(emoji, 4) == 7)
    assert(utf8.offset(emoji, -1) == 6)
    assert(utf8.offset(emoji, -2) == 2)
    assert(utf8.offset(emoji, -3) == 1)
    assert(utf8.offset(emoji, 0, 1) == 1)
    assert(utf8.offset(emoji, 0, 2) == 2)
    assert(utf8.offset(emoji, 0, 3) == 2)
    assert(utf8.offset(emoji, 0, 4) == 2)
    assert(utf8.offset(emoji, 0, 5) == 2)
    assert(utf8.offset(emoji, 0, 6) == 6)
    assert(utf8.offset(emoji, 0, 7) == nil)
end
