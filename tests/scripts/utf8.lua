function is_err(f)
    return pcall(f) == false
end

do
    assert(is_err(function() return utf8.char(0x110000) end) and
        is_err(function() return utf8.char(0.1) end) and
        utf8.char(72, 69, 76.0, "76", 79) == "HELLO")
end
