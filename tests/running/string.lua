function is_err(f)
    return pcall(f) == false
end

function test_concat()
    return
        "a" .. "b" .. "c" == "abc" and
        "a" .. 1 .. "c" == "a1c" and
        1 .. 2 .. 3 == "123"
end

function test_coroutine()
    return nil
end

function test_len()
    return
        is_err(function() return string.len(nil) end) and
        is_err(function() return string.len(true) end) and
        is_err(function() return string.len(false) end) and
        is_err(function() return string.len({}) end) and
        is_err(function() return string.len(is_err) end) and
        is_err(function() return string.len(coroutine.create(test_coroutine)) end) and
        string.len("") == 0 and
        string.len("x") == 1 and
        string.len("x\0") == 2 and
        string.len(1) == 1 and
        string.len(-1) == 2 and
        string.len(12) == 2 and
        string.len(123) == 3 and
        string.len(1.2) == 3 and
        string.len(-1.2) == 4 and
        string.len(1.23) == 4 and
        string.len(2147483647) == 10 and
        string.len(-2147483648) == 11
end

function test_char()
    for i = 0,255,1
    do
        if is_err(function() return string.char(i) end) or string.len(string.char(i)) ~= 1 then
	    return false
        end
    end
    return
        is_err(function() return string.char(-1) end) and
        is_err(function() return string.char(256) end) and
        is_err(function() return string.char(1.5) end) and
        is_err(function() return string.char(nil) end) and
        is_err(function() return string.char(true) end) and
        is_err(function() return string.char(false) end) and
        is_err(function() return string.char({}) end) and
        is_err(function() return string.char(is_err) end) and
        is_err(function() return string.char(coroutine.create(test_coroutine)) end) and
        string.char() == "" and
        string.char(65.0) == "A" and
        string.char("65") == "A" and
        string.char(65) == "A" and
        string.char(65, 66) == "AB" and
        string.char(65, 66, 67) == "ABC" and
        string.char(0) == "\0"
end

function test_lower()
    for i = 0,255,1
    do
        if i >= 65 and i <= 90 then
            if string.lower(string.char(i)) ~= string.char(i + 32) then return false end
        else
            if string.lower(string.char(i)) ~= string.char(i) then return false end
        end
    end
    return
        is_err(function() return string.lower(nil) end) and
        is_err(function() return string.lower(true) end) and
        is_err(function() return string.lower(false) end) and
        is_err(function() return string.lower({}) end) and
        is_err(function() return string.lower(is_err) end) and
        is_err(function() return string.lower(coroutine.create(test_coroutine)) end) and
        string.lower("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") == "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz" -- > 32 bytes
end

function test_upper()
    for i = 0,255,1
    do
        if i >= 97 and i <= 122 then
            if string.upper(string.char(i)) ~= string.char(i - 32) then return false end
        else
            if string.upper(string.char(i)) ~= string.char(i) then return false end
        end
    end
    return
        is_err(function() return string.upper(nil) end) and
        is_err(function() return string.upper(true) end) and
        is_err(function() return string.upper(false) end) and
        is_err(function() return string.upper({}) end) and
        is_err(function() return string.upper(is_err) end) and
        is_err(function() return string.upper(coroutine.create(test_coroutine)) end) and
        string.upper("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") == "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ" and -- > 32 bytes
        string.upper("") == ""
end

function test_reverse()
    return
        is_err(function() return string.reverse(nil) end) and
        is_err(function() return string.reverse(true) end) and
        is_err(function() return string.reverse(false) end) and
        is_err(function() return string.reverse({}) end) and
        is_err(function() return string.reverse(is_err) end) and
        is_err(function() return string.reverse(coroutine.create(test_coroutine)) end) and
        string.reverse("") == "" and
        string.reverse("A") == "A" and
        string.reverse("AB") == "BA" and
        string.reverse("0123456789012345678901234567890123456789") == "9876543210987654321098765432109876543210" and -- > 32 bytes
        string.reverse(1.2) == "2.1" and
        string.reverse(12) == "21"
end

return test_concat() and
       test_len() and
       test_char() and
       test_lower() and
       test_upper() and
       test_reverse()
