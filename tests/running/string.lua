function test_concat()
    return
        "a" .. "b" .. "c" == "abc" and
        "a" .. 1 .. "c" == "a1c" and
        1 .. 2 .. 3 == "123"
end

return test_concat()
