function is_err(f)
    return pcall(f) == false
end

function test1()
    return 2   & 3     == 2 and
           2.0 & 3.0   == 2 and
           "2" & 3.0   == 2 and
           2   & "3.0" == 2
end

function test2()
    return 2   | 3     == 3 and
           2.0 | 3.0   == 3 and
           "2" | 3.0   == 3 and
           2   | "3.0" == 3
end

function test3()
    return 2   ~ 3     == 1 and
           2.0 ~ 3.0   == 1 and
           "2" ~ 3.0   == 1 and
           2   ~ "3.0" == 1
end

function test4()
    return ~2   == -3 and
           ~"2" == -3 and
           ~2.0 == -3
end

function test5()
    return 2   << 3     == 16 and
           2.0 << 3.0   == 16 and
           "2" << 3.0   == 16 and
           2   << "3.0" == 16
end

function test6()
    return 145   >> 3     == 18 and
           145.0 >> 3.0   == 18 and
           "145" >> 3.0   == 18 and
           145   >> "3.0" == 18 and
           -1    >> 1     == 9223372036854775807
end

function test7()
    return is_err(function() return ~2.2    end) and
           is_err(function() return ~"2.2"  end) and
           is_err(function() return 2.2 & 3 end) and
           is_err(function() return 2.2 | 3 end) and
           is_err(function() return 2.2 ~ 3 end) and
           is_err(function() return 2.2 << 3 end) and
           is_err(function() return 2.2 >> 3 end)
end

return
    test1() and
    test2() and
    test3() and
    test4() and
    test5() and
    test6() and
    test7()
