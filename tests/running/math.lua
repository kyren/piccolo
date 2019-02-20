function is_nan(n)
    return n ~= n
end

function is_err(f)
    return pcall(f) == false
end

function is_integer(n)
    return is_err(function() return n % 0 end)
end

function test1()
    return math.abs(  1)     == 1    and
           math.abs( -1)     == 1    and
           math.abs(  1.0)   == 1.0  and
           math.abs( -1.0)   == 1.0  and
           is_integer(math.abs(1  )) and
       not is_integer(math.abs(1.0))
end

function test2()
    return math.acos(1) == 0.0 and
           math.abs(math.acos(0)   - math.pi/2) < 1e-7 and
           math.abs(math.acos(0.5) - math.pi/3) < 1e-7 and
           math.abs(math.acos(-1)  - math.pi) < 1e-7 and
           is_nan(math.acos( 2)) and
           is_nan(math.acos(-2)) and
       not is_integer(math.acos(1))
end

function test3()
    return math.asin(0) == 0.0 and
           math.abs(math.asin(1)   - math.pi/2) < 1e-7 and
           math.abs(math.asin(0.5) - math.pi/6) < 1e-7 and
           math.abs(math.asin(-1)  + math.pi/2) < 1e-7 and
           is_nan(math.asin( 2)) and
           is_nan(math.asin(-2)) and
       not is_integer(math.asin(0))
end

function test4()
    return math.atan(0) == 0.0 and
           math.abs(math.atan(1)   - math.pi/4) < 1e-7 and
           math.abs(math.atan(0.5) - 0.46364760900081) < 1e-7 and
           math.abs(math.atan(-1)  + math.pi/4) < 1e-7 and
           math.abs(math.atan(2)   - 1.1071487177941) < 1e-7 and
           math.abs(math.atan(-2)  + 1.1071487177941) < 1e-7 and
           math.abs(math.atan(math.huge)  - math.pi/2) < 1e-7 and
           math.abs(math.atan(-math.huge) + math.pi/2) < 1e-7 and
       not is_integer(math.atan(0))
end

function test5()
    return math.atan2(0, 0) == 0.0 and
           math.atan2(0, 1) == 0.0 and
           math.abs(math.atan2( 1,  0) - math.pi/2)  < 1e-7 and
           math.abs(math.atan2( 1,  1) - math.pi/4) < 1e-7 and
           math.abs(math.atan2(-1,  1) + math.pi/4) < 1e-7 and
           math.abs(math.atan2( 1, -1) - 3*math.pi/4)  < 1e-7 and
           math.abs(math.atan2(-1, -1) + 3*math.pi/4)  < 1e-7 and
           math.abs(math.atan2(math.huge, 1) - math.pi/2) < 1e-7 and
       not is_integer(math.atan2(0, 0))
end

function test6()
    return math.ceil( 1.0) ==  1 and
           math.ceil(-1.0) == -1 and
           math.ceil( 1.1) ==  2 and
           math.ceil(-1.1) == -1 and
           is_integer(math.ceil(1.0))
end

function test7()
    return math.cos(0) == 1.0 and
           math.abs(math.cos(math.pi) + 1.0) < 1e-7 and
           math.abs(math.cos(math.pi/6) - math.sqrt(3)/2) < 1e-7 and
           math.abs(math.cos(1) - 0.5403023058681398) < 1e-7 and
           math.abs(math.cos(1) - 0.5403023058681398) < 1e-7 and
           is_nan(math.cos(math.huge)) and
       not is_integer(math.cos(0))
end

function test8()
    return math.cosh(0) == 1.0 and
           math.abs(math.cosh( 1) - 1.543080634815244) < 1e-7 and
           math.abs(math.cosh(-1) - 1.543080634815244) < 1e-7 and
           math.cosh(math.huge) == math.huge and
       not is_integer(math.cosh(0))
end

function test9()
    return math.abs(math.deg(math.pi) - 180) < 1e-7 and
           math.abs(math.deg(1) - 180/math.pi) < 1e-7 and
       not is_integer(math.deg(math.pi))
end

function test10()
    return math.abs(math.exp(1) - 2.718281828459) < 1e-7 and
           math.exp(0) == 1 and
           math.abs(math.exp(-1) - 0.36787944117144) < 1e-7 and
           math.exp(-math.huge) == 0 and
           math.exp(math.huge) == math.huge and
       not is_integer(math.exp(0))
end

function test11()
    return math.floor( 1.0) ==  1 and
           math.floor(-1.0) == -1 and
           math.floor( 1.1) ==  1 and
           math.floor(-1.1) == -2 and
           is_integer(math.ceil(1.0))
end

function test12()
    return math.abs(math.fmod( 6.2,  3.4) - 2.8) < 1e-7 and
           math.abs(math.fmod(-6.2,  3.4) + 2.8) < 1e-7 and
           math.abs(math.fmod( 6.2, -3.4) - 2.8) < 1e-7 and
           math.abs(math.fmod(-6.2, -3.4) + 2.8) < 1e-7 and
       not is_integer(math.fmod(1.0, 1.0))
end

return test1() and
       test2() and
       test3() and
       test4() and
       test5() and
       test6() and
       test7() and
       test8() and
       test9() and
       test10() and
       test11() and
       test12()
