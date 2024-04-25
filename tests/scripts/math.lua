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
    return math.atan(0, 0) == 0.0 and
           math.atan(0, 1) == 0.0 and
           math.abs(math.atan( 1,  0) - math.pi/2)  < 1e-7 and
           math.abs(math.atan( 1,  1) - math.pi/4) < 1e-7 and
           math.abs(math.atan(-1,  1) + math.pi/4) < 1e-7 and
           math.abs(math.atan( 1, -1) - 3*math.pi/4)  < 1e-7 and
           math.abs(math.atan(-1, -1) + 3*math.pi/4)  < 1e-7 and
           math.abs(math.atan(math.huge, 1) - math.pi/2) < 1e-7 and
       not is_integer(math.atan(0, 0))
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
           is_nan(math.cos(math.huge)) and
       not is_integer(math.cos(0))
end

function test8()
    return math.abs(math.deg(math.pi) - 180) < 1e-7 and
           math.abs(math.deg(1) - 180/math.pi) < 1e-7 and
       not is_integer(math.deg(math.pi))
end

function test9()
    return math.abs(math.exp(1) - 2.718281828459) < 1e-7 and
           math.exp(0) == 1 and
           math.abs(math.exp(-1) - 0.36787944117144) < 1e-7 and
           math.exp(-math.huge) == 0 and
           math.exp(math.huge) == math.huge and
       not is_integer(math.exp(0))
end

function test10()
    return math.floor( 1.0) ==  1 and
           math.floor(-1.0) == -1 and
           math.floor( 1.1) ==  1 and
           math.floor(-1.1) == -2 and
           is_integer(math.floor(1.0))
end

function test11()
    return math.abs(math.fmod( 6.2,  3.4) - 2.8) < 1e-7 and
           math.abs(math.fmod(-6.2,  3.4) + 2.8) < 1e-7 and
           math.abs(math.fmod( 6.2, -3.4) - 2.8) < 1e-7 and
           math.abs(math.fmod(-6.2, -3.4) + 2.8) < 1e-7 and
       not is_integer(math.fmod(1.0, 1.0))
end

function test12()
    return math.log(0) == -math.huge and
           math.log(1) == 0.0 and
           math.abs(math.log(10) - 2.302585092994) < 1e-7 and
           is_nan(math.log(-1))
end

function test13()
    return math.log(0, 10) == -math.huge and
           math.log(1, 10) == 0.0 and
           math.log(10, 10) == 1.0 and
           math.log(math.exp(1), math.exp(1)) == 1.0 and
           math.abs(math.log(3.1622776601684, 10) - 0.5) < 1e-7 and
           is_nan(math.log(-1, 10))
end

function test14()
    return math.max(1, 2, 3) == 3 and
           is_integer(math.max(1, 2, 3)) and
           math.max(1.0, 2.0, 3.0) == 3.0 and
       not is_integer(math.max(1.0, 2.0, 3.0)) and
           math.max(3, 3.0, 3.0) == 3 and
           is_integer(math.max(3, 3.0, 3.0)) and
           math.max(-5, -4, -3, -2, -1, 0, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1) == 10
    -- Tests we do not currently pass due to incompatibility with PUC-Rio Lua
    --     is_err(math.max(1, "2", 1))
end

function test15()
    return math.min(3, 2, 1) == 1 and
           is_integer(math.min(3, 2, 1)) and
           math.min(3.0, 2.0, 1.0) == 1.0 and
       not is_integer(math.min(3.0, 2.0, 1.0)) and
           math.min(3, 3.0, 3.0) == 3 and
           is_integer(math.min(3, 3.0, 3.0)) and
           math.min(5, 4, 3, 2, 1, 0, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1) == -10
    -- Tests we do not currently pass due to incompatibility with PUC-Rio Lua
    --     is_err(math.min(1, "2", 1))
    --     is_nan(math.min(0.0 % 0.0, 1, 2))
end

function test16()
    return
        math.modf(216.57550707459) == 216 and
        math.abs(select(2, math.modf(216.57550707459)) - 0.5755070745945) < 1e-7 and
        math.modf(-356.93994024768) == -356 and
        math.abs(select(2, math.modf(-356.93994024768)) + 0.93994024768472) < 1e-7 and
        math.modf(-230.78241851181) == -230 and
        math.abs(select(2, math.modf(-230.78241851181)) + 0.78241851180792) < 1e-7 and
        math.modf(-184.36720222235) == -184 and
        math.abs(select(2, math.modf(-184.36720222235)) + 0.36720222234726) < 1e-7 and
        math.modf(95.250256825238) == 95 and
        math.abs(select(2, math.modf(95.250256825238)) - 0.25025682523847) < 1e-7 and
        math.modf(-383.47252598032) == -383 and
        math.abs(select(2, math.modf(-383.47252598032)) + 0.47252598032355) < 1e-7 and
        math.modf(480.07427062839) == 480 and
        math.abs(select(2, math.modf(480.07427062839)) - 0.074270628392696) < 1e-7 and
        math.modf(123.98638017476) == 123 and
        math.abs(select(2, math.modf(123.98638017476)) - 0.98638017475605) < 1e-7 and
        math.modf(365.17379572615) == 365 and
        math.abs(select(2, math.modf(365.17379572615)) - 0.17379572615027) < 1e-7 and
        math.modf(-271.84157492593) == -271 and
        math.abs(select(2, math.modf(-271.84157492593)) + 0.84157492592931) < 1e-7 and
        math.modf(347.25950565189) == 347 and
        math.abs(select(2, math.modf(347.25950565189)) - 0.25950565189123) < 1e-7 and
        math.modf(406.10403195024) == 406 and
        math.abs(select(2, math.modf(406.10403195024)) - 0.10403195023537) < 1e-7 and
        math.modf(365.7353548333) == 365 and
        math.abs(select(2, math.modf(365.7353548333)) - 0.73535483330488) < 1e-7 and
        math.modf(352.64282440767) == 352 and
        math.abs(select(2, math.modf(352.64282440767)) - 0.64282440766692) < 1e-7 and
        math.modf(256.04179827496) == 256 and
        math.abs(select(2, math.modf(256.04179827496)) - 0.041798274964094) < 1e-7 and
        math.modf(-278.28463632613) == -278 and
        math.abs(select(2, math.modf(-278.28463632613)) + 0.2846363261342) < 1e-7 and
        math.modf(-206.89806248993) == -206 and
        math.abs(select(2, math.modf(-206.89806248993)) + 0.89806248992682) < 1e-7 and
        math.modf(-76.712315436453) == -76 and
        math.abs(select(2, math.modf(-76.712315436453)) + 0.71231543645263) < 1e-7 and
        math.modf(205.99580463022) == 205 and
        math.abs(select(2, math.modf(205.99580463022)) - 0.99580463021994) < 1e-7 and
        math.modf(-13.91322305426) == -13 and
        math.abs(select(2, math.modf(-13.91322305426)) + 0.91322305426002) < 1e-7
end

function test17()
    return math.abs(math.rad(180) - math.pi) < 1e-7 and
           math.abs(math.rad(360) - 2*math.pi) < 1e-7 and
           math.abs(math.rad(15) - math.pi/12) < 1e-7
end

function test18()
    local good = true
    for i=1,10000,1 do
        local rand = math.random()
        good = good and rand >= 0 and rand < 1
    end

    for i=1,10000,1 do
        local rand = math.random(4, 20)
        good = good and rand >= 4 and rand <= 20
    end

    for i=1,10000,1 do
        local rand = math.random(35)
        good = good and rand >= 1 and rand <= 35
    end

    local numbers1 = {}
    math.randomseed(8675309)
    for i=1,10000,1 do
        numbers1[#numbers1 + 1] = math.random()
    end

    local numbers2 = {}
    math.randomseed(8675309)
    for i=1,10000,1 do
        numbers2[#numbers2 + 1] = math.random()
    end

    for i=1,10000,1 do
        good = good and numbers1[i] == numbers2[i]
    end

    -- `math.random(0)` should return a fully random integer.
    local val = math.random(0)
    good = good and is_integer(val)

    -- `math.random(i)` should error if i < 0
    local status = pcall(function()
        return math.random(-1)
    end)
    good = good and not status

    for i = 1, 10000, 1 do
        local bigboi, bigboi2 = math.random(math.maxinteger), math.random(0, math.maxinteger)
        good = good and bigboi > 0 and bigboi2 > -1
    end

    -- `math.random(a, b)` where b < a should error
    local status2 = pcall(function()
        return math.random(5, 3)
    end)
    good = good and not status2

    return good
end

function test19()
    return math.sin(0) == 0.0 and
           math.abs(math.sin(math.pi) - 0.0) < 1e-7 and
           math.abs(math.sin(math.pi/6) - 0.5) < 1e-7 and
           math.abs(math.sin(1) - 0.8414709848079) < 1e-7 and
           is_nan(math.sin(math.huge)) and
       not is_integer(math.sin(0))
end

function test20()
    return math.abs(math.sqrt(1.0) - 1.0) < 1e-7 and
           math.abs(math.sqrt(4.0) - 2.0) < 1e-7 and
           math.abs(math.sqrt(0.09) - 0.3) < 1e-7 and
           is_nan(math.sqrt(-3))
end

function test21()
    return math.tan(0) == 0.0 and
           math.abs(math.tan(math.pi) - 0.0) < 1e-7 and
           math.abs(math.tan(math.pi/6) - 0.57735026918963) < 1e-7 and
           math.abs(math.tan(1) - 1.5574077246549) < 1e-7 and
           is_nan(math.tan(math.huge)) and
       not is_integer(math.tan(0))
end

function test22()
    return math.tointeger(1.0) == 1 and
           math.tointeger(1.1) == nil and
           math.tointeger(-3.0) == -3 and
           math.tointeger(4.00000002) == nil and
           is_integer(math.tointeger(8.0))
end

function test23()
    return math.type(1) == "integer" and
           math.type(1.0) == "float" and
           math.type("1.0") == nil
end

function test24()
    return not math.ult(-3, 2) and
               math.ult(-3, -2) and
               math.ult(1, 2)
end

assert(
    test1() and
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
    test12() and
    test13() and
    test14() and
    test15() and
    test16() and
    test17() and
    test18() and
    test19() and
    test20() and
    test21() and
    test22() and
    test23() and
    test24()
)
