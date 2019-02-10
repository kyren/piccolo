function test1()
    return 2   + 3   == 5 and
           2.0 + 3.0 == 5 and
           2   + 3.0 == 5 and
           2.0 + 3   == 5
end

function test2()
    return 2   - 3   == -1 and
           2.0 - 3.0 == -1 and
           2   - 3.0 == -1 and
           2.0 - 3   == -1
end

function test3()
    return 2   * 3   == 6 and
           2.0 * 3.0 == 6 and
           2   * 3.0 == 6 and
           2.0 * 3   == 6
end

function test4()
    return 3   / 2   == 1.5 and
           3.0 / 2.0 == 1.5 and
           3   / 2.0 == 1.5 and
           3.0 / 2   == 1.5
end

function test5()
    return 7   // 3   == 2 and
           7.0 // 3.0 == 2 and
           7   // 3.0 == 2 and
           7.0 // 3   == 2
end

function test6()
    return 7   % 3   == 1 and
           7.0 % 3.0 == 1 and
           7   % 3.0 == 1 and
           7.0 % 3   == 1
end

function test7()
    return -7   % 3   == 2 and
           -7.0 % 3.0 == 2 and
           -7   % 3.0 == 2 and
           -7.0 % 3   == 2
end

function test8()
    return 3   ^ 4   == 81 and
           3.0 ^ 4.0 == 81 and
           3   ^ 4.0 == 81 and
           3.0 ^ 4   == 81
end

function test9()
    return 1   /  0   ~= -1 / 0 and
           1.0 /  0.0 ~= -1 / 0 and
           1   /  0.0 ~= -1 / 0 and
           1.0 /  0   ~= -1 / 0 and
           0   /  0   ~=  0 / 0 and
           0.0 /  0.0 ~=  0 / 0 and
           0   /  0.0 ~=  0 / 0 and
           0.0 /  0   ~=  0 / 0 and
           1   // 0   ~= -1 / 0 and
           1.0 // 0.0 ~= -1 / 0 and
           1   // 0.0 ~= -1 / 0 and
           1.0 // 0   ~= -1 / 0 and
           0   // 0   ~=  0 / 0 and
           0.0 // 0.0 ~=  0 / 0 and
           0   // 0.0 ~=  0 / 0 and
           0.0 // 0   ~=  0 / 0
end

function test10()
    return 1.0 % 0.0 ~= 1.0 % 0.0 and
           1   % 0.0 ~= 1   % 0.0 and
           1.0 % 0   ~= 1.0 % 0   and
           0.0 % 0.0 ~= 0.0 % 0.0 and
           0   % 0.0 ~= 0   % 0.0 and
           0.0 % 0   ~= 0.0 % 0
end

function test11()
    return  120 %  63 ==  57 and
           -120 % -63 == -57 and
            120 % -63 ==  -6 and
           -120 %  63 ==   6
end

function test12()
    return      1 < 2  and
           not (2 < 1) and
           not (1 < 1)
end

function test13()
    return -(-1) == 1 and
           not not true
end

function test14()
    return
        "1.0" + 1.0 == 2.0 and
        1.0 + "0x1.0" == 2.0 and
        "0x10.0" + "3.0" == 19.0 and
        "0x10" + "4" == 20
end

return
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
    test14()
