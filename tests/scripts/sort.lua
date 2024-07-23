
function is_sorted(list)
    for i = 2, #list do
        if list[i - 1] > list[i] then
            return false
        end
    end
    return true
end

do
    local t = { }
    table.sort(t)
    assert(#t == 0)

    t = { 4, 9, 3, 1 }
    table.sort(t)
    assert(#t == 4 and is_sorted(t))
end

do
    local list = {}
    for i = 1, 100 do
        table.insert(list, math.random())
    end

    table.sort(list)
    assert(#list == 100 and is_sorted(list))
end
