do
  local t1 = {1, 2, 3, 4, nil, 6, 7, 8}
  local t2 = {}

  for k, v in ipairs(t1) do
    t2[k] = v
  end

  for i = 1,4 do
    assert(t2[i] == i)
  end

  assert(t2[5] == nil)
  assert(#t2 == 4)
end

do
  local t3 = {1, 2, 3, "a", b = "b", 5, 6}
  local t4 = {}

  for k, v in pairs(t3) do
    t4[k] = v
  end

  for i = 1,3 do
    assert(t4[i] == i)
  end

  assert(t4[4] == 'a')

  for i = 5,6 do
    assert(t4[i] == i)
  end

  assert(t4['b'] == 'b')
end

do
  local t = {}
  setmetatable(t, {
    __index = function(_, key)
      if key <= 10 then
        return key
      else
        return nil
      end
    end
  })

  assert(t[2] == 2)
  assert(t[5] == 5)
  assert(t[11] == nil)

  local t2 = {}

  for k,v in ipairs(t) do
    t2[k] = v
  end

  for i = 1,10 do
    assert(t2[i] == i, i)
  end
end

do
  local t = { [math.mininteger] = 4 }
  local inext = ipairs(t)
  local a, b = inext(t, math.maxinteger)
  assert(a == -9223372036854775808 and b == 4)
end

do
  local t = {}
  setmetatable(t, {
    __pairs = function()
        return 1, 2, 3, 4
      end
  })
  local a, b, c, d = pairs(t)
  assert(a == 1)
  assert(b == 2)
  assert(c == 3)
  assert(d == nil)
  setmetatable(t, {
    __pairs = function()
        return 1, 2
      end
  })
  local a, b, c = pairs(t)
  assert(a == 1)
  assert(b == 2)
  assert(c == nil)
end
