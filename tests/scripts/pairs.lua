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
