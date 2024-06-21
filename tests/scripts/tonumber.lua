do
  assert(tonumber("3.4e0") == 3.4)
  assert(tonumber("-3.4") == -3.4)
  assert(tonumber("3.4e1") == 34)
  assert(tonumber("3e.1") == nil)
  assert(tonumber("3..") == nil)
  assert(tonumber("3ee") == nil)
  assert(tonumber("3f") == nil)
  assert(tonumber("3.4", 10) == nil)
  assert(tonumber("a2", 16) == 162)
  assert(tonumber("A2", 16) == 162)
  assert(tonumber("Z0", 36) == 36 * 35)
  assert(tonumber({}) == nil)
end
