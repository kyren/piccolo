local input = "./tests/scripts/io-test-input"
local output = "./tests/scripts/io-test-output"

do
  io.input(io.stdin)
  assert(io.read("0") == "")
  io.input(input)
  assert(io.read("l") == "0xFFFFF")
  io.input():close()
  assert(io.type(io.input()) == "closed file")
  io.output(output)
  file = io.write("This is output?"):write("\n")
  file:flush()
  file:close()
  assert(io.type(file) == "closed file")
  assert(io.type(io.output()) == "closed file")
end

do
  local file = io.open(output, "w")
  file:write("First Line\n"):write("Second Line\n"):write("Some number: 12345\n"):write("777\n")
  file:flush()
  file:close()
  assert(io.type(file) == "closed file")
  local file = io.open(output, "r")
  assert(file:read("l") == "First Line")
  assert(file:read("l") == "Second Line")
  assert(file:read("l") == "Some number: 12345")
  file:flush()
  file:close()
end

do
  local file = io.open(output, "r")
  assert(file:read("L") == "First Line\n")
  assert(file:read("a") == "Second Line\nSome number: 12345\n777\n")
  file:close()
  local file = io.open(output, "r")
  assert(file:read("l") == "First Line")
  assert(file:read("l") == "Second Line")
  assert(file:read("n") == nil)
  assert(file:read("L") == "Some number: 12345\n")
  assert(file:read("n") == 777)
  file:flush()
  file:close()
end

do
  local file = io.open(output, "r")
  local a, b, c, d, e = file:read("n", "l", "l", "L", "n")
  assert(a == nil)
  assert(b == "First Line")
  assert(c == "Second Line")
  assert(d == "Some number: 12345\n")
  assert(e == 777)
  file:close()
end

do
  local file = io.open(output, "r")
  file:seek("set", 1)
  assert(file:read("l") == "irst Line")
  file:seek("cur", 1)
  assert(file:read("l") == "econd Line")
  file:seek("set", 0)
  assert(file:read("l") == "First Line")
  file:seek("cur", 2)
  assert(file:read("l") == "cond Line")
  file:seek("end", -5)
  assert(file:read("n") == 777)
  file:seek("set", 11)
  file:seek("cur", -2)
  assert(file:read("l") == "e")
  file:seek("set", 0)
  local a, b = file:read("l", "l")
  assert(a == "First Line")
  assert(b == "Second Line")
  file:seek("set", 35)
  assert(file:read("n") == 12345)
  file:seek("set", 15)
  assert(file:read(10)== "nd Line\nSo")
  file:close()
end

do
  tmp = io.tmpfile()
  tmp:write("1234\n"):write("4321\n")
  assert(tmp:read("l") == "1234")
  assert(tmp:read("n") == 4321)
end