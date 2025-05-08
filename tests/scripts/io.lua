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
  assert(io.type(file) == "file")
  assert(io.type(io.output()) == "file")
end

do
  local file = io.open(output, "w")
  file:write("First Line\n"):write("Second Line\n"):write("Some number: 12345\n")
  file:flush()
  file:close()
  assert(io.type(file) == "closed file")
  local file = io.open(output, "r")
  assert(file:read("l") == "First Line")
  assert(file:read("l") == "Second Line")
  assert(file:read("l") == "Some number: 12345")
  file:close()
end

do
  local file = io.open(output, "r")
  local content = file:read("a")
  print(content)
  file:close()
end

