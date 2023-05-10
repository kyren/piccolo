local meta = {}

function meta.__call()
    return 5
end

local test1 = {}
setmetatable(test1, meta)

function test2()
  local function seq()
      local meta = {}
      function meta.__call(_, s, c)
          return c + 1
      end
      local inc = {}
      setmetatable(inc, meta)
      return inc, nil, 1
  end

  for i in seq() do
    if i == 7 then
      return 7
    end
  end
end

assert(
  test1() == 5 and
  test2() == 7
)
