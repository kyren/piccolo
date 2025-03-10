
local function array_generator(arr)
    local i = 0
    return function()
        i = i + 1
        return arr[i]
    end
end

local primitives = { ["nil"] = 0, number = 0, string = 0, boolean = 0, ["function"] = 0, thread = 0 }
local function cmp_array_recurse(a, b, top)
    local a_ty = type(a)
    local b_ty = type(b)
    if a_ty ~= b_ty then
        return false
    end
    if primitives[a_ty] ~= nil then
        return a == b
    end
    if rawlen(a) ~= rawlen(b) then
      return false
    end
    for i = 1, rawlen(a) do
        if not cmp_array_recurse(rawget(a, i), rawget(b, i), false) then
            return false
        end
    end
    return true
end

local log_arr = {}
function log(val)
    table.insert(log_arr, val)
end

do
    log_arr = {}

    local read_func = array_generator({ "log(1)", "log(2)", "log(3)" })
    local f, err = load(read_func)
    assert(f, err)
    f()

    assert(cmp_array_recurse(log_arr, { 1, 2, 3 }))
end

do
    log_arr = {}

    local read_func = array_generator({ "log(", 1, ")", "log(", 1.5, ")" })
    local f, err = load(read_func)
    assert(f, err)
    f()

    assert(cmp_array_recurse(log_arr, { 1, 1.5 }))
end

do
    log_arr = {}

    local read_func = array_generator({ [[log("]], {}, [[")]] })
    local f, err = load(read_func)
    assert(f == nil and err ~= nil, "PRLua does not implicitly convert tables to strings in load")
end

do
    -- PRLua does not support the call metamethod for load
    local callable = setmetatable({}, {
        __call = array_generator({ "log(3)" })
    })
    local res, err = pcall(function()
        local f, err = load(callable)
        assert(f == nil and err ~= nil)
    end)
    -- PRLua throws an actual error here (not caught by load itself),
    -- but this seems underspecified
    assert(not res)
end

-- Environment Tests
-- By default, load uses the global context
do
    local res = load("return _G")()
    assert(res == _G)
end

-- loaded code does not have access to the local scope of the caller
do
    local a = 15
    local res = load("return a")()
    assert(res == nil)
end

-- The value of _G does not affect the load logic
do
    local old_globals = _G
    _G = {}

    local res = load("return _G")()
    assert(res == _G)

    _G = old_globals
end

-- Load defaults to the global context, even from places where the global
-- context is restricted
do
    log_arr = {}

    local f, err = load([[
        local inner = load("log(32)")
        inner()
        log(16)
    ]], "name", "t", { load = load })
    assert(f, err)

    local ok, res = pcall(function() f() end)
    assert(not ok)

    assert(cmp_array_recurse(log_arr, { 32 }))
end

do
    local module = {}
    local f, err = load([[
        a = 1
        b = 2
        c = { [1] = "a" }
    ]], "name", "t", module)
    assert(f, err)

    f()

    assert(module.a == 1 and module.b == 2 and module.c[1] == "a")
end

-- Basic argument handling
do
    local f, err = load([[
        local args = table.pack(...)
        for i = 1, args.n do
            log(args[i])
        end
        return args[1]
    ]])
    assert(f, err)

    log_arr = {}
    local r = f("a", "b", "c")
    assert(r == "a")
    assert(cmp_array_recurse(log_arr, { "a", "b", "c" }))
end
