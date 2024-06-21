
local primitives = { ["nil"] = 0, number = 0, string = 0, boolean = 0, ["function"] = 0, thread = 0 }
local function cmp_array_recurse(a, b)
    local a_ty = type(a)
    local b_ty = type(b)
    if a_ty ~= b_ty then
        return false
    end
    if primitives[a_ty] ~= nil then
        return a == b
    end
    if #a ~= #b then
      return false
    end
    for i = 1, #a do
        if not cmp_array_recurse(a[i], b[i]) then
            return false
        end
    end
    return true
end

do
    local function add(a, b) return { "add", a, b } end
    local function sub(a, b) return { "sub", a, b } end
    local function mul(a, b) return { "mul", a, b } end
    local function div(a, b) return { "div", a, b } end

    local mt = {
        __add = add,
        __sub = sub,
        __mul = mul,
        __div = div,
    }

    local a = { "a" }
    local b = { "b" }
    local c = { "c" }

    setmetatable(a, mt)
    setmetatable(b, mt)
    setmetatable(c, mt)

    local res

    res = a + b * c - a
    assert(cmp_array_recurse(res, { "sub", { "add", { "a" }, { "mul", { "b" }, { "c" } } }, { "a" } }))

    res = c / a
    assert(cmp_array_recurse(res, { "div", { "c" }, { "a" } }))

    res = a * a
    assert(cmp_array_recurse(res, { "mul", { "a" }, { "a" } }))
end
