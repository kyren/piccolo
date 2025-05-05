--- error
--- Runtime(RuntimeError(compiler error at line 8: cannot assign to a const variable))
---

local a <const> = 3

function b()
    a = 4
end
