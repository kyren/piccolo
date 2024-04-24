--- error
--- Runtime(RuntimeError(compiler error at line 7: cannot assign to a const variable))
---
local a <const> = 5

local function b()
    function a()
    end
end
