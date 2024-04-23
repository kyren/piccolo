-- The only whitelisted attributes
-- are `const` and `close`.
--
-- Any other attribute should be a parse error.

local ccount = 0
local function makecloser(closer)
    local closermeta = {}
    function closermeta:__close()
        if self.ccount == ccount then
            ccount = ccount + 1
        end
    end

    setmetatable(closer, closermeta)
    return closer
end

do
    local a <const>, _c, b <close> = 3, nil, makecloser({ ccount = 1 })
    local c <close> = makecloser({ ccount = 0 })
    -- TODO How to test things that should not
    -- compile?
    -- Any reassignment to the above variables
    -- should fail at the compiler stage (before something
    -- can be pcalled)
    -- As in pcall(function() a = 3 end) should still fail
    --
    -- Also, any local statement is only allowed to have
    -- 1 close attribute variable
end
-- TODO This assert should pass when
-- close attribute is implemented
-- assert(ccount == 2)
