local function error_callback(err_msg) -- This is line 1 (base-1)
    -- Padding comment on line 2 (base-1)
    -- Padding comment on line 3 (base-1)
    error(err_msg) -- This is line 4 (base-1)
end

local function baz(a, b) -- This is line 7 (base-1)
    -- Padding comment on line 8 (base-1)
    error_callback(a .. b) -- This is line 9 (base-1)
end

local function bar(x, ...) -- This is line 12 (base-1)
    baz(x, ...) -- This is line 13 (base-1)
end

local function foo(arg1) -- This is line 16 (base-1)
    -- Padding comment on line 17 (base-1)
    -- Padding comment on line 18 (base-1)
    bar(arg1, " error from rust") -- This is line 19 (base-1)
end

foo("test") -- This is line 22 (base-1)