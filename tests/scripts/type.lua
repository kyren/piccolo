function nil_type()
    local empty_call = pcall(function()
        return type()
    end)
    return type(nil) == "nil" and not empty_call
end

function number_type()
    return type(3) == "number" and type(-3) == "number" and
        type(1e10) == "number" and type(1.2) == "number"
end

function boolean_type()
    return type(true) == "boolean" and type(false) == "boolean"
end

function string_type()
    return type("") == "string"
end

function table_type()
    return type({}) == "table"
end

function function_type()
    return type(function() end) == "function"
end

function thread_type()
    main = coroutine.running()
    t1 = coroutine.create(function() end)
    return type(main) == "thread" and type(t1) == "thread"
end

assert(
    number_type() and
    nil_type() and
    function_type() and
    thread_type() and
    table_type() and
    boolean_type()
)
