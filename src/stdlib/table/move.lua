
-- A basic implementation of table.move, directly ported from
-- PUC-Rio Lua's ltablib.c.  The original file's license is included
-- here:

--[[
/******************************************************************************
* Copyright (C) 1994-2024 Lua.org, PUC-Rio.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/
]]

src, src_base, end_idx, dst_base, dst = ...

if dst == nil then
    dst = src
end

src_base = math.tointeger(src_base)
end_idx = math.tointeger(end_idx)
dst_base = math.tointeger(dst_base)

if src_base == nil or end_idx == nil or dst_base == nil then
    error("arguments to move must be integers")
end
if type(src) ~= "table" or type(dst) ~= "table" then
    error("arguments to move must be tables")
end

if end_idx < src_base then
    return dst
end

if src_base <= 0 and end_idx >= math.maxinteger + src_base then
    error("too many elements to move") -- n would overflow
end

local n = end_idx - src_base + 1   -- number of elements to move

if dst_base > math.maxinteger - n + 1 then
    error("destination wrap around")
end

if dst_base > end_idx or dst_base <= src_base or dst ~= src then
    for i = 0, n - 1 do
        dst[dst_base + i] = src[src_base + i]
    end
else
    for i = n - 1, 0, -1 do
        dst[dst_base + i] = src[src_base + i]
    end
end

return dst
