
-- a basic merge sort implementation

-- merges src[lo..mid] and src[mid..=hi] into dst[lo..=hi]
local function merge(src, dst, lo, mid, hi)
    local i = lo
    local j = mid
    for k = lo, hi do
        if i < mid and (j > hi or src[i] < src[j]) then
            dst[k] = src[i]
            i = i + 1
        else
            dst[k] = src[j]
            j = j + 1
        end
    end
end

-- sorts arr[lo ..= hi] (inclusive, one-indexed)
local function sort(arr, lo, hi, t)
    local n = hi - (lo - 1)
    if n <= 1 then
        return
    end
    local mid = lo + n // 2
    sort(arr, lo, mid - 1, t)
    sort(arr, mid, hi, t)
    for i = lo, hi do
        t[i] = arr[i]
    end
    merge(t, arr, lo, mid, hi)
end

local arr = ...
local len = #arr

local temp = {}
sort(arr, 1, len, temp)
return
