
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

-- merge, but uses the given comparison function
local function merge_cmp(src, dst, lo, mid, hi, cmp)
    local i = lo
    local j = mid
    for k = lo, hi do
        if i < mid and (j > hi or cmp(src[i], src[j])) then
            dst[k] = src[i]
            i = i + 1
        else
            dst[k] = src[j]
            j = j + 1
        end
    end
end

-- sorts arr[lo ..= hi] (inclusive, one-indexed)
local function sort(arr, lo, hi, t, cmp)
    local n = hi - (lo - 1)
    if n <= 1 then
        return
    end
    local mid = lo + n // 2
    sort(arr, lo, mid - 1, t, cmp)
    sort(arr, mid, hi, t, cmp)
    for i = lo, hi do
        t[i] = arr[i]
    end
    if cmp == nil then
        merge(t, arr, lo, mid, hi)
    else
        merge_cmp(t, arr, lo, mid, hi, cmp)
    end
end

local arr, cmp = ...
local len = #arr
if len < 0 then
    len = 0
end

local temp = {}
sort(arr, 1, len, temp, cmp)
return
