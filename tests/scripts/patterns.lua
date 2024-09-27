

local s, e = string.find("]]]", "[]]")
assert(s == 1, e == 1)

local s, e = string.find("]]]a", "[^]]")
assert(s == 4, e == 4)

assert(string.find("]]]", "[^]]") == nil)

string.gsub("abcd", "", "@")

-- run_match(b"z?ab", b"abc");
-- run_match(b"aab", b"abc");
-- run_match(b"a?a?a?aaa", b"aaa");
-- run_match(b".*bbb", b"aaaabbb");
-- run_match(b".-b$", b"aaaabbb");

-- run_match(b"((a).-)b$", b"aaaabbb");

-- run_match(b".+bc?", b"ab");
-- run_match(b"%a*", b"aLo_ALO");

-- local cursed = string.rep("a?", 64) .. string.rep("a", 64)
-- local value = string.rep("a", 64)

-- print(string.find(value, cursed))


assert(string.match("a", "a-b") == nil)
assert(string.match("aaa", "^a-$") == "aaa")
