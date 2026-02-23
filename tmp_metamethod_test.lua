-- full metamethod smoke test
local mt = {
  __add = function(a,b) return a.n + b.n end,
  __sub = function(a,b) return a.n - b.n end,
  __mul = function(a,b) return a.n * b.n end,
  __div = function(a,b) return a.n / b.n end,
  __mod = function(a,b) return a.n % b.n end,
  __pow = function(a,b) return a.n ^ b.n end,
  __eq  = function(a,b) return a.n == b.n end,
  __lt  = function(a,b) return a.n < b.n end,
  __le  = function(a,b) return a.n <= b.n end,
  __concat = function(a,b) return a.s .. b.s end,
  __len = function(a) return 99 end,
  __tostring = function(_) return "OBJ" end,
}

local a = setmetatable({n=9, s="x"}, mt)
local b = setmetatable({n=4, s="y"}, mt)

assert(a + b == 13)
assert(a - b == 5)
assert(a * b == 36)
assert(a / b == 2.25)
assert(a % b == 1)
assert(a ^ b == 6561.0)
assert(a ~= b)
assert(a == setmetatable({n=9, s="x"}, mt))
assert(not (a < b))
assert(a <= setmetatable({n=9, s="z"}, mt))
assert(a .. b == "xy")
assert(#a == 99)
assert(tostring(a) == "OBJ")

local idx_t = setmetatable({}, { __index = {k = 7} })
assert(idx_t.k == 7)

local sink = {}
local newidx_t = setmetatable({}, { __newindex = sink })
newidx_t.hello = 42
assert(sink.hello == 42)

local callable = setmetatable({base=10}, { __call = function(self, x) return self.base + x end })
assert(callable(5) == 15)

local pt = setmetatable({}, { __pairs = function(_) return pairs({k=9}) end })
local sum = 0
for _,v in pairs(pt) do sum = sum + v end
assert(sum == 9)

local it = setmetatable({}, { __ipairs = function(_) return ipairs({4,5}) end })
local isum = 0
for _,v in ipairs(it) do isum = isum + v end
assert(isum == 9)

print("lua file test passed")
return "OK"
