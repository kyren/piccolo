function test_coroutine()
    coroutine.yield(1)
    coroutine.yield(2)
    coroutine.yield(3)
end

co = coroutine.create(test_coroutine)

print(coroutine.resume(co))
print(coroutine.status(co))
print(coroutine.resume(co))
print(coroutine.status(co))
print(coroutine.resume(co))
print(coroutine.status(co))
print(coroutine.resume(co))
print(coroutine.status(co))
