import math

""" inserts primes <= N into primes"""
def get_primes(primes, N):
    for i in xrange(2, N+1):
        primes.add(i)
    for i in xrange(2, N+1):
        if i in primes:
            multiple = i * 2
            while multiple <= N:
                if multiple in primes:
                    primes.remove(multiple)
                multiple += i

""" Returns prime factorization of x """
def prime_factor(x, primes):
    factors = [x]
    ret = {}
    while len(factors) > 0:
        n = factors.pop()
        if n in primes:
            if n in ret:
                ret[n] += 1
            else:
                ret[n] = 1
        else:
            for p in primes:
                if n % p == 0:
                    factors.append(p)
                    factors.append(n / p)
                    break
    return ret

N = int(raw_input())
primes = set()
prime_count = { } 
get_primes(primes, N)
for i in xrange(2, N+1):
    temp = prime_factor(i, primes)
    for t in temp:
        temp[t] = temp[t] * 2
        if t in prime_count:
            prime_count[t] += temp[t]
        else:
            prime_count[t] = temp[t]
answer = 1
for prime in prime_count:
    answer = (answer * (prime_count[prime] + 1)) % 1000007
print answer
