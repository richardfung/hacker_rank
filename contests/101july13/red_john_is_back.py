import math
import sys

def comb(n,k):
    larger = max(k, n-k)
    ret = 1
    for i in xrange(larger + 1, n + 1):
        ret = ret * i

    for i in xrange(1, n - larger + 1):
        ret = ret / i

    return ret

def find_ways(N):
    v = N%4
    ways = 0
    while v <= N:
        ways += comb((N + 3*v)/4, v)
        v += 4

    return ways

#finds primes <= n
def primes(n):
    if n > 1:
        n = n + 1
        sieve = [True] * n
        for i in xrange(3,int(n**0.5)+1,2):
            if sieve[i]:
                sieve[i*i::2*i]=[False]*((n-i*i-1)/(2*i)+1)
        return len([2] + [i for i in xrange(3,n,2) if sieve[i]])  
    return 0

T = int(sys.stdin.readline())
t = 0
while t < T:
    N = int(sys.stdin.readline()) 
    print primes(find_ways(N))
    t += 1
