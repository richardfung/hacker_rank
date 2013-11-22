import math

def power(x, y, m):
    ret = 1
    while y > 0:
        if y & 1 == 1:
            ret = (ret * x) % m
        x = (x * x) % m
        y = y >> 1
    return ret

T = int(raw_input())
m = 1000000007
for t in xrange(T):
    (A, B) = raw_input().split()
    A = int(A) % m
    B = int(B) % (m - 1)
    print power(A, B, m)
