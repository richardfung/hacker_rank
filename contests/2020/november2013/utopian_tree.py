def cycle(x):
    ret = 1
    for i in xrange(x):
        ret = ret << 1
        ret = ret + 1
    return ret

T = int(raw_input())
for t in xrange(T):
    N = int(raw_input())
    answer = cycle(N/2)
    if N % 2:
        answer = answer << 1
    print answer
