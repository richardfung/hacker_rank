import sys

#Given a list, returns minimum unfairness
def find_min(packets, K):
    packets.sort()
    helper = make_unfairness_array(K)
    
    current_min = 0
    for j in range(0, K):
        current_min += packets[j] * helper[j]

    for i in range(1, len(packets) - K + 1):
        temp = 0
        for j in range(0, K):
            temp += packets[i + j] * helper[j]
        if temp < current_min:
            current_min = temp

    return current_min 

""" Returns an array of size K such that the dot product of the returned array
    and a sorted array of packets gives the unfairness
"""
def make_unfairness_array(K):
    ret = [0] * K
    for i in range(0, K):
        ret[i] = len(ret[0:i]) - len(ret[i+1:])

    return ret

N = int(sys.stdin.readline())
K = int(sys.stdin.readline())
packets = ([0] * N)
for n in range(0, N):
    packets[n] = int(sys.stdin.readline())

print(find_min(packets, K))
