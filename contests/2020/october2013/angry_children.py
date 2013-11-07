import sys

#Given a list, returns minimum unfairness
def find_min(packets, K):
    packets.sort()
    current_min = packets[K-1] - packets[0]
    for i in range(1, len(packets) - K):
        if(current_min > packets[i + K - 1] - packets[i]):
            current_min = packets[i + K - 1] - packets[i]

    return current_min

N = int(sys.stdin.readline())
K = int(sys.stdin.readline())
packets = [0] * N
for n in range(0, N):
    packets[n] = int(sys.stdin.readline())

print(find_min(packets, K))
