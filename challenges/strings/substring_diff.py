def longest_match(str1, str2):
    mismatches = [-1]
    length = min(len(str1), len(str2))
    for i in xrange(length):
        if str1[i] != str2[i]:
            mismatches.append(i)
    mismatches.append(length)

    #If whole string is ok
    if len(mismatches) - 2 <= K:
        return length

    ret = 0
    for i in xrange(0, len(mismatches) - K - 1):
        ret = max(mismatches[i + K + 1] - mismatches[i], ret)
    return ret - 1

T = int(raw_input())
for t in xrange(0, T):
    longest = 0
    KPQ = raw_input().split()
    K, P, Q = int(KPQ[0]), KPQ[1], KPQ[2]
    for i in xrange(1, len(P) - 1):
        longest = max(longest, longest_match(P[i:], Q))
        longest = max(longest, longest_match(P, Q[i:]))
    longest = max(longest, longest_match(P, Q))
    print longest
