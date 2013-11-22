#!/usr/bin/py
# Head ends here

def findStrings(strings, queries):
    starts = []
    get_starts(starts, strings)
    starts.sort()
    sorted_queries = sorted(queries)
    answers = {}
    seen = 0
    next_value = len(starts[0])
    next_index = 0
    for i in xrange(len(sorted_queries)):
        while sorted_queries[i] > seen + next_value\
                and next_index + 1 < len(starts):
            seen += next_value
            next_value = count(starts[next_index], starts[next_index + 1])
            next_index += 1
        if sorted_queries[i] <= seen + next_value:
            nth = sorted_queries[i] - seen
            answers[sorted_queries[i]] = get_nth(nth, starts[next_index - 1], 
                    starts[next_index])
        else:
            if not sorted_queries[i] in answers:
                answers[sorted_queries[i]] = "INVALID"
    for i in xrange(len(queries)):
        print answers[queries[i]]

#n must be less than len(s1) from before
def get_nth(n, s0, s1): 
    start = len(s1) - count(s0, s1)
    return s1[0:start + n]

def count(s0, s1):
    length = min(len(s0), len(s1))
    ret = 0
    while ret < length and s0[ret] == s1[ret]:
        ret += 1
    return len(s1) - ret

def get_starts(starts, strings):
    for s in strings:
        for i in xrange(len(s)):
            starts.append(s[i:])


# Tail starts here
if __name__ == '__main__':
    n = input()
    string=[]
    for i in range(0,n):
        string.append(raw_input())
    q= input()
    query=[]
    for i in range(0,q):
        query.append(input())
    findStrings(string,query)
