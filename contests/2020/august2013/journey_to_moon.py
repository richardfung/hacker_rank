import sys

#given a list of pairs, returns a list of counts of people in each country
def count_countries(pairs):
    ret = []
    #keep going while we still have things to add
    while len(pairs) > 0:
        #add count for new country
        ret.append(0)
        #keep track of people in current country and add first people
        people = set(pairs.pop()) 
        #increment for those two we just added
        ret[-1] += 2

        #Go until nothing in pairs matches a person we've seen
        done = False 
        while not done:
            stack = []
            #Go backwards so that it's ok for us to pop elements
            for i in range(len(pairs) -1, -1, -1):
                if pairs[i][0] in people or pairs[i][1] in people: 
                    stack.append(pairs.pop(i))
            #if nothing was added then we are done
            if len(stack) == 0:
                done = True
            #add the pairs to people and increase count as necessary
            while len(stack) > 0:
                p = stack.pop()
                #increment count for new people
                for person in p:
                    if not person in people:
                        ret[-1] += 1
                
                #add p to people
                people = people | set(p)
    return ret

#Counts number of ways to arrange pairs of people
def count_ways(country_counts):
    #Set up saved values
    if not hasattr(count_ways, "saved"):
        count_ways.saved = { tuple([0] * len(country_counts)) : 1}

    if tuple(country_counts) in count_ways.saved:
        return count_ways.saved[tuple(country_counts)]

    if len(country_counts) == 1:
        return 0

    ways = 0
    country_counts[0] += -1
    for i in range(1, len(country_counts)):
        if country_counts[i] > 0:
            country_counts[i] += -1
            ways += count_ways(country_counts)
            country_counts[i] += 1

    #Revert country_counts
    country_counts[0] += 1

    #Add to saved
    count_ways.saved[tuple(country_counts)] = ways
    return ways

if __name__ == "__main__":
    NI = sys.stdin.readline().split()
    (N, I) = (int(NI[0]), int(NI[1]))
    pairs = []
    i = 0
    while i < I:
        pair = sys.stdin.readline().split()
        pairs.append((pair[0], pair[1]))
        i += 1

    country_count = count_countries(pairs)
    print(count_ways(country_count))
