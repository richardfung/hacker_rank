class Block:
    def __init__(self, x, y):
        self.x = x
        self.y = y

def find_cost(blocks):
    if len(blocks) == 0:
        return 0
    best = None
    for block in blocks:
        copy = set(blocks)
        copy.remove(block)
        cost = bnb(copy, 0, best, block.x, block.y, block.x, block.y)
        if best == None or cost < best:
            best = cost
    return cost

def bnb(blocks, current, best, min_x, min_y, max_x, max_y):
    if len(blocks) == 0:
        return current
    ret = current
    to_remove = set()
    #First, go through the ones where the min/max don't change
    if min_x != None:
        for block in blocks:
            if block.x >= min_x and block.x <= max_x\
                    and block.y >= min_y and block.y <= max_y:
                ret += max( (abs(max_x - block.x), abs(min_x - block.x),
                        abs(max_y - block.y), abs(min_y - block.y)) )
                to_remove.add(block)
    for block in to_remove:
        blocks.remove(block)
    lowest_cost = None
    if len(blocks) == 0:
        return ret
    for block in blocks:
        copy = set(blocks)
        copy.remove(block)
        if min_x != None:
            temp_cost = max( (abs(max_x - block.x), abs(min_x - block.x),
                    abs(max_y - block.y), abs(min_y - block.y)) )
            temp_current = temp_cost + ret
            if best != None and temp_current >= best:
                continue
            temp_current = bnb(copy, temp_current, best, 
                    min(min_x, block.x), 
                    min(min_y, block.y), 
                    max(max_x, block.x), 
                    max(max_y, block.y))
        else:
            temp_cost = bnb(copy, ret, best, block.x, block.y, block.x, 
                    block.y)
            temp_current = ret + temp_cost
        if lowest_cost != None:
            if temp_current < lowest_cost :
                lowest_cost = temp_current 
        else:
            lowest_cost = temp_current
    if lowest_cost != None:
        ret = lowest_cost
    #This should only happen if all of them were >= best
    else:
        ret = temp_current
    return ret

r = int(raw_input().split()[0])
blocks = set()
for i in xrange(r):
    row = raw_input().split()
    for j in xrange(len(row)):
        if row[j] == '1':
            blocks.add(Block(i, j))
print find_cost(blocks)
