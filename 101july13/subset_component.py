import Queue
import sys

class Node(object):
    def __init__(self):
        self.neighbors = set()

class Graph(object):
    def __init__(self):
        self.nodes = []

def convert_to_graph(int_list):
    #label nodes
    graph = Graph()
    #Add nodes from beginning
    i = 0
    while i < 64:
        node = Node()
        graph.nodes.append(node)
        i += 1

    i = 0
    #for every given number
    while i < len(int_list):
        ones = set()
        bit_pos = 0
        #add all neighbors
        while bit_pos < 64:
            if (int_list[i] >> bit_pos) & 1 > 0:
                ones.add(bit_pos)
            bit_pos += 1

        #Add the neighbors to nodes in graph
        for pos in ones:
            neighbors = set(ones)
            neighbors.remove(pos)
            graph.nodes[pos].neighbors = \
                graph.nodes[pos].neighbors.union(neighbors)
        i += 1

    return graph

def count_ccs(graph):
    count = 0
    visited_nodes = [False] * len(graph.nodes)
    i = 0
    #For every unseen node
    while i < len(visited_nodes):
        queue = []
        #If we haven't traversed it yet add to queue
        if not visited_nodes[i]: 
            queue.append(i)
            #Increase our count for connected component
            count += 1
        #traverse the entire connected component
        while len(queue) > 0:
            current_node = queue.pop()
            #Mark this node as visited
            visited_nodes[current_node] = True
            #Go to all neighboring nodes
            for neighbor in graph.nodes[current_node].neighbors:
                if not visited_nodes[neighbor]:
                    queue.append(neighbor)
        i += 1

    return count

def subsets(int_list):
    #note: len(int_list) > 0 always
    i = 0
    while i < (1 << len(int_list)):
        ret = []
        bit_pos = 0
        #if we haven't hit the end of i already
        while i >> bit_pos > 0:
            if (i >> bit_pos) & 1:
                ret.append(int_list[bit_pos])
            bit_pos += 1
        
        yield ret
        i += 1
     
numbers = []
n = int(sys.stdin.readline())
for i in str.split(sys.stdin.readline()):
    numbers.append(int(i))

total = 0
for subset in subsets(numbers):
    total += count_ccs(convert_to_graph(subset))

print total 
    
