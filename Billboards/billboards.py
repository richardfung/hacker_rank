import sys

def find_choices(length, prev_count, max_count): 
  if(length <= 0):
    yield ()

  #if next one has to be 0
  elif(prev_count == max_count):
    for x in find_choices(length - 1, 0, max_count):
      yield x + (0,)

  #if it can be either 0 or 1
  else:
    #let it be 0
    for x in find_choices(length - 1, 0, max_count):
      yield x + (0,)
    for x in find_choices(length - 1, prev_count + 1, max_count):
      yield x + (1,)

def dot_prod(weights, boards):
  if(not hasattr(dot_prod, "results")):
    dot_prod.results = {(): 0}

  if(boards in dot_prod.results):
    return dot_prod.results[boards]

  

  res = weights[0] * boards[0] + dot_prod(weights[1:], boards[1:])
  dot_prod.results[boards] = res
  return res
      
#nk = sys.stdin.readline().split()
#(N, K) = (nk[0], nk[1])

big  = 0
w = [1, 2, 3, 1, 6, 10]
for x in find_choices(6, 0, 2):
  i = dot_prod(w, x)
  if(i > big):
    big = i

print big
