import sys

def find_choices(length, prev_count, max_count): 
  if(not hasattr(find_choices, "results")):
    find_choices.results = {(0, 0) : []}

  if(length <= 0):
    yield ()

  elif((length, prev_count) in find_choices.results):
    for res in find_choices.results[(length, prev_count)]:
      yield res

  #if next one has to be 0
  elif(prev_count == max_count):
    find_choices.results[(length, prev_count)] = []
    for x in find_choices(length - 1, 0, max_count):
      res = x + (0,)
      find_choices.results[(length, prev_count)].append(res)
      yield res


  #if it can be either 0 or 1
  else:
    #initialize results
    find_choices.results[(length, prev_count)] = []

    #let it be 0
    for x in find_choices(length - 1, 0, max_count):
      res = x + (0,)
      find_choices.results[(length, prev_count)].append(res)
      yield res
    #let it be 1
    for x in find_choices(length - 1, prev_count + 1, max_count):
      res = x + (1,)
      find_choices.results[(length, prev_count)].append(res)
      yield res

def dot_prod(weights, boards):
  if(not hasattr(dot_prod, "results")):
    dot_prod.results = {(): 0}

  if(boards in dot_prod.results):
    return dot_prod.results[boards]

  res = weights[0] * boards[0] + dot_prod(weights[1:], boards[1:])
  dot_prod.results[boards] = res
  return res
      
nk = sys.stdin.readline().split()
(N, K) = (int(nk[0]), int(nk[1]))

i = 0
w = []
while i < N:
  w.append(int(sys.stdin.readline()))
  i = i + 1

big = 0
for x in find_choices(N, 0, K):
  res = dot_prod(w, x)
  if(res > big):
    big = res

print big
