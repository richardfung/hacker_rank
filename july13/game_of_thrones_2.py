#!/usr/local/bin/python3.3

import sys
import array
import math

def fact(x):
  if(not hasattr(fact, "old")):
    fact.old = {1:1}

  #find closest match
  _min = -1
  for k in fact.old.keys():
    if abs(x - k) < _min or _min == -1:
      _min = abs(x - k)
      _argmin = k

  val = fact.old[_argmin]
  while _argmin > x:
    val = val // _argmin
    _argmin -= 1
  
  while _argmin < x:
    _argmin += 1
    val = val * _argmin

  fact.old[x] = val
  return val

str = sys.stdin.readline()
count = [0]*26
modulus = pow(10, 9) + 7

for c in str[:-1] :
  count[ord(c) - ord('a')] += 1

one = 0
for i in range(len(count)):
  if (count[i]%2 != 0):
    one += 1
    count[i] -= 1

if(one > 1):
  print ("0")
else:
  #by here we should have already removed the single middle char if any

  #consider only half of string and find total 
  total = 0
  for i in range(len(count)):
    count[i] = count[i] // 2
    total += count[i]

  bot = 1
  for i in count:
    bot = bot * fact(i)

  top = fact(total)

  print((top//bot) % (pow(10,9) + 7))
