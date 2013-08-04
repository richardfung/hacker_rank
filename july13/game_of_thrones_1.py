#!/usr/local/bin/python3.3

import sys
import array

str = sys.stdin.readline()
count = array.array('i', [0]*26)

for c in str[:-1] :
  count[ord(c) - ord('a')] += 1

one = 0
for i in count:
  if (i%2 != 0):
    one += 1

if(one > 1):
  print ("NO")
else:
  print ("YES")
