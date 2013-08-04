import sys
import math

T = int(sys.stdin.readline())
t = 0
while t < T:
  NK = sys.stdin.readline().split()
  (N, K) = (int(NK[0]), int(NK[1]))

  right = pow(2, N-1, pow(5,K) << K)

  digits_on_right = math.ceil((N-1) * math.log10(2)) - K
  left = (1 << (N-1 - digits_on_right)) // pow(5, digits_on_right)

  print(left + right)

  t += 1


