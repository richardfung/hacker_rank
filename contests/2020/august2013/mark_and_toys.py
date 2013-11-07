import sys

money = int(sys.stdin.readline().split()[1])
prices = sys.stdin.readline().split()
for i in range(len(prices)):
    prices[i] = int(prices[i])
prices.sort()
count = 0

while count < len(prices) and money >= prices[count]:
    money = money - prices[count]
    count += 1

print(count)
