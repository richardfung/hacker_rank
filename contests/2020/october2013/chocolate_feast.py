import sys

#Find total number of chocolates he can get if he has N dollars, C is the price
#of one chocolate, and you have to turn in M wrappers to get a free chocolate
def find_total(N, C, M):
    #initial value for chocolates and wrappers
    chocolates = N//C
    wrappers = chocolates
    #While he's eligible for more chocolate
    while wrappers >= M:
        new_chocolates = wrappers // M
        chocolates += new_chocolates
        wrappers = (wrappers % M) + new_chocolates

    return chocolates

T = int(sys.stdin.readline())
for i in range(0,T):
    (N, C, M) = sys.stdin.readline().split()
    N = int(N)
    C = int(C)
    M = int(M)
    print(find_total(N,C,M))
