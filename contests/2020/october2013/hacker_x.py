import sys

class Missile:
    def __init__(self, t, f):
        self.t = t
        self.f = f

#Gets missiles from stdin
def get_missiles():
    N = int(sys.stdin.readline())
    missiles = [0] * N
    for n in range(0, N):
        (t, f) = sys.stdin.readline().split()
        t = int(t)
        f = int(f)
        missiles[n] = Missile(t, f)

    missiles.sort(key = lambda Missile: Missile.t)
    return missiles

#uses branch and bound to find minimum number of HackerX's needed
def find_min(missiles):
    pass

missiles = get_missiles()
print(missiles)
