import math 
T = [46,85,75,82]
D = [208,1412,1257,1410]

TDs = list(zip(T,D))

def root1(T,D):
  return (T - math.sqrt(T*T - 4*D))/2 

def root2(T,D):
  return (T + math.sqrt(T*T - 4*D))/2 

def ways(T,D):
  a = 0
  r1 = root1(T,D)
  r2 = root2(T,D)
  for t in range(T):
    if r1 < t and t<r2:
      a += 1
  return a

def part1():
  ans = 1 
  for T,D in TDs:
    ans *= ways(T,D)
  return ans

print(part1())