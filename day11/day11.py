from itertools import combinations 

file_path = './day11/input.txt' 
with open(file_path,'r') as file:
  lines = file.readlines()

rows = [line.strip() for line in lines]
cols = list(zip(*rows))

empty_rows = [nrow for nrow,row in enumerate(rows) if '#' not in row]
empty_cols = [ncol for ncol,col in enumerate(cols) if '#' not in col]
galaxy_pos = [(i, j) for i, x in enumerate(rows) for j, y in enumerate(x) if y == "#"]

all_pairs = list(combinations(galaxy_pos,2))

def manhattan(g1,g2):
  return abs (g1[0] - g2[0]) + abs(g1[1] - g2[1])

def empty_rows_between(g1,g2):
  return [r for r in empty_rows if min(g1[0],g2[0]) < r and r < max(g1[0],g2[0])]

def empty_cols_between(g1,g2):
  return [c for c in empty_cols if min(g1[1],g2[1]) < c and c < max(g1[1],g2[1])]
  

# def expansionEffectFrom(start, end, empties):
#     out = 0
#     for place in range(min(start, end), max(start, end)+1):
#         if place in empties:
#             out += 2

#     return out

def expanded_distance(scale,g1,g2):
  nr = len(empty_rows_between(g1,g2)) 
  nc = len(empty_cols_between(g1,g2)) 
  expansion = (nr+nc)*(scale-1)
  return abs (g1[0] - g2[0]) + abs(g1[1] - g2[1]) + expansion

def part1():
  ans = 0
  for g1,g2 in all_pairs:
    ans += expanded_distance(2,g1,g2)
  return ans 

def part2():
  ans = 0
  for g1,g2 in all_pairs:
    ans += expanded_distance(1000000,g1,g2)
  return ans 
  
print(part1())
print(part2())