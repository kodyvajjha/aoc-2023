file_path = './day13/input.txt' 
with open(file_path,'r') as file:
  lines = file.read().split("\n\n")

grids = [str(line).split('\n') for line in lines]

# print(grids[0])


def rowcheck(pattern):
    return [row for row in range(1,len(pattern)) if all([r1 == r2 for r1,r2 in zip(reversed(pattern[:row]),pattern[row:])])]


def colcheck(pattern):
  pattern = list(zip(*pattern))
  return [col for col in range(1,len(pattern)) if all([c1 == c2 for c1,c2 in zip(reversed(pattern[:col]),pattern[col:])])]

def part1():
  colnums = sum(list(map(lambda x: colcheck(x),grids)),[])
  rownums = sum(list(map(lambda x: rowcheck(x),grids)),[])
  return (sum(colnums)+ 100 * sum(rownums))

print(part1())