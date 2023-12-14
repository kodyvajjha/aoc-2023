file_path = './day13/input.txt' 
with open(file_path,'r') as file:
  lines = file.read().split("\n\n")

grids = [str(line).split('\n') for line in lines]

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


# part2, following u/coolioasjulio's solution. (too tired to attempt part 2 - I want to eat ice cream and sleep.)
def num_h_refl_mismatch(pattern: list, row):
    return sum(sum(1 for c1, c2 in zip(r1, r2) if c1 != c2) for r1, r2 in zip(reversed(pattern[:row]), pattern[row:]))

def num_v_refl_mismatch(pattern: list, col):
    return sum(sum(1 for c1, c2 in zip(reversed(row[:col]), row[col:]) if c1 != c2) for row in pattern)

part2 = 0
for pattern in grids:
    score = next((100*r for r in range(1, len(pattern)) if num_h_refl_mismatch(pattern, r) == 1), 0)
    if score == 0:
        score = next((c for c in range(1, len(pattern[0])) if num_v_refl_mismatch(pattern, c) == 1), 0)
    part2 += score
print(part2)