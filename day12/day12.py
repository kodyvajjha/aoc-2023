from functools import lru_cache

file_path = './day12/input.txt' 
with open(file_path,'r') as file:
  lines = file.readlines()

rows = [line.strip().split() for line in lines]


def record(drawing):
  return [len(x) for x in drawing.split('.') if x]


def possibilities(drawing):
    def aux(s, index):
        if index == len(drawing):
            result.append(s)
            return

        if drawing[index] == '?':
            aux(s + '.', index + 1)
            aux(s + '#', index + 1)
        else:
            aux(s + drawing[index], index + 1)

    result = []
    aux('', 0)
    return result

# Brute forcing 
def part1():
  ans = 0
  for x,rec in rows:
    rec = [int(x) for x in rec.split(',')]
    filtered = list(filter(lambda x: x == rec, map(lambda y: record(y), possibilities(x))))
    ans += len(filtered) 
  return ans 

print(part1())