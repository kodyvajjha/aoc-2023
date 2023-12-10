from functools import reduce 

def input():
  file_path = './day9/input.txt' 
  with open(file_path,'r') as file:
    lines = file.readlines()
  lines = [list(map(lambda x : int(x),line.strip().split())) for line in lines]
  return lines 

def diff(line):
  return [line[i+1]-line[i] for i in range(0,len(line)-1)]

def lasts(line):
  cur = [] 
  while sum(line[-1:]) != 0:
    cur.append(line[-1:][0])
    line = diff(line)
  return sum(cur) 

def firsts(line):
  cur = [] 
  while not all(element == 0 for element in line):
    cur.append(line[0])
    line = diff(line)
  final = list(reversed(cur))
  new = []
  return reduce(lambda x,y: y-x, final)

def part1(lines): 
  return sum([lasts(line) for line in lines])
def part2(lines): 
  return sum([firsts(line) for line in lines])

print(part1(input()))
print(part2(input()))