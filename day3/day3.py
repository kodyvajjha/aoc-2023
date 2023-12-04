import re 

def input():
  file_path = './day3/input.txt' 
  with open(file_path,'r') as file:
    lines = file.readlines()
  lines = [line.strip() for line in lines]
  return lines 

def input_test():
  file_path = './day3/input_test.txt' 
  with open(file_path,'r') as file:
    lines = file.readlines()
  lines = [line.strip() for line in lines]
  return lines 

main_grid = input() 
n = len(main_grid) 
m = len(main_grid[0])

def get_ints(input):
  pos = {}
  for x in range(len(input)):
      for m in re.finditer('\d+',input[x]):
          for y in list(range(*m.span())):
            pos[(x,y)] = m.group()
  return pos
          
def part1(input):
  table = get_ints(input)
  ans = []
  for i in range(0,n):
    for j in range(0,m):
      gridset = set()
      for x in range(-1,2):
        for y in range(-1,2):
          try:
            assert not (input[i][j] == "." or input[i][j].isdigit())
            value = table[(i+x,j+y)] 
            gridset.add(int(value))
          except:
            ans += []
      ans += list(gridset)
  return sum(ans)


# Part 2 

def is_gear(c):
    if c == "*":
        return True 
    else:
        return False 


if __name__ == "__main__":
    # print(list(map(lambda x: is_symbol(x), '...*23....')))
    print(part1(main_grid))