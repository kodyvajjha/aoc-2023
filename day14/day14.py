file_path = './day14/input_test.txt' 
with open(file_path,'r') as file:
  lines = file.read().split("\n")

grids = [str(line).split('\n')[0] for line in lines]

cols = [''.join(map(str,x)) for x in list(zip(*lines))]


def pushrocks_left(lst):
  return '#'.join(map(lambda l: ''.join(sorted(l,reverse=True)),lst.split('#')))

def pushrocks_right(lst):
  return '#'.join(map(lambda l: ''.join(sorted(l)),lst.split('#')))

def push_west(cols):
  return list(map(pushrocks_left,cols))

def push_east(cols):
  return list(map(pushrocks_right,cols))


def part1():
  newcols = list(map(pushrocks_left,cols)) 
  # numos = []
  ans = 0
  for pos in range(0,len(cols)):
    # numos.append((len(cols)-pos,len([col[pos] for col in newcols if col[pos] == 'O'])))
    ans += (len(cols)-pos)*(len([col[pos] for col in newcols if col[pos] == 'O']))
  return ans 

print(part1())
print(grids) # original 
print(cols)
print([''.join(map(str,x)) for x in list(zip(*cols))])
print(list(map(pushrocks_left,grids)))