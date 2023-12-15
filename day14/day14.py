file_path = './day14/input.txt' 
with open(file_path,'r') as file:
  lines = file.read().split("\n")

grids = [str(line).split('\n') for line in lines]
# print(lines)
cols = [''.join(map(str,x)) for x in list(zip(*lines))]
# print(cols)

def pushrocks(lst):
  return '#'.join(map(lambda l: ''.join(sorted(l,reverse=True)),lst.split('#')))


def part1():
  newcols = list(map(pushrocks,cols)) 
  # numos = []
  ans = 0
  for pos in range(0,len(cols)):
    # numos.append((len(cols)-pos,len([col[pos] for col in newcols if col[pos] == 'O'])))
    ans += (len(cols)-pos)*(len([col[pos] for col in newcols if col[pos] == 'O']))
  return ans 

# print(list(map(lambda x: cussort(list(x)), splitstring(cols[6]))))
print(part1())