file_path = './day14/input_test.txt' 
with open(file_path,'r') as file:
  lines = file.read().split("\n")

# grids = [str(line).split('\n') for line in lines]
# print(lines)
cols = [''.join(map(str,x)) for x in list(zip(*lines))]
print(cols)


def cussort(lst):
  lst.sort(reverse=True)
  return lst 

print(cussort(list('OO.O.O..')))