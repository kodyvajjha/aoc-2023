file_path = './day1/input.txt' 
with open(file_path,'r') as file:
  lines = file.readlines()

lines = [line.strip() for line in lines]

# Part 1
def get_num(line):
  nums = []
  for x in line:
    if x.isdigit():
      nums.append(x)
  return int(nums[0])*10 + int(nums[-1])

part1 = sum(list(map(lambda x: get_num(x),lines)))

# Part 2
digits = ['one','two','three','four','five','six','seven','eight','nine']
digitMap = {'one':1,'two':2,'three':3,'four':4,'five':5,'six':6,'seven':7,'eight':8,'nine':9}

def get_num2(line):
  c=0
  nums = []
  while c<len(line):
    x = line[c]
    if x.isdigit():
      nums.append(int(x))
    else:
      for diglen in range(3,6):
        word = line[c:diglen+c]
        if word in digits:
          nums.append(digitMap[word])
    c += 1
  return nums[0]*10 + nums[-1]      

part2 = sum(list(map(lambda x: (get_num2(x)),lines)))

if __name__ == "__main__":
  print(part1)
  print(part2)
    