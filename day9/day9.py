import re 
import math
import numpy as np 

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

def part1(lines): 
  return sum([lasts(line) for line in lines])


# print(sum(lasts([1,3,   6,  10,  15,  21])))
# print(sum(lasts([10,13,16,21,30,45])))
print(part1(input()))