#!/usr/bin/python3
#Be sure the indentation is identical and also be sure the line above this is on #the first line 
import sys
import re
 
line = sys.stdin.readline()
pattern = re.compile("[a-zA-Z0-9]+")
while line:
  for word in pattern.findall(line):
    print(word+"\t"+"1")
  line = sys.stdin.readline()




