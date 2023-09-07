#!/usr/bin/python3
import sys
import re
import pandas as pd
import numpy as np
from itertools import repeat

# create empty key_list
key_list = []

# read in initial current_line
current_line = sys.stdin.readline()

# loop through current_lines extracting each word's vowels and adding sorted vowels to key_list
while current_line:      

        #       print(current_line)
        
        # strip whitespace from current_line
        current_line = current_line.strip()
        
        # split current_line based on tab
        current_line_list = current_line.split("\t") 
        
        # split current_line_list based on newline, and flatten resulting list of lists
        current_line_list = [i.split("\n") for i in current_line_list]
        current_line_list_flat = []
        for current_list in current_line_list:
                for current_string in current_list:
                        current_line_list_flat.append(current_string)
        current_line_list = current_line_list_flat
        
        # split current_line_list based on space, and flatten resulting list of lists
        current_line_list = [i.split(" ") for i in current_line_list]
        current_line_list_flat = []
        for current_list in current_line_list:
                for current_string in current_list:
                        if(current_string != ""):
                                current_line_list_flat.append(current_string)
        current_line_list = current_line_list_flat
        
        # remove all non-vowels from current_line_list to get current_vowel_list
        current_vowel_list = [re.sub(pattern = r'[^aeiouy]', repl = "", string = i, flags = re.IGNORECASE) for i in current_line_list]        
        
        # convert current_vowel_list to lowercase
        current_vowel_list = [i.lower() for i in current_vowel_list]
        
        # sort vowel strings
        for i in list(range(0, len(current_vowel_list))):
                current_vowel_string = [letter for letter in current_vowel_list[i]]
                current_vowel_string.sort()
                current_vowel_string = "".join(current_vowel_string)
                current_vowel_list[i] = current_vowel_string
                
        # add current_vowel_list to key_list, and flatten key_list
        key_list.append(current_vowel_list)
        key_list_flat = []
        for current_list in key_list:
                for current_string in current_list:
                        key_list_flat.append(current_string)
        key_list = key_list_flat
        
        # replace any blanks with explicit "no_vowels" text
        key_list = [re.sub(pattern = r'^$', repl = "no_vowels", string = x) for x in key_list]
                
        # read in next line
        current_line = sys.stdin.readline()

# get value_list to output in key-value pair
# note that values will just be 1 as a placeholder, since the key is really the information the reducer needs
value_list = list(repeat("1", len(key_list)))

# loop through key_list and value_list, printing key-value pairs with tab seperator
for i in list(range(0, len(key_list))):
        print(key_list[i] + "\t" + value_list[i])