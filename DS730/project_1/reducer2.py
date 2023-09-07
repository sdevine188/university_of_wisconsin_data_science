#!/usr/bin/python3
import sys
import re
import pandas as pd
import numpy as np

# create empty vowel_string_list
vowel_string_list = []

for current_line in sys.stdin:
        
        # read in current_line and split into key and value
        current_line = current_line.strip()
        key = current_line.split(sep = "\t")[0]
        value = current_line.split(sep = "\t")[1]
        
        # add key to vowel_string_list
        vowel_string_list.append(key)
        
# create vowel_strings dataframe
vowel_strings = pd.DataFrame({"string" : vowel_string_list}) 

# convert no_vowels to be blank
vowel_strings.loc[(vowel_strings.string == "no_vowels"), "string"] = ""        

# get count of each string and convert count to string
vowel_strings = vowel_strings.groupby("string", group_keys = False).size().reset_index(name = "string_count")
vowel_strings["string_count_str"] = vowel_strings["string_count"].astype(str)

# get output
vowel_strings["output"] = vowel_strings["string"] + ":" + vowel_strings["string_count_str"]

# print output
output_list = vowel_strings.output.tolist()
for i in list(range(0, len(output_list))):
        print(output_list[i])