#!/usr/bin/python3.6

import time
import os
import sys

# get start_time
start_time = time.perf_counter()

# handle arguments
input_folder = sys.argv[1]
output_folder = sys.argv[2]
page_character_count = int(sys.argv[3])

time.sleep(5)

# get end_time
end_time = time.perf_counter()

# print total_time
# should print about 1 second
total_time = end_time - start_time
print("Took " + str(total_time) + " seconds to finish.")