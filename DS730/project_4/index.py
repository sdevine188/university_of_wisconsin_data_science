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

# get input_folder_path
input_folder_path = "./" + input_folder

# get output_folder_path 
output_folder_path = "./" + output_folder

# get input_list
input_list = os.listdir(input_folder_path)

# loop through input_list creating index
for input_i in list(range(0, len(input_list))):
        
        # get current_input_file_name
        current_input_file_name = input_list[input_i]
        current_input_file_name = input_folder_path + "/" + current_input_file_name
        
        # read current_input_file
        current_input_file_connection = open(current_input_file_name, "r")
        current_input_file = current_input_file_connection.read()
        current_input_file_connection.close()
        
        # split current_input_file by space
        current_input_file_split_by_space = current_input_file.split(" ")
        
        # create a loop to further split by newline 
        current_input_file_split_by_space_newline = []
        for text_i in list(range(0, len(current_input_file_split_by_space))):
                current_text = current_input_file_split_by_space[text_i].split("\n")
                current_input_file_split_by_space_newline.append(current_text)
        
        # flatten current_input_file_split_by_space_newline_flat
        current_input_file_split_by_space_newline_flat = []
        for sub_list in current_input_file_split_by_space_newline:
            for text in sub_list:
                current_input_file_split_by_space_newline_flat.append(text)
        
        # create a loop to further split by tab
        current_input_file_split_by_space_newline_tab = []
        for text_i in list(range(0, len(current_input_file_split_by_space_newline_flat))):
                current_text = current_input_file_split_by_space_newline_flat[text_i].split("\t")
                current_input_file_split_by_space_newline_tab.append(current_text)
        
        # flatten current_input_file_split_by_space_newline_flat
        current_input_file_split_by_space_newline_tab_flat = []
        for sub_list in current_input_file_split_by_space_newline_tab:
            for text in sub_list:
                current_input_file_split_by_space_newline_tab_flat.append(text)
                
        # get flattened text with all lowercase characters, dropping empty strings
        text = []
        for text_i in list(range(0, len(current_input_file_split_by_space_newline_tab_flat))):
                current_text = current_input_file_split_by_space_newline_tab_flat[text_i]
                if(current_text != ""):
                        text.append(current_text.lower())
        
        # get text_length_list
        text_length_list = []
        for word in text:
                current_word_length = len(word)
                text_length_list.append(current_word_length)
                
        # get text_page_list
        text_page_list = []
        text_cum_length = 0
        current_page = 1
        next_page_threshold = page_character_count
        for current_word_length in text_length_list:
                text_cum_length = text_cum_length + current_word_length
                if(text_cum_length <= next_page_threshold):
                        current_page_str = str(current_page)
                        text_page_list.append(current_page_str)
                if(text_cum_length > next_page_threshold):
                        current_page = current_page + 1
                        current_page_str = str(current_page)
                        text_page_list.append(current_page_str)
                        next_page_threshold = next_page_threshold + page_character_count

        # get word_page_dict combining pages for each word
        word_page_dict = {}
        for word_i in list(range(0, len(text))):
                current_word = text[word_i]
                current_page = text_page_list[word_i]
                if((current_word in word_page_dict) and (current_page not in word_page_dict[current_word])):
                        word_page_dict[current_word].append(current_page)
                elif(current_word not in word_page_dict):
                        word_page_dict[current_word] = [current_page]
                        
        # get word_page_dict_sorted
        word_page_dict_keys = list(word_page_dict.keys())
        word_page_dict_keys_sorted = sorted(word_page_dict_keys)
        word_page_dict_sorted = {}
        for key in word_page_dict_keys_sorted:
                word_page_dict_sorted[key] = word_page_dict[key]
        
        # word_page_output_list preparing output
        word_pages_output_list = []
        for current_word in word_page_dict_sorted:
                current_word_pages = word_page_dict_sorted[current_word]
                current_word_pages_output = current_word + " " + ", ".join(current_word_pages)
                word_pages_output_list.append(current_word_pages_output)
        
        # get current_output_file_name
        current_output_file_name = output_folder_path + "/" + input_list[input_i]
        current_output_file_name = current_output_file_name[0:-4] + "_output.txt"

        # write output to txt file
        word_pages_output_file = open(current_output_file_name, "w")
        word_pages_output_file.write("\n".join(word_pages_output_list))
        word_pages_output_file.close()


# get end_time
end_time = time.perf_counter()

# print total_time
# should print about 1 second
total_time = end_time - start_time
print(total_time)
