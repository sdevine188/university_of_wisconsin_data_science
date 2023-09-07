#!/usr/bin/python3.6

import time
import os
import sys
import threading

# get start_time
start_time = time.perf_counter()

# create get_word_index()        
def get_word_index(input_folder_path, input_file_name, output_folder_path, page_character_count):
        
        # get current_input_file_name
        current_input_file_name = input_folder_path + "/" + input_file_name

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
            
        # acquire lock and call return_word_page_dict_sorted()
        my_lock.acquire()
        return_word_page_dict_sorted(word_page_dict_sorted)
        my_lock.release()      
          
        
#////////////////////////////////////////////////////////////////////////////////////////////
        
        
# get return_word_page_dict_sorted()
def return_word_page_dict_sorted(word_page_dict_sorted):
        word_index_dictionary_list.append(word_page_dict_sorted)                
        
       
#////////////////////////////////////////////////////////////////////////////////////////////

        
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

# create word_index_dictionary_list
word_index_dictionary_list = []
 
# get threads_list
threads_list = list()  

# loop through input_file_name_list creating threads
for input_file_name in input_list:
        threads_list.append(threading.Thread(target = get_word_index, 
                                             args = (input_folder_path, input_file_name, output_folder_path, page_character_count)))
        
# get my_lock
my_lock = threading.Lock()
        
# loop through threads_list starting threads
for thread_i in list(range(0, len(threads_list))):
#        print("starting thread " + str(thread_i))
        current_thread = threads_list[thread_i]
        current_thread.start()
        
# loop through threads_list joining threads
for current_thread in threads_list:
        current_thread.join()
        
        
#//////////////////////////////////////////////////////////////////////////////////////////


# loop through word_index_dictionary_list pulling all keys 
global_word_index_key_list = []
for dict_i in list(range(0, len(word_index_dictionary_list))):    
        global_word_index_key_list.append(list(word_index_dictionary_list[dict_i].keys()))

# flatten global_word_index_key_list
global_word_index_key_list_flat = []
for sub_list in global_word_index_key_list:
    for text in sub_list:
        global_word_index_key_list_flat.append(text)

# create global_word_index_output_header
global_word_index_output_header = "Word, "
for i in list(range(0, len(word_index_dictionary_list))):
        global_word_index_output_header = global_word_index_output_header + "text_" + str(i) + ", "
global_word_index_output_header = global_word_index_output_header[0:-2]
global_word_index_output_list = [global_word_index_output_header]

# loop through global_word_index_key_list_flat and 
# compile global_word_index_output from each word_index_dictionary
for current_key in global_word_index_key_list_flat:
        current_key_value_output = current_key + " "
        for current_word_index_dictionary in word_index_dictionary_list:
                if(current_key in current_word_index_dictionary):
                        current_values = current_word_index_dictionary[current_key]
                        current_values = ":".join(current_values)
                        current_key_value_output = current_key_value_output + current_values + ", "
                else:
                        current_key_value_output = current_key_value_output + ", "
        current_key_value_output = current_key_value_output[0:-2]
        global_word_index_output_list.append(current_key_value_output)
                
# get output_file_name
output_file_name = output_folder_path + "/" + "output.txt"

# write output to txt file
word_pages_output_file = open(output_file_name, "w")
word_pages_output_file.write("\n".join(global_word_index_output_list))
word_pages_output_file.close()
                
        
#//////////////////////////////////////////////////////////////////////////////////////////


# get end_time
end_time = time.perf_counter()

# print total_time
# should print about 1 second
total_time = end_time - start_time
print(total_time)