#!/usr/bin/python3
import sys
import re
import pandas as pd
import numpy as np

# create friends_dict
friends_dict = {}

# read in initial current_line
current_line = sys.stdin.readline()

# loop through current_lines extracting each word's vowels and adding sorted vowels to key_list
while current_line:    
        
        # get current_person
        current_person = current_line.split(":")[0].strip()
        
        # get current_person_friends
        current_person_friends = current_line.split(":")[1].strip()

        # add current_person and current_person_friends to friends_dict
        friends_dict[current_person] = current_person_friends  
        
        # read in next line
        current_line = sys.stdin.readline()
        
# loop through friends_dict, and getting the unknown contacts of friends for each current_person
# store unknown contacts of friends for each current_person in unknown_contacts_dict
unknown_contacts_dict = {}

for i in list(range(0, len(friends_dict))):
        
        # get current_person
        current_person = list(friends_dict)[i]
        
        # get current_person_friends
        current_person_friends = list(friends_dict.values())[i]
        current_person_friends = current_person_friends.split(" ")
        
        # get current_person_unknown_friend_contacts_list 
        current_person_unknown_friend_contacts_list = []
        
        # loop through current_person's friends getting unknown contacts
        for f in list(range(0, len(current_person_friends))):
                
                # get current_friend
                current_friend = current_person_friends[f]
                
                # get current_friend_contacts 
                current_friend_contacts = friends_dict[current_friend]
                current_friend_contacts = current_friend_contacts.split(" ")

                # get unknown_current_friend_contacts
                unknown_current_friend_contacts = list(set(current_friend_contacts).difference(set(current_person_friends)))
                
                # append to unknown_current_friend_contacts to current_person_unknown_friend_contacts_list
                # then flatten current_person_unknown_friend_contacts_list
                current_person_unknown_friend_contacts_list.append(unknown_current_friend_contacts)
                current_person_unknown_friend_contacts_list_flat = []
                for current_list in current_person_unknown_friend_contacts_list:
                        for current_string in current_list:
                                current_person_unknown_friend_contacts_list_flat.append(current_string)
                current_person_unknown_friend_contacts_list = current_person_unknown_friend_contacts_list_flat
        
        
        # add current_person and current_person_unknown_friend_contacts_list to unknown_contacts_dict
        unknown_contacts_dict[current_person] = current_person_unknown_friend_contacts_list
        


# create empty key_list
key_list = []

# create empty value_list
value_list = []

# loop through unknown_contacts_dict adding current_person to key_list
# and adding current_person's unknown contacts to value_list     
for i in list(range(0, len(unknown_contacts_dict))) :
        
        # get current_person and append to key_list
        current_person = list(unknown_contacts_dict)[i]
        key_list.append(current_person)
        
        # get current_person_unknown_contacts, concatenate it, and append to value_list
        current_person_unkonwn_contacts = list(unknown_contacts_dict.values())[i]
        current_person_unkonwn_contacts = ",".join(current_person_unkonwn_contacts)
        value_list.append(current_person_unkonwn_contacts)


# print key and value output seperated by tab
for i in list(range(0, len(key_list))) :
        print(key_list[i] + "\t" + value_list[i])