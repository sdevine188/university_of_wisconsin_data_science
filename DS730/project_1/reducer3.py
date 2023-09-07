#!/usr/bin/python3
import sys
import re
import pandas as pd
import numpy as np
from collections import Counter


# loop through current_line of key-value pairs
for current_line in sys.stdin:
        
        # read in current_line and split into key and value
        current_line = current_line.strip()
        current_person = current_line.split(sep = "\t")[0]
        current_person_unknown_contacts = current_line.split(sep = "\t")[1]

        # split current_person_unkonwn_contacts concatenation into individual contacts
        current_person_unknown_contacts = current_person_unknown_contacts.split(",")
        
        # count how many times each unknown_contact shows up
        current_person_unique_unknown_contacts = list(Counter(current_person_unknown_contacts))
        current_person_unique_unknown_contacts_count = list(Counter(current_person_unknown_contacts).values())
        
        # get current_person_unique_unknown_contacts_df
        current_person_unique_unknown_contacts_df = pd.DataFrame({"unknown_contacts" : current_person_unique_unknown_contacts, \
                                "unknown_contacts_count" : current_person_unique_unknown_contacts_count})

        # get current_person_maybe_know_string formatted for output
        current_person_maybe_know_list = current_person_unique_unknown_contacts_df. \
                query("unknown_contacts_count in ['2', '3']")["unknown_contacts"].tolist()
        current_person_maybe_know_list.sort()
        current_person_maybe_known_string = ",".join(current_person_maybe_know_list)
        current_person_maybe_known_string = "Might(" + current_person_maybe_known_string + ")"
        
        # get current_person_probably_know_list
        current_person_probably_know_list = current_person_unique_unknown_contacts_df. \
                query("unknown_contacts_count >= 4")["unknown_contacts"].tolist()
        current_person_probably_know_list.sort()
        current_person_probably_known_string = ",".join(current_person_probably_know_list)
        current_person_probably_known_string = "Probably(" + current_person_probably_known_string + ")"
        
        # print output
        print(current_person + ":" + current_person_maybe_known_string + " " + current_person_probably_known_string)