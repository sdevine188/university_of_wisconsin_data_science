#///////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////


line = "hello this is a test this is"

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
  
  
#///////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////


# create print_input
  
# note that you need to save csv in notepad++ as a tab delimited text file (just copy/paste from excel into notepad)
# because if you upload as csv, then readline will go on reading in blank rows as " , , , , , "
# but with the text file it knows when to stop trying to read in new lines

  
#!/usr/bin/python3
import sys
import re

# read input
line = sys.stdin.readline()

while line:      
  print(line)
  line = sys.stdin.readline()

#for i in list(range(0, 5)):
#  print(line)
#  line = sys.stdin.readline()


#///////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////


# problem 1
# mapper
  

#!/usr/bin/python3
import sys
import re
import pandas as pd
import numpy as np

# create empty lists for all variables
InvoiceNo_list = []
StockCode_list = []
Description_list = []
Quantity_list = []
InvoiceDate_list = []
UnitPrice_list = []
CustomerID_list = []
Country_list = []


#///////////////////////////////////////////////////////////////////////////////////


# loop through input lines adding them to a pandas dataframe
current_line = sys.stdin.readline()
while current_line:      

        #       print(current_line)
        
        # split current_line based on tab delimiter
        current_line_split = current_line.split("\t")        
        
        # add values from current_line_split to respective variable list
        InvoiceNo_list.append(current_line_split[0])
        StockCode_list.append(current_line_split[1])
        Description_list.append(current_line_split[2])
        Quantity_list.append(current_line_split[3])
        InvoiceDate_list.append(current_line_split[4])
        UnitPrice_list.append(current_line_split[5])
        CustomerID_list.append(current_line_split[6])
        Country_list.append(current_line_split[7])
        
        # read in next line
        current_line = sys.stdin.readline()


#///////////////////////////////////////////////////////////////////////////////////


# create dataframe for current_line
orders_df = pd.DataFrame({"InvoiceNo" : InvoiceNo_list,
                          "StockCode" : StockCode_list,
                          "Description" : Description_list,
                          "Quantity" : Quantity_list,
                          "InvoiceDate" : InvoiceDate_list,
                          "UnitPrice" : UnitPrice_list,
                          "CustomerID" : CustomerID_list,
                          "Country" : Country_list})

# rearrange variable name order, replace blank values with NaN, and drop any row matching the column names, 
# for instance, where mapReduce job reads in headers as first line of tab-delimited data.txt
orders_df = orders_df.query("InvoiceNo != 'InvoiceNo'"). \
        filter(["InvoiceNo", "StockCode", "Description", "Quantity", \
                 "InvoiceDate", "UnitPrice", "CustomerID", "Country"]). \
        replace(r'^\s*$', np.nan, regex = True)

# drop records where invoiceNo begins with a "C" indicating a refund        
orders_df = orders_df[~orders_df["InvoiceNo"].str.contains("^C")]

# drop records where CustomerID is missing
orders_df = orders_df.query("CustomerID == CustomerID")
        
# convert dtypes
orders_df["Quantity"] = pd.to_numeric(orders_df["Quantity"])
orders_df["UnitPrice"] = pd.to_numeric(orders_df["UnitPrice"])
orders_df["InvoiceDate"] = pd.to_datetime(orders_df["InvoiceDate"])
orders_df["Quantity"] = pd.to_numeric(orders_df["Quantity"])
orders_df["UnitPrice"] = pd.to_numeric(orders_df["UnitPrice"])
orders_df["InvoiceDate"] = pd.to_datetime(orders_df["InvoiceDate"])

# get variables for month, year, and day, and convert to string (two digits for month and day)
orders_df["month"] = pd.DatetimeIndex(orders_df["InvoiceDate"]).month
orders_df["month"] = orders_df["month"].astype(str).str.pad(2, side = "left", fillchar = "0")

orders_df["day"] = pd.DatetimeIndex(orders_df["InvoiceDate"]).day
orders_df["day"] = orders_df["day"].astype(str).str.pad(2, side = "left", fillchar = "0")

orders_df["year"] = pd.DatetimeIndex(orders_df["InvoiceDate"]).year
orders_df["year"] = orders_df["year"].astype(str)

# get cost variable, and convert to string
orders_df["cost"] = orders_df["UnitPrice"] * orders_df["Quantity"]
orders_df["cost"] = orders_df["cost"].astype(str)

# combine month and country to get key value
orders_df["key"] = orders_df["month"] + "__" + orders_df["Country"]

# combine CustomerID and cost to get value
orders_df["value"] = orders_df["CustomerID"] + "__" + orders_df["cost"]

# get key_list and value_list
key_list = orders_df["key"].tolist() 
value_list = orders_df["value"].tolist() 

# loop through key_list and value_list, printing key-value pairs with tab seperator
for i in list(range(0, len(key_list))):
        print(key_list[i] + "\t" + value_list[i])
        


#///////////////////


# test
orders_df
orders_df.dtypes


header_line = "InvoiceNo	StockCode	Description	Quantity	InvoiceDate	UnitPrice	CustomerID	Country"
header_line
header_line_split = header_line.split("\t")
header_line_split
header_line_split[1]

line_1 = "536365	71053	WHITE METAL LANTERN	6	12/1/2010 8:26	3.39	17850	United Kingdom"
line
line_1_split = line_1.split("\t")
line_1_split

line_2 = "C536365	85123A	WHITE HANGING HEART T-LIGHT HOLDER	6	1/3/2010 8:26	2.55	17850	United Kingdom"
line_2
line_2_split = line_2.split("\t")
line_2_split
line_2_split

line_3 = "536414	22139		56	4/5/2010 11:52	0		United Kingdom"
line_3
line_3_split = line_3.split("\t")
line_3_split

current_line = line_1
current_line = line_2
current_line = line_3
current_line = header_line


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# problem 1
# reducer


#!/usr/bin/python3
import sys
import re
import pandas as pd
import numpy as np


# create empty lists for month, country, customerID, and cost
month_list = []
country_list = []
customer_id_list = []
cost_list = []

for current_line in sys.stdin:
        
        # read in current_line and split into key and value
        current_line = current_line.strip()
        key = current_line.split(sep = "\t")[0]
        value = current_line.split(sep = "\t")[1]
        
        # split key into month and country
        current_month = key.split(sep = "__")[0]
        current_country = key.split(sep = "__")[1]
        
        # split value into CustomerID and country
        current_customer_id = value.split(sep = "__")[0]
        current_cost = value.split(sep = "__")[1]

        # add current month, country, customerID, and cost to respective lists
        month_list.append(current_month)
        country_list.append(current_country)
        customer_id_list.append(current_customer_id)
        cost_list.append(current_cost)
        
        
#/////////////////////////
        
        
# combine month, country, customerID, and cost into dataframe
reduced_orders = pd.DataFrame({"month" : month_list,
                               "country" : country_list,
                               "customer_id" : customer_id_list,
                               "cost" : cost_list})   
        
# convert cost to numeric
reduced_orders["cost"] = pd.to_numeric(reduced_orders["cost"])

# get the cost_sum per customer for each month/country
reduced_orders_summary = reduced_orders.groupby(["month", "country", "customer_id"], as_index = False). \
        agg({"cost" : ["sum"]})
reduced_orders_summary.columns = list(map("_".join, reduced_orders_summary.columns.values))
reduced_orders_summary.columns = ["month", "country", "customer_id", "cost_sum"]

# filter down to customer_id record with maximum cost for each month/country
reduced_orders_summary.groupby(["month", "country"])
reduced_orders_summary["cost_sum_max"] = reduced_orders_summary.groupby(["month", "country"])["cost_sum"].transform(max)
reduced_orders_summary = reduced_orders_summary.query("cost_sum == cost_sum_max")

# ouptput month,country:customer_id
reduced_orders_summary["output"] = reduced_orders_summary["month"] + "," + \
                                        reduced_orders_summary["country"] + ":" + \
                                        reduced_orders_summary["customer_id"]
output_list = reduced_orders_summary.output.tolist()
for i in list(range(0, len(output_list))):
        print(output_list[i])


#/////////////////////////////
        
        
# test
current_line = "12__United Kingdom\t17850__20.34"
current_line = "12__United Kingdom\t17850__55"
current_line = "01__United Kingdom\t17850__15.299999999999999"
current_line = "01__United Kingdom\t17850__60"
current_line = "12__United Kingdom\t10000__30"
current_line = "12__United Kingdom\t10000__10"
current_line = "01__United Kingdom\t10000__45"
current_line = "01__United Kingdom\t10000__15"


reduced_orders
reduced_orders.dtypes
reduced_orders_v2 = reduced_orders.copy()


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# problem 2
# mapper


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


#///////////////////////////
        
        
# test
current_line = "hEllo    moose pOle\tcccttt.ggg\n abcdef  cat’s.and:d0gs!  "
current_line


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# problem 2
# reducer


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

#////////////////////////////
        
        
current_line = "eo\t1"
current_line = "eoo\t1"
current_line = "eoa\t1"
current_line = "eoo\t1"
current_line = "eoa\t1"
current_line = "no_vowels\t1"
current_line = "no_vowels\t1"


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# problem 3
# mapper


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


#///////////////////////////////


current_line = "6 : 2 9 8 10"
current_line = "1 : 3 5 8 9 10 12"
current_line = "4 : 2 5 7 8 9"
current_line = "2 : 3 4 7 6 13"
current_line = "12 : 1 7 5 9"
current_line = "3 : 9 11 10 1 2 13"
current_line = "10 : 1 3 6 11"
current_line = "5 : 4 1 7 11 12"
current_line = "13 : 2 3"
current_line = "8 : 1 6 4 11"
current_line = "7 : 5 2 4 9 12"
current_line = "11 : 3 5 10 8"
current_line = "9 : 12 1 3 6 4 7"

friends_dict_backup = friends_dict

list(friends_dict)[0]
list(friends_dict.values())[0]


#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////


# problem 3
# reducer


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


#///////////////////////////////
        
        
# test
current_line = '6\t1,3,3,6,7,4,1,3,6,7,1,2,4,1,4,6,1,1,3,1,6,11'
