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