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