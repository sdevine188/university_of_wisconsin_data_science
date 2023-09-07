# problem 1 
# Who was the heaviest player (weight) to hit more than 5 triples (3B) in 2005?


# load batters
batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');

# inspect 
batters_head = LIMIT batters_head 10;
DUMP batters_head;

# get nrow (99847)
batters_count = GROUP batters ALL;
batters_count = FOREACH batters_count GENERATE COUNT(batters);
DUMP batters_count; 

# filter down to batters with year > 0
batters = FILTER batters BY $1>0;

# get nrow (99846)
batters_count = GROUP batters ALL;
batters_count = FOREACH batters_count GENERATE COUNT(batters);
DUMP batters_count; 

# get batters_triples_2005
batters_triples_2005 = FOREACH batters GENERATE $0 AS player_id, $1 AS year, $9 AS triples;

# inspect 
batters_triples_2005_head = LIMIT batters_triples_2005 10;
DUMP batters_triples_2005_head;

# filter down to 2005
batters_triples_2005 = FILTER batters_triples_2005 BY year == 2005;

# filter down to greater than 5 triples
batters_triples_2005 = FILTER batters_triples_2005 BY triples > 5;

# inspect 
batters_triples_2005_head = LIMIT batters_triples_2005 10;
DUMP batters_triples_2005_head;


#################################################################################################


# get batters_weight

# load master
master = LOAD 'hdfs:/user/maria_dev/pigtest/Master.csv' using PigStorage(',');

# inspect 
master_head = LIMIT master 10;
DUMP master_head; 

# get batters_weight
batters_weight = FOREACH master GENERATE $0 AS player_id, $16 AS weight;

# inspect 
batters_weight_head = LIMIT batters_weight 10;
DUMP batters_weight_head; 


#################################################################################################


# join batters_triples_2005 with batters_weight to get batters_triples_2005_w_weight
batters_triples_2005_w_weight = JOIN batters_triples_2005 BY (player_id) LEFT, batters_weight BY (player_id);
batters_triples_2005_w_weight = FOREACH batters_triples_2005_w_weight GENERATE $0 AS player_id, year, triples, weight;

# inspect 
batters_triples_2005_w_weight_head = LIMIT batters_triples_2005_w_weight 10;
DUMP batters_triples_2005_w_weight_head; 

# sort batters_triples_2005_w_weight by descending weight
batters_triples_2005_w_weight_ordered = ORDER batters_triples_2005_w_weight BY weight DESC;

# inspect 
batters_triples_2005_w_weight_ordered_head = LIMIT batters_triples_2005_w_weight_ordered 10;
DUMP batters_triples_2005_w_weight_ordered_head; 

# get heaviest_batter_in_2005_to_hit_over_5_triples
heaviest_batter_in_2005_to_hit_over_5_triples = LIMIT batters_triples_2005_w_weight_ordered 1;
heaviest_batter_in_2005_to_hit_over_5_triples = FOREACH heaviest_batter_in_2005_to_hit_over_5_triples GENERATE player_id;

# inspect 
heaviest_batter_in_2005_to_hit_over_5_triples_head = LIMIT heaviest_batter_in_2005_to_hit_over_5_triples 10;
DUMP heaviest_batter_in_2005_to_hit_over_5_triples_head; 

# print heaviest_batter_in_2005_to_hit_over_5_triples
# problem 1 answer: (hollima01) 
DUMP heaviest_batter_in_2005_to_hit_over_5_triples;


#################################################################################################
#################################################################################################
#################################################################################################

