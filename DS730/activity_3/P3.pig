#################################################################################################
#################################################################################################
#################################################################################################


# problem 3
# What player had the most extra base hits during the entire 1980’s (1980 to 1989)? Note that this question is not asking about any 
# 1 specific year. It is asking about the entire 10 year span in the 80’s. An extra base hit is a double, triple or home run (columns 2B, 3B, HR).


# load batters
batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');
batters = FOREACH batters GENERATE $0 AS player_id, $1 AS year, $8 AS doubles, $9 AS triples, $10 AS home_runs;

# inspect 
batters_head = LIMIT batters 10;
DUMP batters_head;

# filter down to 1980-1989 
batters_80s = FILTER batters BY (year >= 1980) AND (year <= 1989);

# inspect 
batters_80s_head = LIMIT batters_80s 10;
DUMP batters_80s_head;

# sum doubles, triples, and home_runs for each player_id/year record to get extra_base_hits
batters_80s_extra_base_hits = FOREACH batters_80s GENERATE player_id, year, (doubles + triples + home_runs) AS extra_base_hits;
 
# inspect 
batters_80s_extra_base_hits_head = LIMIT batters_80s_extra_base_hits 10;
DUMP batters_80s_extra_base_hits_head;

# group by player_id
batters_80s_extra_base_hits_grouped = GROUP batters_80s_extra_base_hits BY player_id;

# inspect 
batters_80s_extra_base_hits_grouped_head = LIMIT batters_80s_extra_base_hits_grouped 10;
DUMP batters_80s_extra_base_hits_grouped_head;

# sum extra_base_hits by player_id
batters_80s_sum_extra_base_hits = FOREACH batters_80s_extra_base_hits_grouped GENERATE group, SUM(batters_80s_extra_base_hits.extra_base_hits) AS sum_extra_base_hits;
batters_80s_sum_extra_base_hits = FOREACH batters_80s_sum_extra_base_hits GENERATE FLATTEN(group) AS player_id, sum_extra_base_hits;

# inspect 
batters_80s_sum_extra_base_hits_head = LIMIT batters_80s_sum_extra_base_hits 10;
DUMP batters_80s_sum_extra_base_hits_head;

# order by desc sum_total_extra_base_hits
batters_80s_sum_extra_base_hits_ordered = ORDER batters_80s_sum_extra_base_hits BY sum_extra_base_hits DESC;

# inspect 
batters_80s_sum_extra_base_hits_ordered_head = LIMIT batters_80s_sum_extra_base_hits_ordered 10;
DUMP batters_80s_sum_extra_base_hits_ordered_head;

# get player_id with most sum_extra_base_hits and output player_id
batters_80s_sum_extra_base_hits_ordered_max = LIMIT batters_80s_sum_extra_base_hits_ordered 1;
batters_80s_sum_extra_base_hits_ordered_max = FOREACH batters_80s_sum_extra_base_hits_ordered_max GENERATE player_id;
DUMP batters_80s_sum_extra_base_hits_ordered_max;