# problem 4
# Of the right-handed batters (column bats, denoted with an R) who were born in October and died in 2011, 
# which one had the most hits in his career? The column with the heading of H is the hits column. 
# Do not consider switch hitters to be right-handed batters.


# filter masters to right_handed batters born in Oct and died in 2011 to get batters_right_born_oct_died_2011
# group batters by player_id and select group and sum(H) to get batters_career_hits
# ungroup batters_career_hits by flatten(group)
# left_join batters_right_born_oct_died_2011 with batters_career_hits to get batters_right_born_oct_died_2011_career_hits
# order career_hits DESC to get batters_right_born_oct_died_2011_career_hits_ordered
# LIMIT 1
# output player_id


# load master and limit variables to player_id, birth_month, death_year, and batting_hand
master = LOAD 'hdfs:/user/maria_dev/pigtest/Master.csv' using PigStorage(',');
master = FOREACH master GENERATE $0 AS player_id, $2 AS birth_month, $7 AS death_year, $18 AS batting_hand;

# inspect 
master_head = LIMIT master 10;
DUMP master_head; 

# filter masters to right_handed batters born in Oct and died in 2011 
batters_right_born_oct_died_2011 = FILTER master BY (batting_hand == 'R') AND (birth_month == 10) AND (death_year == 2011);

# inspect 
batters_right_born_oct_died_2011_head = LIMIT batters_right_born_oct_died_2011 10;
DUMP batters_right_born_oct_died_2011_head; 

test2 = FOREACH batters_right_born_oct_died_2011_head GENERATE player_id;
DUMP test2;

#////////////////////////////////////////////////////////////////////////////////////////////////////////


# load batters
batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');
batters = FOREACH batters GENERATE $0 AS player_id, $7 AS hits;

# inspect 
batters_head = LIMIT batters 10;
DUMP batters_head;

# group batters by player_id 
batters_grouped = GROUP batters BY player_id;

# inspect 
batters_grouped_head = LIMIT batters_grouped 10;
DUMP batters_grouped_head;

# select group and sum(H) to get batters_career_hits
# note: no need to flatten group since it's only one element long, and so it's not placed inside array like multiple-element groups
batters_career_hits = FOREACH batters_grouped GENERATE group, SUM(batters.hits) AS career_hits;
#batters_career_hits = FOREACH batters_career_hits GENERATE FLATTEN(group) AS player_id, career_hits;

# inspect 
batters_career_hits_head = LIMIT batters_career_hits 10;
DUMP batters_career_hits_head;

test = FOREACH batters_career_hits_head GENERATE player_id;
DUMP test;


#///////////////////////////////////////////////////////////////////////////////////


# left_join batters_right_born_oct_died_2011 with batters_career_hits
# note: the join code would run, but could not DUMP the output for some reason; so switched to filter instead of join
batters_right_born_oct_died_2011_career_hits = JOIN batters_right_born_oct_died_2011 BY (player_id) LEFT, batters_career_hits BY (player_id);
# batters_right_born_oct_died_2011_career_hits = JOIN batters_right_born_oct_died_2011 BY (batters_right_born_oct_died_2011.player_id) LEFT, batters_career_hits BY (batters_career_hits.player_id);
batters_right_born_oct_died_2011_career_hits = FOREACH batters_right_born_oct_died_2011_career_hits GENERATE batters_right_born_oct_died_2011.player_id, career_hits;
# batters_right_born_oct_died_2011_career_hits = JOIN batters_right_born_oct_died_2011 BY ($1) LEFT, batters_career_hits BY ($1);

# inspect 
# batters_right_born_oct_died_2011_career_hits_head = LIMIT batters_right_born_oct_died_2011_career_hits 10;
# DUMP batters_right_born_oct_died_2011_career_hits_head;


#///////////////////////////////////////////////////////////////////////////////////

# filter batters_career_hits down to those player_id in batters_right_born_oct_died_2011
batters_career_hits_filtered = FILTER batters_career_hits BY player_id IN (batters_right_born_oct_died_2011.player_id);

# inspect 
batters_career_hits_filtered_head = LIMIT batters_career_hits_filtered 10;
DUMP batters_career_hits_filtered_head;


#///////////////////////////////////////////////////////////////////////////////////


# order batters_right_born_oct_died_2011_career_hits by career_hits DESC
batters_right_born_oct_died_2011_career_hits_ordered = ORDER batters_right_born_oct_died_2011_career_hits BY career_hits DESC;

# inspect 
batters_right_born_oct_died_2011_career_hits_ordered_head = LIMIT batters_right_born_oct_died_2011_career_hits_ordered 10;
DUMP batters_right_born_oct_died_2011_career_hits_ordered_head;

# get batters_right_born_oct_died_2011_career_hits_max, get player_id, and output player_id
batters_right_born_oct_died_2011_career_hits_max = batters_right_born_oct_died_2011_career_hits_ordered LIMIT 1;
batters_right_born_oct_died_2011_career_hits_max = FOREACH batters_right_born_oct_died_2011_career_hits_max GENERATE player_id;
DUMP batters_right_born_oct_died_2011_career_hits_max;


#//////////////////////////////////////////////////////////////////////////////////////


# output file
# note that output folder specified in command should not yet exist in Ambari HDFS structure
STORE batters_right_born_oct_died_2011_career_hits_ordered INTO 'hdfs:/user/maria_dev/pigtest/baseball_problem_4_output' USING PigStorage(',');

STORE batters_right_born_oct_died_2011_career_hits_ordered INTO 'hdfs:/user/maria_dev/pigtest/baseball_problem_4_output' USING CSVExcelStorage(',', 'NO_MULTILINE', 'WINDOWS');