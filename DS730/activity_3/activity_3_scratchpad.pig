batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');
realbatters = FILTER batters BY $1>0;
run_data = FOREACH realbatters GENERATE $0 AS id, $1 AS year, $6 AS runs;
grouped_by_year = GROUP run_data BY year;
best_per_year = FOREACH grouped_by_year GENERATE group, MAX(run_data.runs) AS best;
get_player_ids = JOIN best_per_year BY ($0, best), run_data BY (year,runs);
nicer_data = FOREACH get_player_ids GENERATE $0 AS year, $2 AS id, $4 AS runs;
names = LOAD 'hdfs:/user/maria_dev/pigtest/Master.csv' using PigStorage(',');
master_data = FOREACH names GENERATE $0 AS id, $13 AS first, $14 AS last;
complete_data = JOIN nicer_data BY id, master_data BY id;
finished = FOREACH complete_data GENERATE $0 AS year, $4 AS first, $5 AS last, $2 AS runs;
sorted = ORDER finished BY year DESC;
STORE sorted INTO 'hdfs:/user/maria_dev/pigtest/baseballsorted' USING PigStorage(',');
DUMP sorted;


#################################################################################################

# set data type
data = FOREACH realbatters GENERATE $0 AS id, (int)$1 AS year:int, (int)$8 AS doubles:int, (int)$9 AS triples:int, (int)$10 AS hr:int;

#################################################################################################

# arrange
sorted = ORDER finished BY year DESC;

#################################################################################################

# nrow
B = GROUP A ALL;
B_COUNT = FOREACH B GENERATE COUNT(A);
DUMP B_COUNT;

#################################################################################################

# select
run_data = FOREACH realbatters GENERATE $0 AS id, $1 AS year, $6 AS runs;

#################################################################################################

# join
complete_data = JOIN nicer_data BY id, master_data BY id;

get_player_ids = JOIN best_per_year BY ($0, best), run_data BY (year,runs);

#################################################################################################


# load batters
batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');
batters_head = LIMIT batters 10;
DUMP batters_head; 
DESCRIBE batters; 

# load masters
master = LOAD 'hdfs:/user/maria_dev/pigtest/Master.csv' using PigStorage(',');
master_head = LIMIT master 10;
DUMP master_head; 


#################################################################################################
#################################################################################################
#################################################################################################


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


# problem 2
# In the batting file, if a player played for more than 1 team in a season, that player will have his name show up in 
# multiple tuples with the same year. For example, in 2011, Francisco Rodriguez (rodrifr03) played for the New York Mets and then 
# played for the Milwaukee Brewers (see tuples 95279 and 95280). The question you have to answer is this: what player played for the 
# most teams in any single season (a season is 1 year)? A player may have played for the same team twice in the same season at different 
# times in the season. If this is the case, you should count this as two different teams.


# get distinct player/year/team records
# group by player/year and get count of teams
# sort to find highest count of teams

# load batters
batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');

# inspect 
batters_head = LIMIT batters 10;
DUMP batters_head;

# get batters_teams
batters_teams = FOREACH batters GENERATE $0 AS player_id, $1 AS year, $2 AS team;

# inspect 
batters_teams_head = LIMIT batters_teams 10;
DUMP batters_teams_head;

# check multiple teams example
multiple_teams_example = FILTER batters_teams BY player_id == 'rodrifr03';
multiple_teams_example_head = LIMIT multiple_teams_example 50;
DUMP multiple_teams_example_head;

# group by player/year
batters_teams_grouped = GROUP batters_teams BY (player_id, year);

# inspect 
batters_teams_grouped_head = LIMIT batters_teams_grouped 10;
DUMP batters_teams_grouped_head;

# get count of teams for each player/year
batters_teams_count = FOREACH batters_teams_grouped GENERATE group, COUNT(batters_teams.player_id) AS team_count;

# inspect 
batters_teams_count_head = LIMIT batters_teams_count 10;
DUMP batters_teams_count_head;

# order by descending team_count
batters_teams_count_ordered = ORDER batters_teams_count BY team_count DESC;

# inspect 
batters_teams_count_ordered_head = LIMIT batters_teams_count_ordered 10;
DUMP batters_teams_count_ordered_head;

# ungroup to extract player_id
batters_teams_count_ordered_ungrouped = FOREACH batters_teams_count_ordered GENERATE FLATTEN(group), team_count;

# inspect 
batters_teams_count_ordered_ungrouped_head = LIMIT batters_teams_count_ordered_ungrouped 10;
DUMP batters_teams_count_ordered_ungrouped_head;

# get player with highest team_count in a year
player_w_highest_team_count_in_a_year = LIMIT batters_teams_count_ordered_ungrouped 1;
player_w_highest_team_count_in_a_year = FOREACH player_w_highest_team_count_in_a_year GENERATE player_id;

# inspect 
player_w_highest_team_count_in_a_year_head = LIMIT player_w_highest_team_count_in_a_year 10;
DUMP player_w_highest_team_count_in_a_year_head;

# print player_w_highest_team_count_in_a_year
# problem 2 answer: (huelsfr01) 
DUMP player_w_highest_team_count_in_a_year;