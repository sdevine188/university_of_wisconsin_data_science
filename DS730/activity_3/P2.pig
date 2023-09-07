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