fielding = LOAD 'hdfs:/user/maria_dev/pigtest/Fielding.csv' using PigStorage(',');
player_position_team_year_errors = FOREACH fielding GENERATE $0 AS player_id, $1 AS year, $2 AS team, $4 as position, $10 AS errors;
player_position_team_year_errors = DISTINCT player_position_team_year_errors;
player_position_team_year_errors_post_1950 = FILTER player_position_team_year_errors BY year > 1950;
team_year_errors_post_1950 = FOREACH player_position_team_year_errors_post_1950 GENERATE team, year, errors;
team_year_errors_post_1950_grouped = GROUP team_year_errors_post_1950 BY (team, year);
team_year_sum_errors_post_1950 = FOREACH team_year_errors_post_1950_grouped GENERATE FLATTEN(group), SUM(team_year_errors_post_1950.errors) AS sum_errors;
team_year_sum_errors_post_1950 = FOREACH team_year_sum_errors_post_1950 GENERATE $0 AS team, $1 AS year, sum_errors;
team_year_sum_errors_post_1950_ranked = RANK team_year_sum_errors_post_1950 BY sum_errors DESC;
team_year_sum_errors_post_1950_ranked = ORDER team_year_sum_errors_post_1950_ranked BY sum_errors DESC;
team_year_sum_errors_post_1950_ranked = FOREACH team_year_sum_errors_post_1950_ranked GENERATE $0 AS rank, team, year, sum_errors;
team_w_most_errors_in_a_season = FILTER team_year_sum_errors_post_1950_ranked BY rank == 1;
team_w_most_errors_in_a_season = FOREACH team_w_most_errors_in_a_season GENERATE team;
team_w_most_errors_in_a_season = LIMIT team_w_most_errors_in_a_season 10;
DUMP team_w_most_errors_in_a_season;






