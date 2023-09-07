batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');
batters = FILTER batters BY $1 > 0;
batters_rbi = FOREACH batters GENERATE $0 AS player_id, $11 AS rbi;
batters_rbi_grouped = GROUP batters_rbi BY player_id;
batters_career_rbi = FOREACH batters_rbi_grouped GENERATE group, SUM(batters_rbi.rbi) AS career_rbi;
batters_career_rbi = FOREACH batters_career_rbi GENERATE $0 AS player_id, career_rbi;
batters_career_rbi_ordered = ORDER batters_career_rbi BY career_rbi DESC;
master = LOAD 'hdfs:/user/maria_dev/pigtest/Master.csv' using PigStorage(',');
player_birth_city = FOREACH master GENERATE $0 AS player_id, $6 AS birth_city;
batters_career_rbi_ordered_w_birth_city = JOIN batters_career_rbi_ordered BY player_id, player_birth_city BY player_id;
batters_career_rbi_ordered_w_birth_city = FOREACH batters_career_rbi_ordered_w_birth_city GENERATE $0 AS player_id, $1 AS career_rbi, $3 AS birth_city;
batters_career_rbi_ordered_w_birth_city = ORDER batters_career_rbi_ordered_w_birth_city BY career_rbi DESC;
batters_career_rbi_ordered_ranked_w_birth_city = RANK batters_career_rbi_ordered_w_birth_city BY career_rbi DESC;
birth_city_for_player_with_most_career_rbi = FILTER batters_career_rbi_ordered_ranked_w_birth_city BY $0 == 1;
birth_city_for_player_with_most_career_rbi = FOREACH birth_city_for_player_with_most_career_rbi GENERATE birth_city;
birth_city_for_player_with_most_career_rbi_output = LIMIT birth_city_for_player_with_most_career_rbi 10; 
DUMP birth_city_for_player_with_most_career_rbi_output;