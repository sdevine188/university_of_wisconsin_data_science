fielding = LOAD 'hdfs:/user/maria_dev/pigtest/Fielding.csv' using PigStorage(',');
player_year_games_errors = FOREACH fielding GENERATE $0 AS player_id, $1 AS year, $5 AS games, $10 AS errors;
player_year_games_errors = FILTER player_year_games_errors BY year IN (2005, 2006, 2007, 2008, 2009);
player_year_games_errors = FILTER player_year_games_errors BY errors >= 0;
player_year_games_errors_grouped = GROUP player_year_games_errors BY player_id;
player_sum_games_sum_errors = FOREACH player_year_games_errors_grouped GENERATE group AS player_id, SUM(player_year_games_errors.games) AS sum_games, SUM(player_year_games_errors.errors) AS sum_errors;
batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');
batters = FILTER batters BY $1 > 0;
player_year_bats_hits = FOREACH batters GENERATE $0 AS player_id, $1 AS year, $5 AS bats, $7 as hits;
player_year_bats_hits = FILTER player_year_bats_hits BY year IN (2005, 2006, 2007, 2008, 2009);
player_year_bats_hits_grouped = GROUP player_year_bats_hits BY player_id;
player_sum_bats_sum_hits = FOREACH player_year_bats_hits_grouped GENERATE group AS player_id, SUM(player_year_bats_hits.bats) AS sum_bats, SUM(player_year_bats_hits.hits) AS sum_hits;
player_hit_rate_error_rate = JOIN player_sum_games_sum_errors BY (player_id), player_sum_bats_sum_hits BY (player_id);
player_hit_rate_error_rate = FOREACH player_hit_rate_error_rate GENERATE $0 AS player_id, sum_hits, sum_bats, sum_errors, sum_games;
player_hit_rate_error_rate = FILTER player_hit_rate_error_rate BY (sum_bats >= 40) AND (sum_games >= 20);
player_hit_rate_error_rate = FOREACH player_hit_rate_error_rate GENERATE player_id, sum_hits, sum_bats, (sum_hits / sum_bats) AS hit_rate, sum_errors, sum_games, (sum_errors / sum_games) AS error_rate;
player_hit_rate_error_rate = FOREACH player_hit_rate_error_rate GENERATE player_id, sum_hits, sum_bats, hit_rate, sum_errors, sum_games, error_rate, (hit_rate - error_rate) AS player_score;
player_hit_rate_error_rate_ranked = RANK player_hit_rate_error_rate BY player_score DESC DENSE;
player_hit_rate_error_rate_ranked = ORDER player_hit_rate_error_rate_ranked BY player_score DESC;
player_hit_rate_error_rate_ranked = FOREACH player_hit_rate_error_rate_ranked GENERATE $0 AS rank, player_id, player_score;
top_3_players = FILTER player_hit_rate_error_rate_ranked BY rank IN (1, 2, 3);
top_3_players = FOREACH top_3_players GENERATE player_id;
top_3_players = LIMIT top_3_players 10;
DUMP top_3_players;