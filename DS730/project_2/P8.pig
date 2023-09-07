batters = LOAD 'hdfs:/user/maria_dev/pigtest/Batting.csv' using PigStorage(',');
batters_clean = FILTER batters BY $1 > 0;
player_hits_bats = FOREACH batters_clean GENERATE $0 AS player_id, $5 AS bats, $7 AS hits;
master = LOAD 'hdfs:/user/maria_dev/pigtest/Master.csv' using PigStorage(',');
player_birth_month_state = FOREACH master GENERATE $0 AS player_id, $2 AS birth_month, $5 AS birth_state;
player_hits_bats_birth_month_state = JOIN player_hits_bats BY (player_id), player_birth_month_state BY (player_id);
player_hits_bats_birth_month_state = FILTER player_hits_bats_birth_month_state BY (birth_month is not null) AND NOT(birth_month MATCHES '') AND (birth_state is not null) AND NOT(birth_state MATCHES '');
player_hits_bats_birth_month_state = FOREACH player_hits_bats_birth_month_state GENERATE $0 AS player_id, CONCAT(birth_month, '_', birth_state) AS birth_month_state, bats, hits; 
player_hits_bats_birth_month_state_grouped = GROUP player_hits_bats_birth_month_state BY birth_month_state;
birth_month_state_sum_hits_sum_bats_player_count = FOREACH player_hits_bats_birth_month_state_grouped GENERATE group AS birth_month_state, COUNT(player_hits_bats_birth_month_state.player_id) AS player_id_count, SUM(player_hits_bats_birth_month_state.hits) AS sum_hits, SUM(player_hits_bats_birth_month_state.bats) AS sum_bats;
birth_month_state_sum_hits_sum_bats_player_count = FILTER birth_month_state_sum_hits_sum_bats_player_count BY (player_id_count >= 10) AND (sum_bats > 1500); 
birth_month_state_score = FOREACH birth_month_state_sum_hits_sum_bats_player_count GENERATE birth_month_state, sum_hits, sum_bats, (sum_hits / sum_bats) AS hit_rate;
birth_month_state_score_ranked = RANK birth_month_state_score BY hit_rate ASC DENSE;
birth_month_state_score_ranked = ORDER birth_month_state_score_ranked BY hit_rate ASC;
birth_month_state_score_ranked = FOREACH birth_month_state_score_ranked GENERATE $0 AS rank, birth_month_state, hit_rate;
birth_month_state_w_lowest_hit_rate = FILTER birth_month_state_score_ranked BY rank == 1;
birth_month_state_w_lowest_hit_rate = FOREACH birth_month_state_w_lowest_hit_rate GENERATE birth_month_state;
birth_month_state_w_lowest_hit_rate = LIMIT birth_month_state_w_lowest_hit_rate 10;
DUMP birth_month_state_w_lowest_hit_rate;





