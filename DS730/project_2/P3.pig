master = LOAD 'hdfs:/user/maria_dev/pigtest/Master.csv' using PigStorage(',');
player_height = FOREACH master GENERATE $0 AS player_id, $17 AS height;
player_height = DISTINCT player_height;
player_height = FILTER player_height BY height > 0;
player_height_grouped = GROUP player_height BY height;
player_height_count = FOREACH player_height_grouped GENERATE group AS height, COUNT(player_height.player_id) AS player_count;
player_height_unique = FILTER player_height_count BY player_count == 1;
player_names_w_unique_height = FOREACH master GENERATE $0 AS player_id, $13 AS first_name, $14 AS last_name, $17 AS height;
player_names_w_unique_height = FILTER player_names_w_unique_height BY height > 0;
player_names_w_unique_height = FILTER player_names_w_unique_height BY height IN (player_height_unique.height);
player_names_w_unique_height = FOREACH master GENERATE $0 AS player_id, $13 AS first_name, $14 AS last_name, $17 AS height;
player_names_w_unique_height = FILTER player_names_w_unique_height BY height > 0;
player_names_w_unique_height = JOIN player_height_unique BY (height) LEFT, player_names_w_unique_height BY (height);
player_names_w_unique_height_output = FOREACH player_names_w_unique_height GENERATE CONCAT(first_name, ' ', last_name);
DUMP player_names_w_unique_height_output;



