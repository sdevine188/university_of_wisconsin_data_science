batters = LOAD 's3://pig.baseball.test.bucket/input/Batting.csv' using PigStorage(',');
realbatters = FILTER batters BY $1>0;
run_data = FOREACH realbatters GENERATE $0 AS id, $1 AS year, $6 AS runs;
grouped_by_year = GROUP run_data BY year;
best_per_year = FOREACH grouped_by_year GENERATE group, MAX(run_data.runs) AS best;
get_player_ids = JOIN best_per_year BY ($0, best), run_data BY (year,runs);
nicer_data = FOREACH get_player_ids GENERATE $0 AS year, $2 AS id, $4 AS runs;
names = LOAD ' s3://pig.baseball.test.bucket/input/Master.csv' using PigStorage(',');
master_data = FOREACH names GENERATE $0 AS id, $13 AS first, $14 AS last;
complete_data = JOIN nicer_data BY id, master_data BY id;
finished = FOREACH complete_data GENERATE $0 AS year, $4 AS first, $5 AS last, $2 AS runs;
sorted = ORDER finished BY year DESC;
STORE sorted INTO 's3://pig.baseball.test.bucket/baseballsorted' USING PigStorage(',');



# inspect 
complete_data_head = LIMIT complete_data 10;
DUMP complete_data_head;

