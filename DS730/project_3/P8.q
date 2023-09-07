-- 8) Output the birthMonth/birthState combination(s) that produced the worst players. The worst players are defined by the lowest of:

-- (number of hits (H) / number of at bats (AB))

-- To ensure 1 player who barely played does not skew the data, make sure that:
-- at least 5 people came from the same state and were born in the same month and
-- the sum of the at-bats for all of the players from the same month/state exceeds 100.
-- For this problem, the year does not matter. A player born in December, 1970 in La Romana and a player born in December, 
-- 1982 in La Romana are in the same group because they were both born in December and were born in La Romana. 
-- A birthState is any non-empty value in the birthState column. In terms of condition a., 
-- you should count a player as one of your 5 players even if the player has no at-bats and/or no hits. 
-- You should ignore all players who do not have a birthMonth or who do not have a birthState.


-- load batters
DROP TABLE IF EXISTS batting2;
CREATE EXTERNAL TABLE IF NOT EXISTS batting2 (id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

-- inspect 
-- SELECT * FROM batting2 LIMIT 10;

-- get player_hits_at_bats
CREATE VIEW player_hits_at_bats AS 
    SELECT id, hits, ab AS at_bats, 
    FROM batting2;

-- inspect 
-- SELECT * FROM player_hits_at_bats LIMIT 10;

-- get player_hit_rate
CREATE VIEW player_hit_rate AS
    SELECT id, hits, at_bats, (hits / at_bats) AS hit_rate
    FROM player_hits_at_bats
    WHERE isnotnull(hits) AND isnotnull(at_bats);
    
-- inspect 
-- SELECT * FROM player_hit_rate LIMIT 10;





-- load master2
DROP TABLE IF EXISTS master2;
CREATE EXTERNAL TABLE IF NOT EXISTS master2 (id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, 
    bcity STRING, dyear INT, dmonth INT, dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, 
    name STRING, weight INT, height INT, bats STRING, throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM master2 LIMIT 10;

-- get player_birth_month_state
CREATE VIEW player_birth_month_state AS
    SELECT id, bmonth, bstate, CONCAT(bmonth, "_", bstate) AS birth_month_state
    FROM master2
    WHERE isnotnull(bmonth) AND isnotnull(bstate);

-- inspect 
-- SELECT * FROM player_birth_month_state LIMIT 10;





-- get player_hit_rate_birth_month_state
CREATE VIEW player_hit_rate_birth_month_state AS
    SELECT x.id AS id, birth_month_state, hits, at_bats, hit_rate
    FROM player_birth_month_state x JOIN player_hit_rate y 
    ON (x.id = y.id);
    
-- inspect 
-- SELECT * FROM player_hit_rate_birth_month_state LIMIT 10;

    
-- get player_count_and_at_bats_sum_per_birth_month_state
CREATE VIEW player_count_and_at_bats_sum_per_birth_month_state AS
    SELECT birth_month_state, COUNT(DISTINCT id) AS player_count, SUM(at_bats) AS at_bats_sum
    FROM player_hit_rate_birth_month_state
    WHERE isnotnull(birth_month_state)
    GROUP BY birth_month_state;
    
-- inspect 
-- SELECT * FROM player_count_and_at_bats_sum_per_birth_month_state LIMIT 10;

-- get birth_month_state_w_5_players_and_100_at_bats
CREATE VIEW birth_month_state_w_5_players_and_100_at_bats AS
    SELECT birth_month_state, player_count, at_bats_sum
    FROM player_count_and_at_bats_sum_per_birth_month_state
    WHERE (player_count >= 5) AND (at_bats_sum > 100);
    
-- inspect 
-- SELECT * FROM birth_month_state_w_5_players_and_100_at_bats LIMIT 10;


-- get player_hit_rate_birth_month_state_filtered
CREATE VIEW player_hit_rate_birth_month_state_filtered AS
    SELECT x.birth_month_state AS birth_month_state, player_count, at_bats_sum, hit_rate, 
    FROM birth_month_state_w_5_players_and_100_at_bats x 
    JOIN player_hit_rate_birth_month_state y 
    ON (x.birth_month_state = y.birth_month_state);




    
-- get player_hit_rate_birth_month_state_filtered_ranked
CREATE VIEW player_hit_rate_birth_month_state_filtered_ranked AS
    SELECT birth_month_state, hit_rate, DENSE_RANK() OVER (ORDER BY hit_rate ASC) AS hit_rate_rank
    FROM player_hit_rate_birth_month_state_filtered
    ORDER BY hit_rate ASC;
    
-- inspect
-- SELECT * FROM player_hit_rate_birth_month_state_filtered_ranked LIMIT 10;

-- output top 3 players based on score
SELECT birth_city_state
    FROM player_hit_rate_birth_month_state_filtered_ranked
    WHERE hit_rate_rank == 1;    
    



-- drop tables/views
DROP TABLE master2;
DROP TABLE batting2;
DROP VIEW player_hits_at_bats;
DROP VIEW player_hit_rate;
DROP VIEW player_birth_month_state;
DROP VIEW player_hit_rate_birth_month_state;
DROP VIEW player_count_and_at_bats_sum_per_birth_month_state;
DROP VIEW birth_month_state_w_5_players_and_100_at_bats;
DROP VIEW player_hit_rate_birth_month_state_filtered;
DROP VIEW player_hit_rate_birth_month_state_filtered_ranked;