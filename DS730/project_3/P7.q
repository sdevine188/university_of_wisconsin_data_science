-- 8) Sum up the number of doubles and triples for each birthCity/birthState combination. 
-- Output the top 5 ranked birthCity/birthState combinations that produced the 
-- players who had the most doubles and triples (i.e. combine the doubles and triples for all players with that city/state combination). 
-- A birthState is any non-empty value in the birthState column.


-- load batters
DROP TABLE IF EXISTS batting2;
CREATE EXTERNAL TABLE IF NOT EXISTS batting2 (id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

-- inspect 
-- SELECT * FROM batting2 LIMIT 10;

-- get player_doubles_triples_sum_records
CREATE VIEW player_doubles_triples_sum_records AS
    SELECT id, doubles, triples, doubles + triples AS hits_2_3
    FROM batting2;

-- inspect 
-- SELECT * FROM player_doubles_triples_sum_records LIMIT 10;

-- get player_doubles_triples_sum
CREATE VIEW player_doubles_triples_sum AS
    SELECT id, SUM(hits_2_3) AS hits_2_3_sum
    FROM player_doubles_triples_sum_records
    WHERE isnotnull(hits_2_3)
    GROUP BY id;

-- inspect 
-- SELECT * FROM player_doubles_triples_sum LIMIT 10;





-- load master2
DROP TABLE IF EXISTS master2;
CREATE EXTERNAL TABLE IF NOT EXISTS master2 (id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, 
    bcity STRING, dyear INT, dmonth INT, dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, 
    name STRING, weight INT, height INT, bats STRING, throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM master2 LIMIT 10;

-- get player_birth_city_state
CREATE VIEW player_birth_city_state AS
    SELECT id, bcity, bstate, CONCAT(bcity, "_", bstate) AS birth_city_state
    FROM master2;

-- inspect 
-- SELECT * FROM player_birth_city_state LIMIT 10;




-- get player_doubles_triples_sum_birth_city_state
CREATE VIEW player_doubles_triples_sum_birth_city_state AS
    SELECT x.id AS id, birth_city_state, hits_2_3_sum
    FROM player_birth_city_state x JOIN player_doubles_triples_sum y 
    ON (x.id = y.id) 
    ORDER BY hits_2_3_sum DESC;
    
-- inspect 
-- SELECT * FROM player_doubles_triples_sum_birth_city_state LIMIT 10;

-- get doubles_triples_sum_by_birth_city_state
CREATE VIEW doubles_triples_sum_by_birth_city_state AS
    SELECT birth_city_state, SUM(hits_2_3_sum) AS hits_2_3_per_birth_city_state
    FROM player_doubles_triples_sum_birth_city_state
    GROUP BY birth_city_state;

-- inspect 
-- SELECT * FROM doubles_triples_sum_by_birth_city_state LIMIT 10;

-- get doubles_triples_sum_by_birth_city_state_ranked
CREATE VIEW doubles_triples_sum_by_birth_city_state_ranked AS
    SELECT birth_city_state, hits_2_3_per_birth_city_state, DENSE_RANK() OVER (ORDER BY hits_2_3_per_birth_city_state DESC) AS hits_2_3_per_birth_city_state_rank
    FROM doubles_triples_sum_by_birth_city_state
    ORDER BY hits_2_3_per_birth_city_state DESC;
    
-- inspect
-- SELECT * FROM doubles_triples_sum_by_birth_city_state_ranked LIMIT 10;

-- output top 3 players based on score
SELECT birth_city_state
    FROM doubles_triples_sum_by_birth_city_state_ranked
    WHERE hits_2_3_per_birth_city_state_rank <= 5;





-- drop tables/views
DROP TABLE master2;
DROP TABLE batting2;
DROP VIEW player_doubles_triples_sum_records;
DROP VIEW player_doubles_triples_sum;
DROP VIEW player_birth_city_state;
DROP VIEW player_doubles_triples_sum_birth_city_state;
DROP VIEW doubles_triples_sum_by_birth_city_state;
DROP VIEW doubles_triples_sum_by_birth_city_state_ranked;