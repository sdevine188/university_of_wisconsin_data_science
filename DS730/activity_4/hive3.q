-- 3) What player had the most extra base hits during the entire 1980’s (1980 to 1989)? 
-- Note that this question is not asking about any 1 specific year. It is asking about the entire 10 year span in the 80’s. 
-- An extra base hit is a double, triple or home run (columns 2B, 3B, HR).


-- load batters
DROP TABLE IF EXISTS batting2;
CREATE EXTERNAL TABLE IF NOT EXISTS batting2 (id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

-- inspect master2
-- SELECT * FROM batting2 LIMIT 10;

-- get player_extra_base_hits_per_year
CREATE VIEW player_extra_base_hits_per_year AS 
    SELECT id, year, doubles, triples, homeruns, doubles + triples + homeruns AS extra_base_hits
    FROM batting2
    WHERE year IN (1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989)
    ORDER BY id;
    
-- inspect
-- SELECT * FROM player_extra_base_hits_per_year LIMIT 10;

-- get player_extra_base_hits_1980s
CREATE VIEW player_extra_base_hits_1980s AS
    SELECT id, SUM(extra_base_hits) AS extra_base_hits_sum
    FROM player_extra_base_hits_per_year
    GROUP BY id
    ORDER BY extra_base_hits_sum DESC;
    
-- inspect
-- SELECT * FROM player_extra_base_hits_1980s LIMIT 10;

-- get player_extra_base_hits_1980s_ranked
CREATE VIEW player_extra_base_hits_1980s_ranked AS
    SELECT *, DENSE_RANK() OVER (ORDER BY extra_base_hits_sum DESC) AS extra_base_hits_sum_rank
    FROM player_extra_base_hits_1980s
    ORDER BY extra_base_hits_sum DESC;

-- inspect
-- SELECT * FROM player_extra_base_hits_1980s_ranked LIMIT 10;

-- output player with most extra_base_hits in 1980s
SELECT id 
    FROM player_extra_base_hits_1980s_ranked 
    WHERE extra_base_hits_sum_rank == 1;



-- drop tables/views
DROP TABLE batting2;
DROP VIEW player_extra_base_hits_per_year;
DROP VIEW player_extra_base_hits_1980s;
DROP VIEW player_extra_base_hits_1980s_ranked;