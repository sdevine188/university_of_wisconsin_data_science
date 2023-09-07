-- 6) A player who hits well and doesn’t commit a lot of errors is obviously a player you want on your team. 
-- Output the playerID’s of the top 3 ranked players from 2005 through 2009 (including 2005 and 2009) who maximized the following criterion:
--(number of hits (H) / number of at bats (AB)) – (number of errors (E) / number of games (G))
-- The above equation might be skewed by a player who only had 3 at bats but got two hits. 
-- To account for that, only consider players who had at least 40 at bats and played in at least 20 games over that 
-- entire 5 year span. You should note that both files contain a “number of games” column. 
-- The 20 game minimum that you are using is from the Fielding file. 
-- For this problem, be sure to ignore rows in the Fielding file that are in the file for informational purposes only. 
-- An informational row contains no data in the 7th-17th columns (start counting at column 1). 
-- In other words, if all of the 7th, 8th, 9th, … 16th and 17th columns are empty, the row is informational and should be ignored.


-- load fielding2
DROP TABLE IF EXISTS fielding2;
CREATE EXTERNAL TABLE IF NOT EXISTS fielding2 (id STRING, year INT, team STRING, league STRING, position STRING, games INT, gs INT, 
    inn_outs INT, po INT, a INT, errors INT, dp INT, pb INT, wp INT, sb INT, cs INT, zr INT)  
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/fielding' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM fielding2 LIMIT 10;

-- get player_errors
CREATE VIEW player_errors AS
   SELECT id, SUM(errors) AS sum_errors, SUM(games) AS sum_games
   FROM fielding2
   WHERE (year >= 2005) AND (year <= 2009) 
   GROUP BY id;
   
-- inspect 
-- SELECT * FROM player_errors LIMIT 10;
   
-- get player_errors_filtered
CREATE VIEW player_errors_filtered AS       
    SELECT id, sum_errors, sum_games 
    FROM player_errors
    WHERE (sum_games >= 20);
   
-- inspect 
-- SELECT * FROM player_errors_filtered LIMIT 10;

-- get player_error_rate
CREATE VIEW player_error_rate AS
    SELECT id, sum_errors, sum_games, (sum_errors / sum_games) AS error_rate
    FROM player_errors_filtered
    ORDER BY error_rate ASC;

-- inspect 
-- SELECT * FROM player_error_rate LIMIT 10;



-- load batters
DROP TABLE IF EXISTS batting2;
CREATE EXTERNAL TABLE IF NOT EXISTS batting2 (id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

-- inspect 
-- SELECT * FROM batting2 LIMIT 10;

-- get player_hits
CREATE VIEW player_hits AS
   SELECT id, SUM(hits) AS sum_hits, SUM(ab) AS sum_at_bats
   FROM batting2
   WHERE (year >= 2005) AND (year <= 2009) 
   GROUP BY id;
   
-- inspect 
-- SELECT * FROM player_hits LIMIT 10;
   
-- get player_hits_filtered
CREATE VIEW player_hits_filtered AS       
    SELECT id, sum_hits, sum_at_bats 
    FROM player_hits
    WHERE (sum_at_bats >= 40);
   
-- inspect 
-- SELECT * FROM player_hits_filtered LIMIT 10;

-- get player_hit_rate
CREATE VIEW player_hit_rate AS
    SELECT id, sum_hits, sum_at_bats, (sum_hits / sum_at_bats) AS hit_rate
    FROM player_hits_filtered
    ORDER BY hit_rate DESC;

-- inspect 
-- SELECT * FROM player_hit_rate LIMIT 10;




-- get player_hit_rate_error_rate
CREATE VIEW player_hit_rate_error_rate AS
    SELECT x.id AS id, sum_hits, sum_at_bats, hit_rate, sum_errors, sum_games, error_rate
    FROM player_hit_rate x JOIN player_error_rate y 
    ON (x.id = y.id) 
    ORDER BY hit_rate DESC;
    
-- inspect 
-- SELECT * FROM player_hit_rate_error_rate LIMIT 10;

-- get player_hit_rate_error_rate_filtered
CREATE VIEW player_hit_rate_error_rate_filtered AS
    SELECT id, hit_rate, error_rate
    FROM player_hit_rate_error_rate
    WHERE (hit_rate >= 0) AND (error_rate >= 0);

-- inspect 
-- SELECT * FROM player_hit_rate_error_rate_filtered LIMIT 10;


-- get player_score
CREATE VIEW player_score AS
    SELECT id, hit_rate, error_rate, hit_rate - error_rate AS score
    FROM player_hit_rate_error_rate_filtered;
    
-- inspect 
-- SELECT * FROM player_score LIMIT 10;


-- get player_score_ranked
CREATE VIEW player_score_ranked AS
    SELECT id, hit_rate, error_rate, score, DENSE_RANK() OVER (ORDER BY score DESC) AS score_rank
    FROM player_score
    ORDER BY score DESC;
    
-- inspect
-- SELECT * FROM player_score_ranked LIMIT 10;

-- output top 3 players based on score
SELECT id
    FROM player_score_ranked
    WHERE score_rank == 1;






-- drop tables/views
DROP TABLE fielding2;
DROP TABLE batting2;
DROP VIEW player_errors;
DROP VIEW player_errors_filtered;
DROP VIEW player_error_rate;
DROP VIEW player_hits;
DROP VIEW player_hits_filtered;
DROP VIEW player_hit_rate;
DROP VIEW player_hit_rate_error_rate;
DROP VIEW player_hit_rate_error_rate_filtered;
DROP VIEW player_score;
DROP VIEW player_score_ranked;