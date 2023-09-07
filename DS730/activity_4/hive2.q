-- In the batting file, if a player played for more than 1 team in a season, that player will have his name show up 
--in multiple tuples with the same year. For example, in 2011, Francisco Rodriguez (rodrifr03) played for the 
--New York Mets and then played for the Milwaukee Brewers (see tuples 95279 and 95280). 
--The question you have to answer is this: what player played for the most teams in any single season? 
--A player may have played for the same team twice in the same season at different times in the season. 
--If this is the case, you should count this as two different teams.


-- load batters
DROP TABLE IF EXISTS batting2;
CREATE EXTERNAL TABLE IF NOT EXISTS batting2 (id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

-- inspect master2
-- SELECT * FROM batting2 LIMIT 10;

-- get player with higest annual_team_count
CREATE VIEW player_annual_team_count AS
    SELECT id, year, COUNT(DISTINCT team) AS annual_team_count
    FROM batting2 
    GROUP BY id, year
    ORDER BY annual_team_count DESC;
    
-- inspect
-- SELECT * FROM player_annual_team_count LIMIT 10;

-- get player_annual_team_count_ranked
CREATE VIEW player_annual_team_count_ranked AS 
    SELECT *, DENSE_RANK() OVER (ORDER BY annual_team_count DESC) AS annual_team_count_rank
    FROM player_annual_team_count
    ORDER BY annual_team_count_rank;
    
-- inspect
-- SELECT * FROM player_annual_team_count_ranked LIMIT 10;

-- output player with highest annual_team_count
SELECT id FROM player_annual_team_count_ranked LIMIT 1;



-- drop tables/views
DROP TABLE batting2;
DROP TABLE master2;
DROP VIEW player_annual_team_count;
DROP VIEW player_annual_team_count_ranked;