-- 5) Output the playerID(s) of the player who had the most errors (E) in all seasons combined. 

-- load fielding2
DROP TABLE IF EXISTS fielding2;
CREATE EXTERNAL TABLE IF NOT EXISTS fielding2 (id STRING, year INT, team STRING, league STRING, position STRING, g INT, gs INT, 
    inn_outs INT, po INT, a INT, errors INT, dp INT, pb INT, wp INT, sb INT, cs INT, zr INT)  
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/fielding' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM fielding2 LIMIT 10;

-- get player_errors
CREATE VIEW player_errors AS
   SELECT id, SUM(errors) AS sum_errors
   FROM fielding2
   GROUP BY id
   ORDER BY sum_errors DESC;
   
-- inspect 
-- SELECT * FROM player_errors LIMIT 10;

-- get player_errors_ranked
CREATE VIEW player_errors_ranked AS
    SELECT *, DENSE_RANK() OVER (ORDER BY sum_errors DESC) AS sum_errors_rank
    FROM player_errors
    ORDER BY sum_errors DESC;
    
-- inspect
-- SELECT * FROM player_errors_ranked LIMIT 10;

-- output player with most errors
SELECT id
    FROM player_errors_ranked
    WHERE sum_errors_rank == 1;


-- drop tables/views
DROP TABLE fielding2;
DROP VIEW player_errors;
DROP VIEW player_errors_ranked;