-- 4) Output the team(s) that had the most errors (E) in 2001.

-- load fielding2
DROP TABLE IF EXISTS fielding2;
CREATE EXTERNAL TABLE IF NOT EXISTS fielding2 (id STRING, year INT, team STRING, league STRING, position STRING, g INT, gs INT, 
    inn_outs INT, po INT, a INT, errors INT, dp INT, pb INT, wp INT, sb INT, cs INT, zr INT)  
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/fielding' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM fielding2 LIMIT 10;

-- get team_errors
CREATE VIEW team_errors AS
   SELECT team, SUM(errors) AS sum_errors
   FROM fielding2
   WHERE year == 2001
   GROUP BY team
   ORDER BY sum_errors DESC;
   
-- inspect 
-- SELECT * FROM team_errors LIMIT 10;

-- get team_errors_ranked
CREATE VIEW team_errors_ranked AS
    SELECT *, DENSE_RANK() OVER (ORDER BY sum_errors DESC) AS sum_errors_rank
    FROM team_errors
    ORDER BY sum_errors DESC;
    
-- inspect
-- SELECT * FROM team_errors_ranked LIMIT 10;

-- output team with most errors in 2001
SELECT team
    FROM team_errors_ranked
    WHERE sum_errors_rank == 1;


-- drop tables/views
DROP TABLE fielding2;
DROP VIEW team_errors;
DROP VIEW team_errors_ranked;