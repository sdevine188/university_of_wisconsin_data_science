-- Who was the heaviest player (weight) to hit more than 5 triples (3B) in 2005?

-- load master2
DROP TABLE IF EXISTS master2;
CREATE EXTERNAL TABLE IF NOT EXISTS master2 (id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, 
    bcity STRING, dyear INT, dmonth INT, dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, 
    name STRING, weight INT, height INT, bats STRING, throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM master2 LIMIT 10;



-- load batters
DROP TABLE IF EXISTS batting2;
CREATE EXTERNAL TABLE IF NOT EXISTS batting2 (id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

-- inspect 
-- SELECT * FROM batting2 LIMIT 10;

-- load id_triples_2005
CREATE VIEW id_triples_2005 AS SELECT id, year, triples FROM batting WHERE triples > 5 AND year == 2005;

-- inspect 
-- SELECT * FROM id_triples_2005 LIMIT 10;


    
    
-- join id_triples_2005 with master2, order by weight_rank
CREATE VIEW id_triples_2005_weight AS 
    SELECT id_triples_2005.id, year, triples, weight, DENSE_RANK() OVER (ORDER BY weight DESC) AS weight_rank 
    FROM id_triples_2005 JOIN master2 ON (id_triples_2005.id = master2.id) 
    ORDER BY weight_rank;

-- inspect
-- SELECT * FROM id_triples_2005_weight LIMIT 10;
    
-- output heaviest player with more than 5 triples in 2005
SELECT id FROM id_triples_2005_weight LIMIT 1;

-- drop tables/views
DROP TABLE batting2;
DROP TABLE master2;
DROP VIEW id_triples_2005;
DROP VIEW id_triples_2005_weight;
