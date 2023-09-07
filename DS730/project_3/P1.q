-- 1) Output the birth city (or cities) of the player(s) who had the most at bats (AB) in his career.

-- load batters
DROP TABLE IF EXISTS batting2;
CREATE EXTERNAL TABLE IF NOT EXISTS batting2 (id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

-- inspect 
-- SELECT * FROM batting2 LIMIT 10;

-- get player_career_at_bats
CREATE VIEW player_career_at_bats AS 
    SELECT id, SUM(ab) AS career_at_bats
    FROM batting2
    GROUP BY id;

-- inspect
-- SELECT * FROM player_career_at_bats LIMIT 10;



-- load master2
DROP TABLE IF EXISTS master2;
CREATE EXTERNAL TABLE IF NOT EXISTS master2 (id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, 
    bcity STRING, dyear INT, dmonth INT, dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, 
    name STRING, weight INT, height INT, bats STRING, throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM master2 LIMIT 10;

-- get player_career_at_bats_birth_city
CREATE VIEW player_career_at_bats_birth_city AS
    SELECT x.id AS id, bcity, career_at_bats
    FROM master2 x JOIN player_career_at_bats y 
    ON (x.id = y.id) 
    ORDER BY career_at_bats DESC;
    
-- inspect 
-- SELECT * FROM player_career_at_bats_birth_city LIMIT 10;

-- get player_career_at_bats_birth_city_ranked
CREATE VIEW player_career_at_bats_birth_city_ranked AS
    SELECT *, DENSE_RANK() OVER (ORDER BY career_at_bats DESC) AS career_at_bats_rank
    FROM player_career_at_bats_birth_city
    ORDER BY career_at_bats DESC;
    
-- inspect
-- SELECT * FROM player_career_at_bats_birth_city_ranked LIMIT 10;

-- output birth city for players with most career hits
SELECT bcity 
    FROM player_career_at_bats_birth_city_ranked
    WHERE career_at_bats_rank == 1;
    
    
-- drop tables/views
DROP TABLE batting2;
DROP TABLE master2;
DROP VIEW player_career_at_bats;
DROP VIEW player_career_at_bats_birth_city;
DROP VIEW player_career_at_bats_birth_city_ranked;
