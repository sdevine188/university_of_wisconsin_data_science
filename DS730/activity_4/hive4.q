-- problem 4) Of the right-handed batters who were born in October and died in 2011, which one had the most hits in his career? 
-- The column with the heading of H is the hits column. Do not consider switch hitters to be right-handed batters.

-- load batters
DROP TABLE IF EXISTS batting2;
CREATE EXTERNAL TABLE IF NOT EXISTS batting2 (id STRING, year INT, team STRING, league STRING, games INT, ab INT, runs INT, hits INT, 
    doubles INT, triples INT, homeruns INT, rbi INT, sb INT, cs INT, walks INT, strikeouts INT, ibb INT, hbp INT, sh INT, sf INT, gidp INT) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/batting';

-- inspect 
-- SELECT * FROM batting2 LIMIT 10;


-- load master2
DROP TABLE IF EXISTS master2;
CREATE EXTERNAL TABLE IF NOT EXISTS master2 (id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, 
    bcity STRING, dyear INT, dmonth INT, dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, 
    name STRING, weight INT, height INT, bats STRING, throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM master2 LIMIT 10;


-- get player_right_hand_born_oct_died_2011
CREATE VIEW player_right_hand_born_oct_died_2011 AS
    SELECT id, bmonth, dyear, bats 
    FROM master2
    WHERE (bats = 'R') AND (bmonth == 10) AND (dyear == 2011);
    
-- inspect
-- SELECT * FROM player_right_hand_born_oct_died_2011 LIMIT 10;



-- get player_career_hits
CREATE VIEW player_career_hits AS 
    SELECT id, SUM(hits) AS career_hits
    FROM batting2
    GROUP BY id;

-- inspect
-- SELECT * FROM player_career_hits LIMIT 10;


-- get player_right_hand_born_oct_died_2011_career_hits
CREATE VIEW player_right_hand_born_oct_died_2011_career_hits AS
    SELECT x.id AS id, bmonth, dyear, bats, career_hits
    FROM player_right_hand_born_oct_died_2011 x JOIN player_career_hits y 
    ON (x.id = y.id) 
    ORDER BY career_hits DESC;
    
-- inspect
-- SELECT * FROM player_right_hand_born_oct_died_2011_career_hits LIMIT 10;

-- get player_right_hand_born_oct_died_2011_career_hits_ranked
CREATE VIEW player_right_hand_born_oct_died_2011_career_hits_ranked AS
    SELECT *, DENSE_RANK() OVER (ORDER BY career_hits DESC) AS career_hits_rank
    FROM player_right_hand_born_oct_died_2011_career_hits
    ORDER BY career_hits DESC;
    
-- inspect
-- SELECT * FROM player_right_hand_born_oct_died_2011_career_hits_ranked LIMIT 10;

-- output player born in oct, died in 2011, right-handed, with most career hits
SELECT id 
    FROM player_right_hand_born_oct_died_2011_career_hits_ranked
    WHERE career_hits_rank == 1;
    
    
-- drop tables/views
DROP TABLE batting2;
DROP TABLE master2;
DROP VIEW player_right_hand_born_oct_died_2011;
DROP VIEW player_career_hits;
DROP VIEW player_right_hand_born_oct_died_2011_career_hits;
DROP VIEW player_right_hand_born_oct_died_2011_career_hits_ranked;