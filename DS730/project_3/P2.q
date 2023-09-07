-- 2) Output the top three ranked birthdates that had the most players born. I am only looking for day and month combinations. 
-- For instance, how many were born on February 3rd, how many were born on March 8th, how many were born on July 20th… print out 
-- the top three mm/dd combinations. Filter out any person who has no birthMonth or no birthDay.


-- load master2
DROP TABLE IF EXISTS master2;
CREATE EXTERNAL TABLE IF NOT EXISTS master2 (id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, 
    bcity STRING, dyear INT, dmonth INT, dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, 
    name STRING, weight INT, height INT, bats STRING, throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM master2 LIMIT 10;

-- get player_birthdate
CREATE VIEW player_birthdate AS
    SELECT id, bday, bmonth, CONCAT(bmonth, '_', bday) AS birthdate 
    FROM master2
    WHERE isnotnull(bday) AND isnotnull(bmonth);
    
-- inspect 
-- SELECT * FROM player_birthdate LIMIT 10;
    
-- get birthdate_player_count
CREATE VIEW birthdate_player_count AS
   SELECT birthdate, COUNT(DISTINCT id) AS player_count
   FROM player_birthdate
   GROUP BY birthdate
   ORDER BY player_count DESC;
    
-- inspect 
-- SELECT * FROM birthdate_player_count LIMIT 10;

-- get birthdate_player_count_ranked
CREATE VIEW birthdate_player_count_ranked AS
    SELECT *, DENSE_RANK() OVER (ORDER BY player_count DESC) AS player_count_rank
    FROM birthdate_player_count
    ORDER BY player_count DESC;
    
-- inspect
-- SELECT * FROM birthdate_player_count_ranked LIMIT 10;

-- output top 3 birthdates
SELECT birthdate
    FROM birthdate_player_count_ranked
    WHERE player_count_rank IN (1, 2, 3);


-- drop tables/views
DROP TABLE batting2;
DROP TABLE master2;
DROP VIEW player_birthdate;
DROP VIEW birthdate_player_count;
DROP VIEW birthdate_player_count_ranked;