3) Output the second most common weight by rank.

-- load master2
DROP TABLE IF EXISTS master2;
CREATE EXTERNAL TABLE IF NOT EXISTS master2 (id STRING, byear INT, bmonth INT, bday INT, bcountry STRING, bstate STRING, 
    bcity STRING, dyear INT, dmonth INT, dday INT, dcountry STRING, dstate STRING, dcity STRING, fname STRING, lname STRING, 
    name STRING, weight INT, height INT, bats STRING, throws STRING, debut STRING, finalgame STRING, retro STRING, bbref STRING) 
    ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' LOCATION '/user/maria_dev/hivetest/master' tblproperties ("skip.header.line.count"="1");
    
-- inspect 
-- SELECT * FROM master2 LIMIT 10;

-- get weight_count_tbl
CREATE VIEW weight_count_tbl AS
    SELECT weight, COUNT(*) AS weight_count
    FROM master2
    GROUP BY weight
    ORDER BY weight_count DESC;
        
-- inspect 
-- SELECT * FROM weight_count_tbl LIMIT 10;

-- get weight_count_ranked
CREATE VIEW weight_count_ranked AS
    SELECT *, DENSE_RANK() OVER (ORDER BY weight_count DESC) AS weight_count_rank
    FROM weight_count_tbl
    ORDER BY weight_count DESC;
    
-- inspect
-- SELECT * FROM weight_count_ranked LIMIT 10;

-- output top 3 birthdates
SELECT weight
    FROM weight_count_ranked
    WHERE weight_count_rank == 2;


-- drop tables/views
DROP TABLE batting2;
DROP TABLE master2;
DROP VIEW weight_count_tbl;
DROP VIEW weight_count_ranked;