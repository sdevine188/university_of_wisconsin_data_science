
--1) Product categories and number of products in each category?

SELECT P.Product_Category, COUNT(DISTINCT P.Prod_ID) AS p_prod_id_count
FROM Tb_Product AS P
GROUP BY P.Product_Category;


--2) Cities having at least 3 different consumers?

SELECT C.City, COUNT(DISTINCT C.Name) AS c_name_count
FROM Tb_Consumer AS C
GROUP BY C.City
HAVING COUNT(C.Name) >= 3;


--3) Cities and number of different products offered in city?
SELECT City, SUM(o_prod_id_count) AS temp_prod_id_sum
FROM (SELECT O.Supp_ID, COUNT(DISTINCT O.Prod_ID) AS o_prod_id_count
		FROM Tb_Offers AS O
		GROUP BY O.Supp_ID) AS temp
	INNER JOIN Tb_Supplier AS S
		ON temp.Supp_ID = S.Supp_ID
GROUP BY City;


--4) List of states and number of car suppliers in each state?
SELECT S.City, COUNT(DISTINCT S.Supp_ID) AS s_supp_id_count
FROM Tb_Supplier AS S
GROUP BY S.City;


--5) Product name and quantity offered in each city?

-- get product_offered_quantity_by_city
SELECT S.Supp_ID, S.City, P.Prod_ID, P.Name AS p_name, O.Quantity INTO #product_offered_quantity_by_city
FROM Tb_Supplier AS S
	INNER JOIN Tb_Offers AS O
		ON S.Supp_ID = O.Supp_ID
	INNER JOIN Tb_Product AS P
		ON O.Prod_ID = P.Prod_ID;

-- get quantity_offered_sum grouped by city and product
SELECT temp.City, temp.p_name, SUM(temp.Quantity) AS quantity_offered_sum
FROM #product_offered_quantity_by_city AS temp
GROUP BY temp.City, temp.p_name;

DROP TABLE #product_offered_quantity_by_city;


--6) Supplier name and quantity of TV sold in each consumer city?

-- get product_offered_quantity_by_supplier_and_city
SELECT S.Supp_ID, S.Name AS s_name, S.City, P.Prod_ID, P.Name AS p_name, O.Quantity 
	INTO #product_offered_quantity_by_supplier_and_city
FROM Tb_Supplier AS S
	INNER JOIN Tb_Offers AS O
		ON S.Supp_ID = O.Supp_ID
	INNER JOIN Tb_Product AS P
		ON O.Prod_ID = P.Prod_ID;

-- get quantity_offered_sum for TVs grouped by city and supplier
SELECT temp.City, temp.s_name, temp.p_name, SUM(temp.Quantity) AS quantity_offered_sum
FROM #product_offered_quantity_by_supplier_and_city AS temp
GROUP BY temp.City, temp.s_name, temp.p_name
HAVING temp.p_name = 'TV';

DROP TABLE #product_offered_quantity_by_supplier_and_city;


--7) Supplier-consumer-product names such that supplier is selling product to
--consumer with total value of transactions between supplier and consumer
--for given product?
SELECT S.s_name, C.c_name, P.p_name, D.DateYear, D.DateMonth, T.TimeKey, T.Price, T.Quantity, T.Price*T.Quantity AS value
	INTO #supplier_consumer_product_transactions
FROM Tb_Transactions AS T
	INNER JOIN (SELECT S.Supp_ID, S.Name AS s_name 
				FROM Tb_Supplier AS S) AS S
		ON T.Supp_ID = S.Supp_ID
	INNER JOIN (SELECT C.Con_ID, C.Name AS c_name
				FROM Tb_Consumer AS C) AS C
		ON T.Con_ID = C.Con_ID
	INNER JOIN (SELECT P.Prod_ID, P.Name AS p_name
				FROM Tb_Product AS P) AS P
		ON T.Prod_ID = P.Prod_ID
	INNER JOIN (SELECT D.DateKey, D.DateYear, D.DateMonth
				FROM Tb_Date AS D) AS D
		ON T.DateKey = D.DateKey;

SELECT temp.s_name, temp.c_name, temp.p_name, SUM(value) AS total_transactions_value
FROM #supplier_consumer_product_transactions AS temp
GROUP BY temp.s_name, temp.c_name, temp.p_name;

DROP TABLE #supplier_consumer_product_transactions;


--8) Monthly sales data (total transactions quantity, number of transactions,
--total transactions value) by supplier, consumer, and product during the
--year 2018?
--for given product?
SELECT S.s_name, C.c_name, P.p_name, D.DateYear, D.DateMonth, T.TimeKey, T.Price, T.Quantity, T.Price*T.Quantity AS value
	INTO #supplier_consumer_product_transactions
FROM Tb_Transactions AS T
	INNER JOIN (SELECT S.Supp_ID, S.Name AS s_name 
				FROM Tb_Supplier AS S) AS S
		ON T.Supp_ID = S.Supp_ID
	INNER JOIN (SELECT C.Con_ID, C.Name AS c_name
				FROM Tb_Consumer AS C) AS C
		ON T.Con_ID = C.Con_ID
	INNER JOIN (SELECT P.Prod_ID, P.Name AS p_name
				FROM Tb_Product AS P) AS P
		ON T.Prod_ID = P.Prod_ID
	INNER JOIN (SELECT D.DateKey, D.DateYear, D.DateMonth
				FROM Tb_Date AS D) AS D
		ON T.DateKey = D.DateKey;

SELECT temp.s_name, temp.c_name, temp.p_name, temp.DateYear, temp.DateMonth,
	COUNT(*) AS monthly_transactions_count,
	SUM(temp.Quantity) AS monthly_transactions_quantity_sum,
	SUM(temp.value) AS monthly_transactions_value_sum
FROM #supplier_consumer_product_transactions AS temp
GROUP BY temp.s_name, temp.c_name, temp.p_name, temp.DateYear, temp.DateMonth
	HAVING temp.DateYear = 2018;

DROP TABLE #supplier_consumer_product_transactions;


--9) States where the number of suppliers exceeds the number of consumers?
SELECT *
FROM (SELECT C.State AS c_state, COUNT(*) AS consumer_count
		FROM Tb_Consumer AS C
		GROUP BY C.State) AS C
	INNER JOIN (SELECT S.State AS s_state, COUNT(*) AS supplier_count
				FROM Tb_Supplier AS S
				GROUP BY S.State) AS S
		ON C.c_state = S.s_state
WHERE S.supplier_count > C.consumer_count;


--10) Comparative list of supplier or consumer states and cities with
--respective number of suppliers and consumers in each city (columns are
--State, City, NumberOfSuppliers, NumberOfConsumers)?
SELECT *, ISNULL(C.consumer_count, 0) AS consumer_count_final, 
	ISNULL(S.supplier_count, 0) AS supplier_count_final 
INTO #supplier_and_consumer_count_by_state_city
FROM (SELECT C.State AS c_state, C.City AS c_city, COUNT(*) AS consumer_count
		FROM Tb_Consumer AS C
		GROUP BY C.State, C.City) AS C
	FULL OUTER JOIN (SELECT S.State AS s_state, S.City AS s_city, COUNT(*) AS supplier_count
				FROM Tb_Supplier AS S
				GROUP BY S.State, S.City) AS S
		ON C.c_state = S.s_state
			AND C.c_city = S.s_city;

ALTER TABLE #supplier_and_consumer_count_by_state_city
	DROP COLUMN consumer_count, supplier_count;

SELECT *
FROM #supplier_and_consumer_count_by_state_city
WHERE supplier_count_final > consumer_count_final;

DROP TABLE #supplier_and_consumer_count_by_state_city;















