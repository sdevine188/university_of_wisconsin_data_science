
--1) List full transaction data relating to suppliers from Madison and
--consumers from Stevens Point where transaction value is higher than
--$10,000 (show supplier, consumer and product names, quantity and price)?

SELECT T.Supp_ID, T.Con_ID, P.Name, T.Quantity, T.Price
FROM Tb_Transactions AS T INNER JOIN Tb_Product AS P
	ON T.Prod_ID = P.Prod_ID
WHERE (T.Supp_ID IN (SELECT Supp_ID
					FROM Tb_Supplier S
					WHERE City = 'Stevens Point')
	OR T.Con_ID IN (SELECT Con_ID
					FROM Tb_Consumer C
					WHERE City = 'Madison'))
	AND T.Price > 10000;


--2. Name of suppliers offering both computers and oranges? (do not use set
--operations)

SELECT Name
FROM (SELECT temp.Supp_ID, COUNT(*) AS n 
		FROM (SELECT DISTINCT O.Supp_ID, O.Prod_ID
				FROM Tb_Offers AS O
				WHERE O.Prod_ID IN (SELECT P.Prod_ID
									FROM Tb_Product P
									WHERE P.Name IN ('Computer', 'Orange'))) AS temp
	GROUP BY temp.Supp_ID) AS temp 
	INNER JOIN Tb_Supplier AS S
		ON temp.Supp_ID = S.Supp_ID 
WHERE temp.n = 2;


--3. Name of suppliers from Wausau or offering computers or offering
--oranges?

SELECT S.Name
FROM Tb_Supplier AS S
WHERE (S.Supp_ID IN (SELECT DISTINCT O.Supp_ID
					FROM Tb_Offers AS O
					WHERE O.Prod_ID IN (SELECT P.Prod_ID
										FROM Tb_Product P
										WHERE P.Name IN ('Computer', 'Orange'))))
	 OR (S.Supp_ID IN (SELECT S.Supp_ID
						FROM Tb_Supplier AS S
						WHERE S.City = 'Wausau'));


--4. Name of suppliers offering computer, auto and orange?SELECT Name
FROM (SELECT temp.Supp_ID, COUNT(*) AS n 
		FROM (SELECT DISTINCT O.Supp_ID, O.Prod_ID
				FROM Tb_Offers AS O
				WHERE O.Prod_ID IN (SELECT P.Prod_ID
									FROM Tb_Product P
									WHERE P.Name IN ('Computer', 'Orange', 'Auto'))) AS temp
	GROUP BY temp.Supp_ID) AS temp 
	INNER JOIN Tb_Supplier AS S
		ON temp.Supp_ID = S.Supp_ID
WHERE temp.n = 3;


--5. Name of products not offered in Chicago?
SELECT P.Name
FROM Tb_Product AS P
WHERE P.Prod_ID NOT IN (SELECT Prod_ID
					FROM (SELECT S.Supp_ID
						FROM Tb_Supplier AS S
						WHERE City = 'Chicago') AS temp
						INNER JOIN Tb_Offers AS O
							ON temp.Supp_ID = O.Supp_ID);


--6. Name of consumers requesting only computers?

SELECT T.Name
FROM (SELECT R.Con_ID, COUNT(*) AS n
		FROM Tb_Requests AS R
		WHERE R.Con_ID IN (SELECT R.Con_ID
							FROM Tb_Requests AS R
							WHERE R.Prod_ID = (SELECT P.Prod_ID
												FROM Tb_Product P
												WHERE P.Name = 'Computer'))
		GROUP BY R.Con_ID) AS temp 
		INNER JOIN Tb_Consumer AS T
			ON temp.Con_ID = T.Con_ID
WHERE n = 1;


--7. Name of supplier cities where none of the suppliers has any offer?

SELECT S.City
FROM Tb_Supplier AS S
WHERE S.Supp_ID NOT IN (SELECT S.Supp_ID
						FROM Tb_Supplier AS S
						WHERE S.Supp_ID IN (SELECT DISTINCT O.Supp_ID
											FROM Tb_Offers AS O));


--8. Name of products requested by all consumers?

SELECT P.Name
FROM (SELECT R.Prod_ID, COUNT(DISTINCT R.Con_ID) AS Con_ID_count
		FROM Tb_Requests AS R
		GROUP BY R.Prod_ID) AS temp 
		INNER JOIN Tb_Product AS P
			ON temp.Prod_ID = P.Prod_ID
WHERE Con_ID_count = (SELECT COUNT(DISTINCT C.Con_ID) AS Con_ID_count
						FROM Tb_Consumer AS C);


--9. Product name and supplier having the largest offer (as quantity) for that product?

SELECT S.Name, P.Name
FROM Tb_Offers AS O 
	INNER JOIN Tb_Supplier AS S
		ON O.Supp_ID = S.Supp_ID
	INNER JOIN Tb_Product AS P
		ON O.Prod_ID = P.Prod_ID
WHERE (O.Prod_ID IN (SELECT P.Prod_ID
					FROM Tb_Product P
					WHERE P.Name = 'Computer'))
	AND (O.Quantity = (SELECT MAX(O.Quantity) AS quantity_max
						FROM Tb_Offers AS O
						WHERE (O.Prod_ID IN (SELECT P.Prod_ID
											FROM Tb_Product P
											WHERE P.Name = 'Computer'))));

--10. Product name and city where that product sold best, as in largest total quantity?

SELECT P.Name, S.City
FROM (SELECT T.Supp_ID, T.Prod_ID, SUM(T.Quantity) AS quantity_sum
		FROM Tb_Transactions AS T
		WHERE T.Prod_ID IN (SELECT P.Prod_ID
							FROM Tb_Product P
							WHERE P.Name = 'Computer')
		GROUP BY T.Supp_ID, T.Prod_ID) AS temp
		INNER JOIN Tb_Supplier AS S
			ON temp.Supp_ID = S.Supp_ID
		INNER JOIN Tb_Product AS P
			ON temp.Prod_ID = P.Prod_ID
WHERE quantity_sum = (SELECT MAX(quantity_sum) AS max_quantity_sum
							FROM (SELECT T.Supp_ID, SUM(T.Quantity) AS quantity_sum
									FROM Tb_Transactions AS T
									WHERE T.Prod_ID IN (SELECT P.Prod_ID
														FROM Tb_Product P
														WHERE P.Name = 'Computer')
									GROUP BY T.Supp_ID) AS temp);


--(Extra Credit 2%) Name of products requested in all consumer cities other than Stevens Point?
	
-- get list of stevens point consumers
-- filter requests down to non-stevens point consumers 
-- join consumer_tbl to get city
-- group by product and count consumer cities
-- filter down to count_Consumer_Cities = (total_consumer_cities - 1)

SELECT *
FROM (SELECT R.Prod_ID, COUNT(C.Name) AS non_sp_consumer_city_count
		FROM Tb_Requests AS R
			INNER JOIN Tb_Consumer AS C
				ON R.Con_ID = C.Con_ID
		WHERE R.Con_ID NOT IN (SELECT C.Con_ID
								FROM Tb_Consumer AS C
								WHERE C.City = 'Stevens Point')
		GROUP BY R.Prod_ID) AS temp
WHERE consumer_city_count = ((SELECT COUNT(DISTINCT C.City) AS total_cities
								FROM Tb_Consumer AS C) - 1)


SELECT COUNT(DISTINCT C.City) AS total_cities
FROM Tb_Consumer AS C


SELECT C.Con_ID
FROM Tb_Consumer AS C
WHERE C.City = 'Stevens Point'











SELECT *
FROM Tb_Requests

SELECT *
FROM Tb_Consumer

SELECT *
FROM Tb_Product
