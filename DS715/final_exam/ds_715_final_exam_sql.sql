--final exam problem 2
--use SQL with GROUP BY, CUBE and ROLLUP to create a cube 
SELECT DISTINCT C.Name [Consumer Name],
	 C.City [Consumer City],
	 C.State [Consumer State],
	 P.Name [Product Name],
	 P.Product_Packaging [Product Packaging],
	 P.Product_Line [Product Line],
	 P.Product_Category [Product Category],
	 SUM(Quantity) [Total Transactions Quantity],
	 SUM(Quantity*Price) [Total cost],
	 MAX(Price) [Maximum Price], 
	 MIN(Price) [Minimum Price],
	 COUNT(Tran_ID) [Number of Transactions]
INTO Tb_final_exam_cube
 FROM Tb_Consumer C, Tb_Product P, Tb_Transactions T
 WHERE C.Con_ID=T.Con_ID AND
	 P.Prod_ID=T.Prod_ID
 GROUP BY CUBE((C.State, C.City, C.Name),
	(P.Product_Packaging, P.Name),
	(P.Product_Category, P.Product_Line, P.Name)),
	 ROLLUP(C.State, C.City, C.Name),
	 ROLLUP(P.Product_Packaging, P.Product_Category, P.Product_Line, P.Name);


SELECT *
FROM Tb_final_exam_cube


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------


--1. Value of products purchased by consumer and by product? 

SELECT DISTINCT fec.[Consumer Name], fec.[Product Name], fec.[Total cost]
FROM Tb_final_exam_cube AS fec
WHERE fec.[Consumer Name] IS NOT NULL
	AND fec.[Product Name] IS NOT NULL;


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------


--2. Volume of gas purchased by each consumer in Wausau? 

SELECT DISTINCT fec.[Consumer Name], fec.[Consumer City], fec.[Product Name], fec.[Total Transactions Quantity]
FROM Tb_final_exam_cube AS fec
WHERE fec.[Consumer Name] IS NOT NULL
	AND fec.[Product Name] IS NOT NULL
	AND fec.[Consumer City] = 'Wausau'
	AND fec.[Product Name] = 'Gas';


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------


--3. Find the minimum purchase price for each product sold in Wausau? 

SELECT DISTINCT fec.[Consumer City], fec.[Product Name], fec.[Minimum Price]
FROM Tb_final_exam_cube AS fec
WHERE fec.[Consumer Name] IS NULL
	AND fec.[Consumer City] IS NOT NULL
	AND fec.[Product Name] IS NOT NULL
	AND fec.[Consumer City] = 'Wausau';


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------


--4. For each consumer find the cheapest product he/she purchased? 

SELECT DISTINCT fec.[Consumer Name], fec.[Product Name], fec.[Minimum Price]
	INTO #consumer_product_min_price_tbl
FROM Tb_final_exam_cube AS fec
WHERE fec.[Consumer Name] IS NOT NULL
	AND fec.[Product Name] IS NOT NULL;

SELECT * 
FROM #consumer_product_min_price_tbl

SELECT temp.[Consumer Name], temp.[Product Name], temp.[Minimum Price] 
FROM #consumer_product_min_price_tbl AS temp
WHERE temp.[Minimum Price] = (SELECT MIN(temp2.[Minimum Price])
              FROM #consumer_product_min_price_tbl AS temp2
              WHERE temp.[Consumer Name] = temp2.[Consumer Name])

DROP #consumer_product_min_price_tbl


------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------


--5. Name of all consumers and volume of gas and milk each purchased 
--(columns: consumer name, total quantity of gas – 0 if none, total quantity of milk – 0 if none)? 

SELECT DISTINCT fec.[Consumer Name], fec.[Product Name], fec.[Total Transactions Quantity]
	INTO #consumer_gas_and_milk_quantity_tbl
FROM Tb_final_exam_cube AS fec
WHERE fec.[Consumer Name] IS NOT NULL
	AND fec.[Product Name] IS NOT NULL
	AND fec.[Product NAme] IN ('Gas', 'Milk')
	
SELECT *
FROM #consumer_gas_and_milk_quantity_tbl

SELECT [Consumer Name], [Gas], [Milk]
FROM #consumer_gas_and_milk_quantity_tbl
PIVOT ( SUM([Total Transactions Quantity])
	 FOR [Product Name] IN ([Milk], [Gas])) AS PivotTable;

DROP #consumer_gas_and_milk_quantity_tbl


