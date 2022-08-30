-- get Tn_Transactions_Cube
	 S.City [Supplier City],
	 S.State [Supplier State],
	 C.Name [Consumer Name],
	 C.City [Consumer City],
	 C.State [Consumer State],
	 P.Name [Product Name],
	 SUM(Quantity) [Total Transactions Quantity],
	 COUNT(Tran_ID) [Number of Transactions]
INTO Tb_Transactions_Cube
 FROM Tb_Supplier S, Tb_Consumer C, Tb_Product P, Tb_Transactions T
 WHERE S.Supp_ID=T.Supp_ID AND
	 C.Con_ID=T.Con_ID AND
	 P.Prod_ID=T.Prod_ID
 GROUP BY CUBE((S.State, S.City, S.Name),
	(C.State, C.City, C.Name),
	P.Name),
	 ROLLUP(S.State, S.City, S.Name),
	 ROLLUP(C.State, C.City, C.Name);
--product?
--consumer in Illinois?
--consumer in Illinois?
--Chicago?
--consumers in Chicago?
--Madison to consumers in Chicago versus quantity sold by suppliers in
--Chicago to consumers in Madison (result columns will be: product name,
--quantity Madison_Chicago, quantity Chicago_Madison)?