INSERT INTO vend_year_month
	SELECT Vendor,
	strftime('%Y', transactions.date) as Year, 
	strftime('%m', transactions.date) as Month, 
	sum(transactions.net_sale) as Sales, 
	sum(transactions.quan_sold) as Quantity	
	FROM sku_2021
	INNER JOIN transactions 
	ON transactions.sku = sku_2021.sku
	GROUP BY Vendor, Year, Month