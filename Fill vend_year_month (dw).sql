INSERT INTO vend_year_month	
	SELECT Vendor,
	VendorDescription,
	strftime('%Y', transactions.date) as Year, 
	strftime('%m', transactions.date) as Month, 	
	sum(transactions.quan_sold) as Quantity,
	round(sum(transactions.net_sale),2) as Sales 
	FROM sku_2021
	INNER JOIN transactions 
	ON transactions.sku = sku_2021.sku
	GROUP BY Vendor, Year, Month