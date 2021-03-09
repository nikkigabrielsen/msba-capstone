INSERT INTO master	
	SELECT date,
	time,
	ticket_num,
	emp_num,
	trans_type,
	cust,
	transactions.sku,
	sku_2021.Vendor as vendor,
	sku_2021.VendorDescription as vendor_desc,
	sku_2021.Category as category,
	sku_2021.CategoryDescription as category_desc,
	sku_2021.Description as item_desc,
	sku_2021.SizeType as size_type,
	size,
	sku_2021.StyleColor as style_color,
	quan_sold,
	quan_returned,
	net_sale
	FROM transactions
	JOIN sku_2021 
	ON sku_2021.sku = transactions.sku 