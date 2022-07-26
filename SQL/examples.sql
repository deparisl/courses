/* examples from classes */

SELECT vendor_name, CONCAT(vendor_city, ',', vendor_state, ' ', vendor_zip_code) AS adress
FROM vendors
ORDER BY vendor_name;

SELECT invoice_number, vendor_name
FROM vendors INNER JOIN invoices
	ON vendors.vendor_id = invoices.vendor_id;

SELECT invoice_number, vendor_name, invoice_due_date, invoice_total - payment_total - credit_total AS balance_due
FROM vendors v JOIN invoices i
	ON v.vendor_id = i.vendor_id
WHERE invoice_total - payment_total - credit_total > 0 
ORDER BY invoice_due_date DESC; 

SELECT vendor_name, invoice_number, invoice_date, line_item_amount, account_description 
FROM vendors v
	JOIN invoices i
		ON v.vendor_id = i.vendor_id
	JOIN invoice_line_items li
		ON i.invoice_id = li.invoice_id
	JOIN general_ledger_accounts gl
		ON li.account_number = gl.account_number
WHERE invoice_total - payment_total - credit_total > 0
ORDER BY vendor_name, line_item_amount DESC;

