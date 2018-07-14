#Task 1: 

# A. This superstore database contains the information like Customers details, Product details, Order details, shipping details. 
# Five tables are used to capture all the information, and tables are dependent on other tables. This database is relational database 
# as relations exist in tables. Tables whihch are used in this database are :
# cust_dimen(contains the informaton related to individual cutomer like Customer Name, Province, Customer ID etc), 
# prod_dimen(contains the information related to individual product like Product category, product sub category and prodcut id),
# market_fact(contains the information related to individual order like order id, prodcut id, customer id, profit, order quantity, shiping information etc),
# order_dimen(contains the information related to individual order like when the order was placed, priority of order, order id etc), 
# shipping_dimen(contains the information rerlated to shipping of products like order id, mode of shipping, shipping date, shipping id etc).

# B. Primary and Foreign keys in individual tables:
# cust_dimen : Primary key - Cust_id, no foreign key
# prod_dimen : Primary key - Prod_id, no foreign key
# orders_dimen : Primary Key - Ord_id, Foreign key are same - Ord_id, Order_ID
# market_fact : Primary key - No Primary key, Foreign key - Ord_id, Prod_id, Ship_id, Cust_id
# shipping_dimen : Primary Key - Ship_id, Foreign key - Order_ID

#Task 2 : Basic Analysis
use superstoresdb;

# A. Finding the total and average sales. 

select sum(Sales) as total_sales, avg(Sales) as avg_sales from market_fact;

# B. Displaying the number of customers in each region in decreasing order of number of customers. 

select Region, count(*) as no_of_customers from cust_dimen 
group by Region 
order by no_of_customers desc;

# C. Finding the Region which have maximum customers

select Region, count(*) as max_no_of_customers from cust_dimen 
group by Region 
order by max_no_of_customers desc limit 1;

# D. Finding the product_id and number of products sold in decreasing order of number of prodcuts sold

select Prod_id, count(*) as no_of_products from market_fact 
group by Prod_id 
order by no_of_products desc;

# E. Finding the customers from Atlantic region who have ever purchased 'TABLES' and the number of tables purchased
# Joining market_fact, prod_dimen and cust_dimen to get the required results

select c.Customer_Name as customer_name, m.Order_Quantity as no_of_tables 
from market_fact m inner join prod_dimen p on m.Prod_id = p.Prod_id 
inner join cust_dimen c on m.Cust_id = c.Cust_id 
where p.Product_Sub_Category = 'TABLES' and c.Region = 'ATLANTIC';


#Task 2 : Advanced Analysis

# A. Displayinng the product categories in descending order of profits
# Joining prod_dimen with market_fact table to get the desired results

select p.Product_Category as product_category, sum(m.profit) as profit 
from prod_dimen p inner join market_fact m on p.Prod_id = m.Prod_id 
group by p.Product_Category 
order by profit desc;


# B. Displaying the product categories, product sub-categories and the profit within each subcategory
# Joining prod_dimen with market_fact table to get the desired results

select (select Product_Category from prod_dimen where Product_Sub_Category = p.Product_Sub_Category) as product_category, 
p.Product_Sub_Category as product_sub_category, sum(m.profit) as profit 
from prod_dimen p inner join market_fact m on p.Prod_id = m.Prod_id 
group by p.Product_Sub_Category;


# C. For least profitable product subcategory, displaying the region wise number of shipments, profit made in decreasing order. 

# We are joining market_fact, prod_dimen and cust_dimen to get the desired output. 

select c.Region as Region, count(*) as no_of_shipments, sum(m.Profit) as profit_in_each_region 
from market_fact m inner join cust_dimen c on m.Cust_id = c.Cust_id 
inner join prod_dimen p on m.Prod_id = p.Prod_id where p.Product_Sub_Category = (
select p.Product_Sub_Category as product_sub_category
from prod_dimen p inner join market_fact m on p.Prod_id = m.Prod_id 
group by p.Product_Sub_Category 
order by sum(m.profit) asc limit 1)
group by Region 
order by profit_in_each_region desc;
