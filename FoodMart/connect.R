library(RMySQL)
library(ggplot2)
#Connecting to Foodmart
foodmart <- dbConnect(MySQL(),user="root",host="localhost",password="",db="foodmart")
tables <- dbListTables(foodmart)

#Read Required Tables for Analysis
customer = dbReadTable(foodmart,"customer")
sales_fact_1998 <- dbReadTable(foodmart,"sales_fact_1998")
sales_fact_dec_1998 <- dbReadTable(foodmart,"sales_fact_dec_1998")
sales_fact_1997 <- dbReadTable(foodmart,"sales_fact_1997")
product <- dbReadTable(foodmart,"product")
time_by_day <- dbReadTable(foodmart,"time_by_day")

# Date Conversions
library(lubridate)
time_by_day$the_date <- as.Date(time_by_day$the_date)
customer$date_accnt_opened <- as.Date(customer$date_accnt_opened)
customer$birthdate <- as.Date(customer$birthdate)


# Add 16 years to foodmart data
year(time_by_day$the_date) <- year(time_by_day$the_date) + 16
year(customer$date_accnt_opened) <- year(customer$date_accnt_opened) + 16
year(customer$birthdate) <- year(customer$birthdate) + 16

#customer_age
customer$age <- year(now()) - year(customer$birthdate)-1
customer$accnt_opened_days <- as.Date(now()) - customer$date_accnt_opened

#factorize

time_by_day$the_day = factor(time_by_day$the_day)
time_by_day$the_month = factor(time_by_day$the_month)
time_by_day$the_year = factor(time_by_day$the_year)
time_by_day$day_of_month = factor(time_by_day$day_of_month)
time_by_day$week_of_year = factor(time_by_day$week_of_year)
time_by_day$month_of_year = factor(time_by_day$month_of_year)
time_by_day$quarter = factor(time_by_day$quarter)
time_by_day$fiscal_period = factor(time_by_day$fiscal_period)
customer$account_num = factor(customer$account_num)
customer$city = factor(customer$city)
customer$state_province = factor(customer$state_province)
customer$postal_code = factor(customer$postal_code)
customer$country = factor(customer$country)
customer$customer_region_id = factor(customer$customer_region_id)
customer$marital_status = factor(customer$marital_status)
customer$yearly_income = factor(customer$yearly_income)
customer$gender = factor(customer$gender)
customer$total_children = factor(customer$total_children)
customer$num_children_at_home = factor(customer$num_children_at_home)
customer$education = factor(customer$education)
customer$member_card = factor(customer$member_card)
customer$occupation = factor(customer$occupation)
customer$houseowner = factor(customer$houseowner)
customer$num_cars_owned = factor(customer$num_cars_owned)
customer$customer_id = factor(customer$customer_id)
customer$age = factor(customer$age)
#Binding Data

sales <- rbind(sales_fact_1997,sales_fact_1998)

sales <- merge(sales,time_by_day, by.x = "time_id", by.y = "time_id")
sales <- merge(sales,customer, by.x = "customer_id", by.y = "customer_id")
sales <- merge(sales,product, by.x = "product_id", by.y = "product_id")

#Factorize Sales
sales$customer_id = factor(sales$customer_id)
sales$product_id = factor(sales$product_id)
sales$promotion_id = factor(sales$promotion_id)
sales$store_id = factor(sales$store_id)
#Generate Order_Id
sales$order_id <- seq(rnorm(length(sales$the_date)))

today <- max(sales$the_date)
R_table <- aggregate(the_date ~ customer_id, sales, FUN=max)
R_table$R <- as.numeric(today - R_table$the_date)
F_table <- aggregate(order_id ~ customer_id, sales, FUN=length)
M_table <- aggregate(store_sales ~ customer_id, sales, FUN=sum)
RFM_table <- merge(R_table,F_table,by.x="customer_id", by.y="customer_id") # Merge R with F
RFM_table <- merge(RFM_table,M_table, by.x="customer_id", by.y="customer_id") # Merge M into RF
RFM_table$the_date <- NULL
names(RFM_table) <- c("customer_id", "R", "F", "M")
customer_RFM <- merge(RFM_table,customer, by.x="customer_id", by.y="customer_id")
results <- kmeans(customer_RFM[,2:4],5)
customer_RFM$cluster <- results$cluster