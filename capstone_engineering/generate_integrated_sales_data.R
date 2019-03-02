# Load required dependencies  --------------------------------------------------
source("data_prep_utils.R")  # functions for data prep and feature engineering
library(readr)  # for data manipulation

# Define Inputs  ---------------------------------------------------------------

start_date <- as.Date("2010-01-01")  # order time series start date
end_date <- as.Date("2018-12-31")  # order time series end date

# path for raw Scholle IPN bag order data
raw_orders_path <- 
  "data/raw/Sales_data_for_Capstone_Project_-_Student_Version_-_v3.xlsx"

# path to save clean Scholle IPN bag order data
clean_orders_path <- "data/clean/tomato_bag_orders.rds"

# path to save monthly orders dataset
monthly_orders_path <- "data/pipeline/monthly_bag_orders.rds"

# path to the folder with raw unemployment and labor force participation rt data
raw_unemployment_path <- "data/raw/unemployment"

# path to save clean unemployment data
clean_unemployment_path <- 
  "data/pipeline/us_unemployment_and_labor_force_participation.rds"

# path to the folder with raw OECD index data
raw_oecd_path <- "data/raw/oecd"

# path to save clean OECD index data
clean_oecd_path <- "data/pipeline/us_oecd_cci_bci.rds"

# path to raw federal funds rate data
raw_fed_fund_path <- "data/raw/FEDFUNDS.csv"

# path to save federal funds rate data
clean_fed_fund_path <- "data/pipeline/federal_funds_rate.rds"

# path to raw us city avg field grown tomato price data
raw_tomato_price_path <- "data/raw/tomato_cpi"

# path to save us city avg field grown tomato price data
clean_tomato_price_path <- "data/pipeline/us_city_field_grown_tomato_price.rds"

# path to raw housing price index data
raw_housing_price_index_path <- "data/raw/HPI_master.csv"

# path to raw Invesco DB Agriculture ETF Pricing data
raw_dba_path <- "data/raw/DBA.csv"

# Prepare the Sholle IPN Orders Dataset  ---------------------------------------

# read the raw order data
orders_dat <- read_raw_orders(raw_orders_path)

# clean and prepare the orders data
orders_dat <- clean_orders(
  orders_dat, 
  start_date, end_date, 
  peak_period_only = FALSE
)

# save the clean orders dataset
write_rds(orders_dat, clean_orders_path)

# prepare the Scholle IPN monthly order total dataset
monthly_orders_dat <- prepare_monthly_orders(orders_dat, by_item = FALSE)

# save the monthly orders dataset
write_rds(monthly_orders_dat, monthly_orders_path)

# Prepare the national unemployment & labor force participation rate data  -----

# read the raw data (as downloaded in text file instructions)
employment_dat <- read_raw_employment(raw_unemployment_path)

# clean and prepare the data
employment_dat <- clean_employment(employment_dat)

# save the clean unemployment and labor force participation data
write_rds(employment_dat, clean_unemployment_path)

# Prepare the national consumer and business confidence index data  ------------

# read the raw OECD indices data
oecd_dat <- read_raw_oecd_indices(raw_oecd_path)

# clean the oecd indicator data
oecd_dat <- clean_oecd_indices(oecd_dat)

# save the monthly CCI and BCI data
write_rds(oecd_dat, clean_oecd_path)

# Prepare the national federal funds rate data  --------------------------------

# read the raw data
fed_fund_rate_dat <- read_raw_federal_funds_rate(raw_fed_fund_path)

# clean the data
fed_fund_rate_dat <- clean_federal_funds_rate(fed_fund_rate_dat)

# save the federal fund rate data
write_rds(fed_fund_rate_dat, clean_fed_fund_path)

# Prepare the field grown tomato price data  -----------------------------------

# read the raw average field grown tomato price for US cities 
city_field_grown_tomato_price_dat <- 
  read_raw_tomato_price(raw_tomato_price_path)

# clean the data
city_field_grown_tomato_price_dat <- 
  clean_tomato_prices(city_field_grown_tomato_price_dat)

write_rds(city_field_grown_tomato_price_dat, clean_tomato_price_path)

# Prepare the housing price index data  ----------------------------------------

# read the raw housing price index data
housing_price_index_dat <- 
  read_housing_price_index(raw_housing_price_index_path)

# clean and prepare the data
housing_price_index_dat <- clean_housing_price_index(housing_price_index_dat)

# Prepare the Invesco DBA data  ------------------------------------------------
invesco_dba_dat <- read_dba(raw_dba_path)

# clean and prepare the data
invesco_dba_dat <- clean_dba(invesco_dba_dat)

# Prepare the final integrated monthly analysis datasets  ----------------------

# join the monthly orders with the employment data
monthly_analysis_dat <- prepare_monthly_analysis(
  monthly_orders_dat, employment_dat, oecd_dat, fed_fund_rate_dat, 
  city_field_grown_tomato_price_dat, housing_price_index_dat, invesco_dba_dat
)






# --------------OLD-----------------------

# # Read the Raw Sales Order Data from the client file
# sales_dat <- read_raw_sales()
# 
# # Filter the date range  ----
# sales_dat <- sales_dat %>% filter(Planned_Delivery_Date >= '2010-01-01')
# 
# # Filter to only include tomato bag sales
# sales_dat <- sales_dat %>% 
#   filter(Line_of_Business == "TOMATO (PROCESSED FRUIT TOMATO)")
# 
# # Clean Up City/State  ----
# 
# # Read the state abbreviations data
# state_abbreviations_dat <- read_state_abbreviations()
# 
# # Clean the city/state columns
# clean_geography <- mapply(
#   clean_city_state, 
#   sales_dat$Ship_to_City, 
#   sales_dat$Ship_to_State
# )
# 
# # Update the sales data
# sales_dat <- sales_dat %>%
#   mutate(
#     Ship_to_State = unname(unlist(clean_geography[2, row_number()])),
#     Ship_to_City = unname(unlist(clean_geography[1, row_number()]))#,
#     #domestic_or_international = unname(unlist(clean_geography[3, row_number()]))
#   )
# #rm(clean_geography, state_abbreviations_dat)
# 
# # Add Informative Date Columns ----
# sales_dat <- sales_dat %>%
#   mutate(
#     planned_delivery_year      = as.integer(year(Planned_Delivery_Date)),
#     planned_delivery_month     = month(Planned_Delivery_Date, label = TRUE),
#     planned_delivery_month_num = as.integer(month(Planned_Delivery_Date)),
#     planned_delivery_day       = as.integer(mday(Planned_Delivery_Date))
#   )
# 
# # Final Filtering  ----
# 
# # Write the Clean Sales Data to Disk  ----
# write_clean_sales_data(sales_dat)
# #rm(sales_dat)
