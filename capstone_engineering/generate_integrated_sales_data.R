# Generate the sales order table with full integration
# TODO: This should actually be written as a function and called here


# Load required dependencies
library(tidyverse)
library(lubridate)
source("data_prep_utils.R")

# Read the Raw Sales Order Data from the client file
sales_dat <- read_raw_sales()

# Clean Up City/State  ----

# Read the state abbreviations data
state_abbreviations_dat <- read_state_abbreviations()

# Clean the city/state columns
clean_geography <- mapply(
  clean_city_state, 
  sales_dat$Ship_to_City, 
  sales_dat$Ship_to_State
)

# Update the sales data
sales_dat <- sales_dat %>%
  mutate(
    Ship_to_State = unname(unlist(clean_geography[2, row_number()])),
    Ship_to_City = unname(unlist(clean_geography[1, row_number()])),
    domestic_or_international = unname(unlist(clean_geography[3, row_number()]))
  )
rm(clean_geography, state_abbreviations_dat)

# Add Informative Date Columns ----
sales_dat <- sales_dat %>%
  mutate(
    planned_delivery_year      = as.integer(year(Planned_Delivery_Date)),
    planned_delivery_month     = month(Planned_Delivery_Date, label = TRUE),
    planned_delivery_month_num = as.integer(month(Planned_Delivery_Date)),
    planned_delivery_day       = as.integer(mday(Planned_Delivery_Date))
  )

# Final Filtering  ----

# Filter to only include tomato bag sales
sales_dat <- sales_dat %>% 
  filter(Line_of_Business == "TOMATO (PROCESSED FRUIT TOMATO)")

# Write the Clean Sales Data to Disk  ----
write_clean_sales_data(sales_dat)
rm(sales_dat)
