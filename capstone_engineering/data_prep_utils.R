# This file contains functions for preparing data.

# Load required libraries  ----
library(tidyverse)  # tidyverse, for data manipulation
library(readxl)  # for loading excel files
library(stringr)  # for managing strings
library(janitor)  # for clenaing data
library(lubridate)  # for manipulating dates
library(reshape2)  # for reshaping data

# Define feature engineering and data cleanup functions  #######################

# Define a function for parsing out city state and international status
# - For the "return_coL" parameter, pass either "state" or "is_domestic". The 
#   former will return a character vector of length 1, and the latter will 
#   return a boolean value.
# - Note that the domestic indicator will return TRUE when a state can be 
#   succesfully determined. All other scenarios will return FALSE
# - Determination of state and domestic status is based entirely on matching
#   either the full state name or 2 character abbreviation. For example, "Miami"
#   will be improperly classified. 
extract_state <- function(raw_city, raw_state, return_col = "state") {
  
  # define a string to pattern match two character state abbreviations
  state_abb_pattern <- paste0(
    "^.+\\s+(AL|AK|AZ|AR|CA|CO|CT|DE|FL|GA|HI|ID|IL|IN|IA|KS|KY|LA|ME|MD|MA|MI",
    "|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|OH|OK|OR|PA|RI|SC|SD|TN|TX|UT|VT|VA|",
    "WA|WV|WI|WY).*$"
  )
  
  state_abb_pattern_lower <- paste0(
    "^.+\\s+(al|ak|az|ar|ca|co|ct|de|fl|ga|hi|id|il|in|ia|ks|ky|la|me|md|ma|mi",
    "|mn|ms|mo|mt|ne|nv|nh|nj|nm|ny|nc|nd|oh|ok|or|pa|ri|sc|sd|tn|tx|ut|vt|va|",
    "wa|wv|wi|wy).*$"
  )
  
  # define a string to pattern match full state names (lowercase)
  full_state_pattern <- paste0(
    "^.*\\s+(alabama|alaska|arizona|arkansas|california|colorado|connecticut|d",
    "elaware|florida|georgia|hawaii|idaho|illinois|indiana|iowa|kansas|kentuck",
    "y|louisiana|maine|maryland|massachusetts|michigan|minnesota|mississippi|m",
    "issouri|montana|nebraska|nevada|new hampshire|new jersey|new mexico|new y",
    "ork|north carolina|north dakota|ohio|oklahoma|oregon|pennsylvania|rhode i",
    "sland|south carolina|south dakota|tennessee|texas|utah|vermont|virginia|w",
    "ashington|west virginia|wisconsin|wyoming).*$"
  )
  
  states_dat <- cbind.data.frame(
    state.abb, 
    state.name, 
    stringsAsFactors = FALSE
  )
  
  # convert the raw_city variable into lowercase
  raw_city <- tolower(raw_city)
  
  # implement logic to capture state
  clean_state <- ifelse(
    raw_state %in% state.abb, raw_state, ifelse(
      str_detect(raw_city, state_abb_pattern_lower),
      toupper(str_match(raw_city, state_abb_pattern_lower)[1, 2]),
      ifelse(
        str_detect(raw_city, full_state_pattern),
        states_dat[tolower(states_dat[, 2]) == 
                     str_match(raw_city, full_state_pattern)[1, 2], 1],
        NA
      )
    )
  )
  
  # based on the return_col parameter, return state
  if (return_col == "state") return(as.character(clean_state)) 
  
  # code whether the state is captured, and use to determine if the row is 
  # domestic
  is_domestic <- !is.na(clean_state)
  
  # return whether or not the row is domestic
  if (return_col == "is_domestic") return(is_domestic)
}

# Define a vectorized version of the extract_state_function
extract_state_v <- Vectorize(extract_state)

# Define functions for reading data into R  ####################################

# Define a function for reading the raw order data
read_raw_orders <- 
  function(path = paste0("data/raw/Sales_data_for_",
                         "Capstone_Project_-_Student_Version_-_v3.xlsx")) {
    
   # define the schema for the orders data
   schema = c("text", "text", "numeric", "text", "text", "text", "text", 
               "text", "text", "text", "numeric", "date", "text", "text", 
               "text", "numeric", "numeric", "text")
    
    # read the excel file
    dat <- read_excel(
      path, 
      sheet = "Processed Fruits", 
      col_names = TRUE, 
      col_types = schema, 
      na = c("", "NULL", "  ", "N/A"))
    
    dat
  }

# Define a functoin for reading the raw unemployment and labor force
# participation rate files
read_raw_employment <- function(path = "data/raw/unemployment") {
  
  # define a vector of filenames in the path & append path
  filenames <- paste(path, dir(path, pattern = "*.xlsx"), sep = "/")
  
  # combine the files into one dataframe
  dat <- filenames %>% 
    map(read_excel, sheet = "BLS Data Series", skip = 11) %>% 
    reduce(rbind)
  
  dat
}

# Define a function for reading the raw consumer and business confidence indexes
read_raw_oecd_indices <- function(path = "data/raw/oecd") {
  
  # define a vector of filenames in the path and append path
  filenames <- paste(path, dir(path, pattern = "*.csv"), sep = "/")
  
  # combine the files into one dataframe
  dat <- filenames %>% map(read_csv) %>% reduce(rbind)
  
  dat
}

# Define a function for reading the raw federal funds rate data
read_raw_federal_funds_rate <- function(path = "data/raw/FEDFUNDS.csv") {
  
  # read the raw data
  dat <- read_csv(path)
  
  dat
}

# Define a function for reading average raw us city field grown tomato price
read_raw_tomato_price <- function(path = "data/raw/tomato_cpi") {
  
  # define a vector of filenames in the path and append path
  filenames <- paste(path, dir(path, pattern = "*.xlsx"), sep = "/")
  
  # combine the files into one dataframe (if applicable)
  dat <- filenames %>% 
    map(read_excel, sheet = "BLS Data Series", skip = 9) %>%
    reduce(rbind)
  
  dat
}

# Define functions for preparing clean datasets  ###############################

# Define a function for cleaning and preparing the Scholle IPN order data
clean_orders <- function(
  data, start, end,
  peak_period_only  = FALSE,
  domestic_only     = TRUE, 
  tomato_only       = TRUE,
  positive_qty_only = TRUE
) {
  
  dat <- data  # handle easier input notation then use standard notation
  
  # clean the column names to snake case
  dat <- dat %>% clean_names()
  
  # clean the ship to state column and whether or not the order is in the US
  dat <- dat %>% mutate(
    ship_to_state = extract_state_v(ship_to_city, ship_to_state, "state"),
    is_domestic   = extract_state_v(ship_to_city, ship_to_state, "is_domestic")
  )
  
  # add planned_delivery_date component columns
  dat <- dat %>% mutate(
    planned_delivery_year             = year(planned_delivery_date),
    planned_delivery_month_abbr       = month(planned_delivery_date,
                                              label = TRUE, abbr = TRUE),
    planned_delivery_month_num        = month(planned_delivery_date, 
                                              label = FALSE), 
    planned_delivery_day_of_week_abbr = wday(planned_delivery_date, 
                                             label = TRUE, abbr = TRUE),
    planned_delivery_day_of_week_num  = wday(planned_delivery_date, 
                                             label = FALSE)
  )
  
  # filter the data on the date range
  dat <- dat %>% filter(
    planned_delivery_date >= start & planned_delivery_date <= end
  )
  
  # filter the data on the peak_period_only argument (Jun - Oct)
  if (peak_period_only) dat <- dat %>% filter(
    planned_delivery_month_num >= 6 & planned_delivery_month_num <= 10
  )
  
  # filter the data on the domestic_only argument
  if (domestic_only) dat <- dat %>% filter(is_domestic)
  
  # filter the data on the tomato_only argument
  if (tomato_only) dat <- dat %>% 
    filter(line_of_business == "TOMATO (PROCESSED FRUIT TOMATO)")
  
  if (positive_qty_only) dat <- dat %>% filter(quantity > 0)
  
  dat
}

# Define a function for aggregating orders by month
prepare_monthly_orders <- function(data, by_item = FALSE) {
  
  dat <- data
  
  if (by_item) dat <- dat %>%
      group_by(
        planned_delivery_year, 
        planned_delivery_month_abbr, planned_delivery_month_num,
        item_number, item_description
      ) %>%
      summarise(order_quantity = sum(quantity))
  
  if (by_item == FALSE) dat <- dat %>%
      group_by(
        planned_delivery_year, 
        planned_delivery_month_abbr, planned_delivery_month_num
      ) %>%
      summarise(order_quantity = sum(quantity))
}

# Define a function for cleaning the unemployment & labor force participation
# data
clean_employment <- function(data) {
  
  dat <- data
  
  # define a dataframe of BLS series ID info
  bls_series_id_dat <- data.frame(
    series_id = c("LNS11300000", "LNU01300000", "LNS14000000", "LNU04000000"),
    measure   = c(
      "us_labor_force_participation_seas_adj", "us_labor_force_participation",
      "us_unemployment_seas_adj", "us_unemployment"
    )
  )
  
  # clean column names to snake case
  dat <- dat %>% clean_names()
  
  # clean the month names
  dat <- dat %>% mutate(month = as.integer(substr(period, 2, 3)))
  
  # include the descriptive metric names
  dat <- dat %>% inner_join(bls_series_id_dat, by = "series_id")
  
  # reshape the data to wide format
  dat <- dat %>% dcast(year + month ~ measure, value.var = "value")
  
  dat
}

# Define a function for cleaning the OECD index data
clean_oecd_indices <- function(data) {
  dat <- data
  
  # snake case column names
  dat <- dat %>% clean_names()
  
  # filter to only include the US
  dat <- dat %>% filter(location == "USA")
  
  # create month and year columns
  dat <- dat %>% mutate(
    year  = as.integer(substr(time, 1, 4)),
    month = as.integer(substr(time, 6, 7))
  )
  
  # clean up the indicator names
  dat <- dat %>% mutate(indicator = case_when(
    indicator == "CCI" ~ "us_consumer_confidence_index",
    indicator == "BCI" ~ "us_business_confidence_index"
  ))
  
  # reshape the data to wide
  dat <- dat %>% dcast(year + month ~ indicator, value.var = "value")
  
  dat
}

# Define a function for cleaning hte US federal funds rate data
clean_federal_funds_rate <- function(data) {
  dat <- data
  
  # snake case the column names
  dat <- dat %>% clean_names()
  
  # define columns for month and year
  dat <- dat %>% mutate(
    year  = year(date),
    month = month(date, label = FALSE)
  )
  
  # drop the date column
  dat <- dat %>% select(-date)
  
  # rename the federal funds rate column
  dat <- dat %>% rename(us_federal_funds_rate = fedfunds)
  
  dat
}

# Define a function for cleaning the field grown tomato price data
clean_tomato_prices <- function(data) {
  dat <- data
  
  # snake case the column nmaes
  dat <- dat %>% clean_names()
  
  # add a month column
  dat <- dat %>% mutate(month = as.integer(substr(period, 2, 3)))
  
  # rename the value column to represent the measure
  dat <- dat %>% rename(us_city_avg_field_grown_tomato_price_per_lb = value)
  
  # drop the series id and period columns
  dat <- dat %>% select(-series_id, -period)
  
  dat
}

# Define a function for joining monthly data to the final analysis dataset
prepare_monthly_analysis <- function(
  monthly_orders, employment, oecd, federal_funds_rate, field_grown_tomato_price
) {
  
  # join the orders with the employment data
  dat <- monthly_orders %>% inner_join(employment, by = c(
    "planned_delivery_month_num" = "month",
    "planned_delivery_year"      = "year"))
  
  # join with the oecd CCI and BCI data
  dat <- dat %>% inner_join(oecd, by = c(
    "planned_delivery_month_num" = "month",
    "planned_delivery_year"      = "year"
  ))
  
  # join with the federal funds rate data
  dat <- dat %>% inner_join(federal_funds_rate, by = c(
    "planned_delivery_month_num" = "month",
    "planned_delivery_year"      = "year"
  ))
  
  # join with the us city avg field grown tomato price data
  dat <- dat %>% inner_join(field_grown_tomato_price, by = c(
    "planned_delivery_month_num" = "month",
    "planned_delivery_year"      = "year"
  ))
  
  dat
}

# --------------------OLD-----------------------------------------------------

# # Load and Clean Raw Wikipedia State Abbreviations Table  ----
# read_wiki_state_table <- function(path, states_and_fed_dist_only = TRUE) {
#   
#   # define region_types to include when states_and_fed_dist_only = TRUE
#   domestic_types <- c("State", "Federal district")
#   
#   # define the schema of the wikipedia state table
#   schema <- cols(
#     state_or_region    = col_character(),
#     region_type        = readr::col_factor(NULL),
#     ISO                = col_character(),
#     ANSI_alpha         = col_character(),
#     USPS               = col_character(),
#     USCG               = col_character(),
#     GPO                = col_character(),
#     AP                 = col_character(),
#     Otherabbreviations = col_character()
#   )
#   
#   # read the CSV file
#   dat <- read_csv(path, col_types = schema)
#   
#   # Clean up the region_type column
#   dat$region_type <- fct_collapse(
#     dat$region_type,
#     State                     = c("State", "State (Commonwealth)"),
#     "Insular Area"            = c("Insular area (Territory)", "Insular areas",
#                                   "Insular area (Commonwealth)"),
#     Island                    = c("island"),
#     Atoll                     = c("atoll", "atoll[c]"),
#     "Freely associated state" = c("Freely associated state"),
#     "US military mail code"   = c("US military mail code"),
#     "Obsolete postal code"    = c("Obsolete postal code[g]",
#                                   "Obsolete postal code",
#                                   "Obsolete postal code[h]")
#   )
#   
#   # if only including states and federal districts, filter the other rows out
#   if (states_and_fed_dist_only == TRUE) {
#     dat <- dat %>% filter(region_type %in% domestic_types)
#   }
#   
#   # include a lowercase version of the state or region column
#   state_or_region_lower <- str_to_lower(dat$state_or_region)
#   dat <- cbind.data.frame(dat, state_or_region_lower)
#   
#   # return the final dataset
#   dat
# }
# 
# # Read the Canadian Provinces Table  ----
# read_raw_provinces <- function(
#   path = "data/raw/reference_data/CountryCAN_e.xls"
# ) {
#   
#   # define schema
#   schema <- c("text", "text")
#   
#   # read the data
#   dat <- read_excel(path, sheet = "Canada English")
#   
#   # align with states dataset
#   dat <- dat %>%
#     rename(
#       state_or_region = `Province or Territory Name`,
#       USPS            = Code
#     ) %>%
#     mutate(region_type = "CA Province")
#   
#   state_or_region_lower <- str_to_lower(dat$state_or_region)
#   dat <- cbind.data.frame(dat, state_or_region_lower)
#   
#   dat
# }
# 
# 
# 
# # Read the clean state abbreviations table  ----
# read_state_abbreviations <- 
#   function(path = paste0("data/pipeline/reference_data/",
#                          "state_abbreviations.csv")) {
#     
#     schema <- cols(
#       state_or_region       = col_character(),
#       region_type           = readr::col_factor(NULL),
#       ISO                   = col_character(),
#       ANSI_alpha            = col_character(),
#       USPS                  = col_character(),
#       USCG                  = col_character(),
#       GPO                   = col_character(),
#       AP                    = col_character(),
#       Otherabbreviations    = col_character(),
#       state_or_region_lower = col_character()
#     )
#     
#     dat <- read_csv(path, col_types = schema)
#     
#     dat
#   }
# 
# # Read in the Raw Sales Order Data into memory  ----
# read_raw_sales <- 
#   function(path = paste0("data/raw/Sales_data_for_",
#                          "Capstone_Project_-_Student_Version_-_v3.xlsx")) {
#     
#     # define the schema (readxl has a different method for col_types, below
#     # code commented and retained for posterity's sake.)
#     # schema <- cols(
#     #   fiscalquartername     = col_character(),
#     #   FiscalPeriod          = col_character(),
#     #   FiscalYear            = col_integer(),
#     #   Company               = col_character(),
#     #   Top_Most_BP           = col_character(),
#     #   BusinessPartner       = col_character(),
#     #   Item_Number           = col_factor(NULL),
#     #   Item_Description      = col_character(),
#     #   Ship_to_State         = col_factor(NULL),
#     #   Ship_to_City          = col_character(),
#     #   Quantity              = col_integer(),
#     #   Planned_Delivery_Date = col_date("%Y-%m-%d"),
#     #   Address_1             = col_character(),
#     #   Address_2             = col_character(),
#     #   Postal_Code           = col_character(),
#     #   Budget_Qty            = col_integer(),
#     #   Forecast_Qty          = col_integer(),
#     #   Line_of_Business      = col_factor(NULL)
#     # )
#     
#     schema = c("text", "text", "numeric", "text", "text", "text", "text", 
#                "text", "text", "text", "numeric", "date", "text", "text", 
#                "text", "numeric", "numeric", "text")
#     
#     # read the excel file
#     dat <- read_excel(
#       path, sheet = "Processed Fruits", 
#       col_names = TRUE, col_types = schema, na = c("", "NULL", "  ", "N/A"))
#     
#     dat
#   }
# 
# # Clean City/State Pair (only works for US States and DC). Function  ----
# # returns a list of length 3, corresponding to city, state, and international
# # status. 
# clean_city_state <- function(
#   city, state, 
#   state_abbrev_path = paste0("data/pipeline/reference_data/",
#                              "state_abbreviations_domestic.csv")
# ) {
#   
#   # define two character 'state' string pattern
#   two_char_pattern <- "^.+\\s+([A-Z]{2})(\\s+.*|$)"
#   
#   # define a pattern for matching the full state name
#   full_state_pattern <- paste0(
#     "^.*(alabama|alaska|arizona|arkansas|california|colorado|connecticut|delaw",
#     "are|district of columbia|florida|georgia|hawaii|idaho|illinois|indiana|io",
#     "wa|kansas|kentucky|louisiana|maine|maryland|massachusetts|michigan|minnes",
#     "ota|mississippi|missouri|montana|nebraska|nevada|new hampshire|new jersey",
#     "|new mexico|new york|north carolina|north dakota|ohio|oklahoma|oregon|pen",
#     "nsylvania|rhode island|south carolina|south dakota|tennessee|texas|utah|v",
#     "ermont|virginia|washington|west virginia|wisconsin|wyoming)")
#   full_prov_pattern <- paste0("(alberta|britis",
#     "columbia|manitoba|new brunswick|newfoundland and labradornorthwest territ",
#     "ories|nova scotia|nunavut|ontario|prince edward island|quebec|saskatchewa",
#     "|yukon|republica dominicana).*$"
#   )
#   
#   # check if state abbreviation table is loaded, and load if not
#   if (!exists("state_abbreviations_dat")) {
#     state_abbreviations_dat <- read_state_abbreviations(state_abbrev_path)
#   }
#   
#   # classify the state value
#   state_class <- if (is.na(state)) "blank" else {
#     if (state %in% state_abbreviations_dat$USPS) "dom" else "int"
#   }
#   
#   # classify the city value
#   if (is.na(city)) {
#     city_class <- "blank"
#     city_has_two_char_pattern <- FALSE
#     matches_full_state <- FALSE
#     matches_full_province <- FALSE
#     
#   } else {
#     city_has_two_char_pattern <- str_detect(city, two_char_pattern)
#     matches_full_state <- FALSE
#     
#     if (city_has_two_char_pattern) {
#       two_char <- str_match(city, two_char_pattern)[1, 2]
#       two_char_is_state <- two_char %in% state_abbreviations_dat$USPS
#       
#       if (two_char_is_state) {
#         city_class <- "2_dom"
#         state_in_city <- two_char
#         
#       } else {
#         city_class <- "int"
#         
#       }
#       
#     } else {
#       matches_full_state <- str_detect(str_to_lower(city), full_state_pattern) |
#         str_detect(str_to_lower(city), full_prov_pattern)
#     
#       if (matches_full_state) {
#         city_class <- "full_dom"
#         full_state <- str_match(str_to_lower(city), full_state_pattern)[1, 2]
#         state_in_city <- state_abbreviations_dat[
#           state_abbreviations_dat$state_or_region_lower == full_state, 
#           6
#           ]
#       } else {
#         city_class <- "unknown"
#       }
#       
#     }
#   }
#   
#   # define the final outputs
#   if (state_class == "dom" | state_class == "int") {
#     state_clean <- state
#   } else if (state_class == "blank" & city_has_two_char_pattern) {
#     state_clean <- state_in_city
#   } else if (state_class == "blank" & matches_full_state) {
#     state_clean <- state_in_city
#   } else {
#     state_clean <- NA
#   }
#   
#   # note that city will not currently "clean". If people want the cities cleaned
#   # I can put work in, but unless anyone wants city level data I'll leave the
#   # returned city value equal to the inputted city value. Recommend replacing
#   # the city column in your code anyway to avoid having to make modifications if
#   # a change is implemented
#   city <- city
#   dom_int <- if (state_class == "dom" | city_class %in% c("2_dom", "full_dom")) {
#     "domestic"
#   } else "unknown"
#   
#   # assemble
#   output <- c(city, state_clean, dom_int)
#   
#   output
# }
# 
# # Write the clean order-level dataset
# write_clean_sales_data <- function(
#   df, 
#   dest = "data/clean/clean_tomato_bag_sales.csv"
# ) {
#   write_csv(df, dest)
# }
# 
# # Read the Cleaned and integrated Sales Data  ----
# read_clean_sales <- function(path = "data/clean/clean_tomato_bag_sales.csv") {
#   
#   month_levels <- c(
#     "Jan", "Feb", "Mar", "Apr", "May", "Jun",
#     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
#   )
#   
#   schema <- cols(
#     fiscalquartername          = col_character(),
#     FiscalPeriod               = col_character(),
#     FiscalYear                 = col_integer(),
#     Company                    = col_character(),
#     Top_Most_BP                = col_character(),
#     BusinessPartner            = col_character(),
#     Item_Number                = readr::col_factor(NULL),
#     Item_Description           = col_character(),
#     Ship_to_State              = readr::col_factor(NULL),
#     Ship_to_City               = col_character(),
#     Quantity                   = col_number(),
#     Planned_Delivery_Date      = col_datetime(),
#     Address_1                  = col_character(),
#     Address_2                  = col_character(),
#     Postal_Code                = col_character(),
#     Budget_Qty                 = col_number(),
#     Forecast_Qty               = col_number(),
#     Line_of_Business           = readr::col_factor(NULL),
#     domestic_or_international  = readr::col_factor(NULL),
#     planned_delivery_year      = col_integer(),
#     planned_delivery_month     = readr::col_factor(levels = month_levels,
#                                                    ordered = TRUE),
#     planned_delivery_month_num = col_integer(),
#     planned_delivery_day       = col_integer()
#   )
#   
#   dat <- read_csv(path, col_types = schema)
#   
#   dat$Quantity <- as.integer(dat$Quantity)
#   
#   dat
# }
