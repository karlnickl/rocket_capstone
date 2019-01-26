# Load Required Libraries ----
library(tidyverse)  # for data manipulation and plotting
library(scales)  # for graph formatting
library(gridExtra)  # for arranging plots
library(grid)  # for arranging plots
source("../capstone_engineering/data_prep_utils.R")  # project utilities

# Define Inputs ----

# Path for cleaned sales data
sales_path <- "../capstone_engineering/data/clean/clean_tomato_bag_sales.csv"

# Read the integrated sales data  ----
sales_dat <- read_clean_sales(sales_path)

# Plot the States Occuring by Year  ----

# aggregate data
orders_by_state_by_year_dat <- sales_dat %>%
  filter(domestic_or_international == "domestic") %>%
  group_by(Ship_to_State, planned_delivery_year) %>%
  summarise(
    total_orders        = n(),
    total_quantity      = sum(Quantity),
    mean_order_quantity = mean(Quantity),
    sd_order_quantity   = sd(Quantity)
  )

states_by_year_dat <- sales_dat %>%
  filter(domestic_or_international == "domestic") %>%
  group_by(planned_delivery_year) %>%
  summarise(state_count = n_distinct(Ship_to_State))
  

# plot the Total Order Quantity by State and Year
orders_by_state_by_year_dat %>%
  ggplot(aes(
    x = planned_delivery_year, 
    y = total_quantity, 
    fill = Ship_to_State
  )) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(label = comma, name = "Total Quantity", expand = c(0, 0)) +
  scale_x_continuous(name = "Planned Delivery Year") +
  scale_fill_discrete(name = "Ship to State") +
  ggtitle("Total Order Quantity by State and Year") +
  theme_light()

# plot the order count by state and year
orders_by_state_by_year_dat %>%
  ggplot(aes(
    x = planned_delivery_year, 
    y = total_orders, 
    fill = Ship_to_State
  )) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(
    label = comma, 
    name = "Total Number of Orders", 
    expand = c(0, 0)
  ) +
  scale_x_continuous(name = "Planned Delivery Year") +
  scale_fill_discrete(name = "Ship to State") +
  ggtitle("Total Number of Orders by State and Year") +
  theme_light()

# plot the average order size
orders_by_state_by_year_dat %>%
  ggplot(aes(
    x = planned_delivery_year, 
    y = mean_order_quantity, 
    fill = Ship_to_State
  )) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(
    label = comma, 
    name = "Mean Order Quantity", 
    expand = c(0, 0)
  ) +
  scale_x_continuous(name = "Planned Delivery Year") +
  scale_fill_discrete(name = "Ship to State") +
  ggtitle("Mean Order Quantity by State and Year") +
  theme_light()

# plot the number of states per year
states_by_year_dat %>%
  ggplot(aes(x = planned_delivery_year, y = state_count)) +
  geom_col() +
  scale_y_continuous(
    label = comma, 
    name = "Number of States", 
    expand = c(0, 0)
  ) +
  scale_x_continuous(name = "Planned Delivery Year") +
  ggtitle("Number of Ship to States by Year") +
  theme_light()

# Create a dataframe of orders by state by month
orders_by_state_by_month_dat <- sales_dat %>%
  filter(domestic_or_international == "domestic") %>%
  group_by(Ship_to_State, planned_delivery_month) %>%
  summarise(
    total_orders        = n(),
    total_quantity      = sum(Quantity),
    mean_order_quantity = mean(Quantity),
    sd_order_quantity   = sd(Quantity)
  )

# Define a function for plotting monthly state ordres
plot_order_quantity_by_month_for_state <- function(state, dat) {
  dat <- dat %>% filter(Ship_to_State == state)
  
  plt <- dat %>%
    ggplot(aes(x = planned_delivery_month, y = total_quantity)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
      label = comma, 
      name = "Total Order Quantity", 
      expand = c(0, 0)
    ) +
    scale_x_discrete(name = "Planned Delivery Month", drop = FALSE) +
    ggtitle(state)
  
  plt
}

# Get distinct states
unique_states <- sales_dat %>%
  filter(domestic_or_international == "domestic") %>%
  pull(Ship_to_State) %>%
  as.character() %>%
  unique() %>%
  sort()
  

# Plot each states total order quantity by month
state_monthly_quantity_plt_lst <- lapply(
  unique_states, 
  plot_order_quantity_by_month_for_state,
  dat = orders_by_state_by_month_dat
)

grid.arrange(
  grobs = state_monthly_quantity_plt_lst, 
  top = textGrob("Total Order Quantity by Month", gp=gpar(fontsize=20))
)

