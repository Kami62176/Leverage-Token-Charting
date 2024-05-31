
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(jsonlite)
library(gridExtra)
library(plotly)
library(scales)

# Load the JSON data
btc_data <- fromJSON("./sol-price.json")

# Extract the price data
price_data <- btc_data$prices
df <- data.frame(timestamp = as.POSIXct(price_data[, 1] / 1000, origin = "1970-01-01"),
                 price = price_data[, 2])

# Function to interpolate hourly data
interpolate_hourly <- function(df) {
  hourly_data <- data.frame()
  for (i in 1:(nrow(df) - 1)) {
    start_time <- df$timestamp[i]
    end_time <- df$timestamp[i + 1]
    start_price <- df$price[i]
    end_price <- df$price[i + 1]
    
    hourly_times <- seq(from = start_time, to = end_time, by = "hour")
    hourly_prices <- seq(from = start_price, to = end_price, length.out = length(hourly_times))
    
    hourly_data <- rbind(hourly_data, data.frame(timestamp = hourly_times, price = hourly_prices))
  }
  return(hourly_data)
}
# Generate hourly data
hourly_df <- df #interpolate_hourly(df)

#### Simulation 


# Set initial parameters
initial_nav <- 10
initial_price <- df$price[1]
basket <- 300
circulating_supply <- 400000
target_leverage <- 3
current_leverage <- target_leverage
leverage_range <- 1

# Define calculation functions
calculate_nav <- function(initial_nav, leverage, price_change) {
  return(initial_nav * (1 + leverage * price_change))
}

calculate_cumulative_change <- function(nav, initial_nav) {
  return((nav - initial_nav) / initial_nav)
}

calculate_actual_leverage <- function(basket, price, nav, circulating_supply) {
  return((basket * price) / (nav * circulating_supply))
}

# Initialize lists to store the results
navs <- c(initial_nav)
cumulative_changes <- c(0)
actual_leverages <- c(target_leverage)

# Simulate hour-by-hour changes
for (i in 2:nrow(hourly_df)) {
  price_today <- hourly_df$price[i]
  price_yesterday <- hourly_df$price[i-1]
  price_change <- (price_today - price_yesterday) / price_yesterday
  
  # Calculate NAV for today
  nav_today <- calculate_nav(navs[i-1], current_leverage, price_change)
  navs <- c(navs, round(nav_today,2))
  
  # Calculate cumulative change in NAV
  cumulative_change_today <- calculate_cumulative_change(nav_today, initial_nav)
  cumulative_changes <- c(cumulative_changes, round(cumulative_change_today, 2))  # in percentage
  
  # Calculate actual leverage
  current_leverage <- calculate_actual_leverage(basket, price_today, nav_today, circulating_supply)
  actual_leverages <- c(actual_leverages, current_leverage)
  
  # Check if rebalancing is needed
  if (current_leverage > target_leverage + leverage_range 
      || current_leverage < target_leverage - leverage_range) {  # Rebalancing threshold
    target_position <- nav_today * circulating_supply * target_leverage
    current_position <- basket * price_today
    rebalance_position <- (target_position - current_position) / price_today
    basket <- basket + rebalance_position  # Adjust basket size
    
    # Recalculate actual leverage after rebalancing
    current_leverage <- calculate_actual_leverage(basket, price_today, nav_today, circulating_supply)
    actual_leverages[length(actual_leverages)] <- round(current_leverage, 2)
  }
}

# Create a data frame to display the results
results_df <- data.frame(
  Date = hourly_df$timestamp,
  BTC_Price = hourly_df$price,
  NAV = navs,
  Cumulative_Change_in_NAV = cumulative_changes,
  Actual_Leverage = actual_leverages
)

daily_results_df <-results_df
## Aggregate the results to daily data
#daily_results_df <- results_df %>%
#  mutate(Date = as.Date(Date)) %>%
#  group_by(Date) %>%
#  summarize(
#    BTC_Price = first(BTC_Price),
#    NAV = first(NAV),
#    Cumulative_Change_in_NAV = first(Cumulative_Change_in_NAV),
#    Actual_Leverage = first(Actual_Leverage)
#  )

# Plotting the results using ggplot2

# BTC Price Plot
btc_price_plot <- ggplot(daily_results_df, aes(x = Date, y = BTC_Price)) +
  geom_line(color = "blue") +
  scale_y_log10() +
  labs(title = "1 Year BTC Price (Log Scale)", y = "BTC Price") +
  theme_minimal()

# NAV Plot
leverage_plot_title <- paste("Net Asset Value of", target_leverage,"x Leverage Token (Log Scale)")

nav_plot <- ggplot(daily_results_df, aes(x = Date, y = NAV)) +
  geom_line(color = "green") +
  scale_y_log10() + 
  labs(title = leverage_plot_title, y = "NAV") +
  theme_minimal()

# Actual Leverage Plot
leverage_plot <- ggplot(daily_results_df, aes(x = Date, y = Actual_Leverage)) +
  geom_line(color = "red") +
  geom_hline(yintercept = target_leverage, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = (target_leverage + leverage_range), linetype = "dashed", color = "black") +
  geom_hline(yintercept = (target_leverage - leverage_range), linetype = "dashed", color = "black") +
  labs(title = "Actual Leverage of Token", y = "Actual Leverage") +
  theme_minimal()

# Percentage change of Position
percentage_change <- ggplot(daily_results_df, aes(x = Date, y = Cumulative_Change_in_NAV)) +
  geom_line(color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Net % Return (Log Scale)", y = "% Change") +
  theme_minimal()

# Convert ggplot2 objects to plotly
btc_price_plotly <- ggplotly(btc_price_plot)
nav_plotly <- ggplotly(nav_plot)
leverage_plotly <- ggplotly(leverage_plot)
cumulative_change_plotly <- ggplotly(percentage_change)

interactive_plot_title <- paste("Bitcoin",target_leverage,"x leverage simulation" )

# Arrange the interactive plots
interactive_plots <- subplot(btc_price_plotly, nav_plotly, leverage_plotly, cumulative_change_plotly, nrows = 4, shareX = TRUE) %>%
  layout(title = interactive_plot_title)

# Print the interactive plots to ensure they are displayed
print(interactive_plots)