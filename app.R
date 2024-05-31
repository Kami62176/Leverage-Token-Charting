
# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(gridExtra)
library(plotly)
library(scales)
library(lubridate)


btc_data = fromJSON("./priceData/btc-price.json")
price_data <- btc_data$prices
btc_df <- data.frame(timestamp = as.POSIXct(price_data[, 1] / 1000, origin = "1970-01-01"),
                     price = price_data[, 2])

eth_data = fromJSON("./priceData/eth-price.json")
price_data <- eth_data$prices
eth_df <- data.frame(timestamp = as.POSIXct(price_data[, 1] / 1000, origin = "1970-01-01"),
                     price = price_data[, 2])

sol_data = fromJSON("./priceData/sol-price.json")

price_data <- sol_data$prices
sol_df <- data.frame(timestamp = as.POSIXct(price_data[, 1] / 1000, origin = "1970-01-01"),
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

# Define calculation functions
calculate_nav <- function(initial_nav, leverage, price_change) {
  return(round(initial_nav * (1 + (leverage * price_change)),2))
}

calculate_cumulative_change <- function(nav, initial_nav) {
  return(round(((nav - initial_nav) / initial_nav), 2))
}

calculate_actual_leverage <- function(basket, price, nav, circulating_supply) {
  return(round((basket * price) / (nav * circulating_supply), 2))
}




simulate_leverage <- function(hourly_df, leverage, lower, upper) {
  # Set initial parameters
  initial_nav <- 10
  initial_price <- hourly_df$price[1]
  basket <- 300
  circulating_supply <- 400000
  target_leverage <- leverage
  current_leverage <- target_leverage
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
    navs <- c(navs, nav_today)
    
    # Calculate cumulative change in NAV
    cumulative_change_today <- calculate_cumulative_change(nav_today, initial_nav)
    cumulative_changes <- c(cumulative_changes, cumulative_change_today)  # in percentage
    
    # Calculate actual leverage
    current_leverage <- calculate_actual_leverage(basket, price_today, nav_today, circulating_supply)
    actual_leverages <- c(actual_leverages, current_leverage)
    
    # Check if rebalancing is needed
    if (current_leverage >= target_leverage + upper 
        || current_leverage <= target_leverage - lower) {  # Rebalancing threshold
      target_position <- nav_today * circulating_supply * target_leverage
      current_position <- basket * price_today
      rebalance_position <- (target_position - current_position) / price_today
      basket <- basket + rebalance_position  # Adjust basket size
      
      # Recalculate actual leverage after rebalancing
      current_leverage <- calculate_actual_leverage(basket, price_today, nav_today, circulating_supply)
      actual_leverages[length(actual_leverages)] <- current_leverage
    }
  }
  
  # Create a data frame to display the results
  hourly_results_df <- data.frame(
    Date = hourly_df$timestamp,
    BTC_Price = hourly_df$price,
    NAV = navs,
    Cumulative_Change_in_NAV = cumulative_changes,
    Actual_Leverage = actual_leverages
  )
  
  # Aggregate the results to daily data
  daily_results_df <- hourly_results_df %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Date) %>%
    summarize(
      BTC_Price = first(BTC_Price),
      NAV = first(NAV),
      Cumulative_Change_in_NAV = first(Cumulative_Change_in_NAV),
      Actual_Leverage = first(Actual_Leverage)
  )
  return(daily_results_df)
}
# Plotting the results using ggplot2


options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

# Define UI
ui <- fluidPage(
  titlePanel("Bracketed Leverage Token Simulation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crypto", "Select Token: ", 
                  choices = list("Bitcoin" = "btc", "Ethereum" = "eth", "Solana" = "sol")),
      numericInput("leverage", "Leverage:", value = 2, min = 1, max = 50, step = 1),
      numericInput("upperBound", "Upper Bound:", value = 0.2, min = 0.1, max = 50, step = 0.1),
      numericInput("lowerBound", "Lower Bound:", value = 0.2, min = 0.1, max = 50, step = 0.1)
    ),
    mainPanel(
      plotlyOutput("btc_price_plot"),
      plotlyOutput("nav_plot"),
      plotlyOutput("Cumulative_Change_in_NAV_plot"),
      plotlyOutput("leverage_plot")
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  # Load data based on the selected cryptocurrency
  load_crypto_data <- reactive({
    switch(input$crypto,
           "btc" = btc_df,
           "eth" = eth_df,
           "sol" = sol_df)
  })
  
  get_crypto_name <- reactive({
    switch(input$crypto,
           "btc" = "Bitcoin Price",
           "eth" = "Ethereum Price",
           "sol" = "Solana Price")
  })
  
  result_data <- reactive({
    df <- load_crypto_data()
    hourly_df <- interpolate_hourly(df)
    leverage <- input$leverage
    LowerBound <- input$lowerBound
    UpperBound <- input$upperBound
    simulation_data <- simulate_leverage(hourly_df, leverage, LowerBound, UpperBound)
  })
  
  # BTC Price Plot
  output$btc_price_plot <- renderPlotly({
    data <- result_data()
    chart_title <- get_crypto_name()
    btc_price_plot <- ggplot(data, aes(x = Date, y = BTC_Price)) +
      geom_line(color = "blue") +
      scale_y_continuous(labels = scales::number_format(accuracy = 1)) +  # Whole numbers without scientific notation
      labs(title = chart_title, y = "Price (USD)") +
      theme_minimal()
  })
  
  # NAV Plot
  output$nav_plot <- renderPlotly({
    data <- result_data()
    leverage <- input$leverage
    leverage_plot_title <- paste("Net Asset Value of", leverage,"x Bracketed Leverage Token")
    nav_plot <- ggplot(data, aes(x = Date, y = NAV)) +
      geom_line(color = "green") +
      scale_y_continuous(labels = scales::number_format(accuracy = 1)) +  # Whole numbers without scientific notation
      labs(title = leverage_plot_title, y = "NAV") +
      theme_minimal()
  })
  
  # Cumulative Change in NAV Plot
  output$Cumulative_Change_in_NAV_plot <- renderPlotly({
    data <- result_data()
    Cumulative_Change_in_NAV_plot <- ggplot(data, aes(x = Date, y = Cumulative_Change_in_NAV)) +
      geom_line(color = "black") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Net % Return", y = "% Change") +
      theme_minimal()
  })
  
  # Actual Leverage Plot
  output$leverage_plot <- renderPlotly({
    data <- result_data()
    leverage <- input$leverage
    LowerBound <- input$lowerBound
    UpperBound <- input$upperBound
    leverage_plot <- ggplot(data, aes(x = Date, y = Actual_Leverage)) +
      geom_line(color = "red") +
      geom_hline(yintercept = leverage, linetype = "dashed", color = "grey") +
      geom_hline(yintercept = (leverage + UpperBound), linetype = "dashed", color = "black") +
      geom_hline(yintercept = (leverage - LowerBound), linetype = "dashed", color = "black") +
      labs(title = "Actual Leverage of Token", y = "Actual Leverage") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
