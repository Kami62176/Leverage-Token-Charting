
# Load necessary libraries
library(shiny)
library(shinyBS)
library(jsonlite)
library(ggplot2)
library(plotly)
library(scales)
library(gridExtra)
library(dplyr)

### DATA ACCESS


btc_data = fromJSON("./priceData/btc-hour-price.json")
price_data <- btc_data$prices
btc_df <- data.frame(timestamp = as.POSIXct(price_data[, 1] / 1000, origin = "1970-01-01"),
                     price = price_data[, 2])

eth_data = fromJSON("./priceData/eth-hour-price.json")
price_data <- eth_data$prices
eth_df <- data.frame(timestamp = as.POSIXct(price_data[, 1] / 1000, origin = "1970-01-01"),
                     price = price_data[, 2])

sol_data = fromJSON("./priceData/sol-hour-price.json")

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
  return(initial_nav * (1 + (leverage * price_change)))
}

calculate_cumulative_change <- function(nav, initial_nav) {
  return(round(((nav - initial_nav) / initial_nav), 2))
}

calculate_actual_leverage <- function(basket, price, nav, circulating_supply) {
  return((basket * price) / (nav * circulating_supply))
}


### CALCULATION FOR PROFORMANCE RATIOS


calculate_daily_returns <- function(nav) {
  returns <- diff(nav) / head(nav, -1)
  return(returns)
}

calculate_sharpe_ratio <- function(returns, risk_free_rate = 0) {
  mean_return <- mean(returns)
  excess_return <- mean_return - risk_free_rate
  sd_return <- sd(returns)
  sharpe_ratio <- excess_return / sd_return
  return(sharpe_ratio)
}

calculate_sortino_ratio <- function(returns, risk_free_rate = 0, mar = 0) {
  downside_returns <- returns[returns < mar]
  downside_deviation <- sd(downside_returns)
  mean_return <- mean(returns)
  excess_return <- mean_return - risk_free_rate
  sortino_ratio <- excess_return / downside_deviation
  return(sortino_ratio)
}

calculate_omega_ratio <- function(returns, threshold) {
  positive_returns <- returns[returns > threshold]
  negative_returns <- returns[returns <= threshold]
  
  gain_sum <- sum(positive_returns - threshold)
  loss_sum <- sum(threshold - negative_returns)
  
  omega_ratio <- gain_sum / loss_sum
  return(omega_ratio)
}




### SERVER CONFIGURATIONS

options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

### UI STUFF

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type="text/css", href = "./styling.css")
  ),
  
  titlePanel("Bracketed Leverage Token Simulation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("crypto", "Select Token: ", 
                  choices = list("Bitcoin" = "btc", "Ethereum" = "eth", "Solana" = "sol")),
      dateInput("start_date", "Start Date:", value = "2018-01-01"),
      dateInput("end_date", "End Date:", value = "2021-01-08"),
      numericInput("leverage", "Leverage:", value = 5, min = 1, max = 50, step = 1),
      numericInput("upperBound", "Upper Bound Increment:", value = 1.33, min = 0.1, max = 50, step = 0.1),
      numericInput("lowerBound", "Lower Bound Decrement:", value = 0.8, min = 0.1, max = 50, step = 0.1),
      bsTooltip(list("upperBound", "lowerBound", "leverage"), 
                "Values can range from 1 - 50", 
                "right"),
      textOutput("current_range"),
      
      hr(),
      h3("Rebalancing Inputs"),
      numericInput("fee_percentage", "Fee Percentage:", value = 0.1, min = 0, max = 50, step = 0.01),
      textOutput("rebalancing_count"),
      
      hr(),  # Add a horizontal line to separate the plot and the metrics
      h3("Risk Adjusted Performance Ratios of NAV"),
      textOutput("sharpe_ratio"),
      textOutput("sortino_ratio"),
      textOutput("omega_ratio"),
      h4("Notice: This simulation does not take into consideration the transaction fees of rebalancing.")
    ),
    
    mainPanel(
      plotlyOutput("price_plot"),
      plotlyOutput("nav_plot"),
      plotlyOutput("Cumulative_Change_in_NAV_plot"),
      plotlyOutput("leverage_plot")

    )
  )
)


### MAIN SERVER LOGIC
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
  
  filtered_data <- reactive({
    crypto_df <- load_crypto_data()
    crypto_df <- crypto_df %>%
      filter(timestamp >= as.POSIXct(input$start_date) & timestamp <= as.POSIXct(input$end_date))
    crypto_df
  })
  
  result_data <- reactive({
    df <- filtered_data()
    hourly_df <- df #interpolate_hourly(df)
    leverage <- input$leverage
    LowerBound <- input$lowerBound
    UpperBound <- input$upperBound
    percentage_fee <- input$fee_percentage
    simulation_data <- simulate_leverage(hourly_df, leverage, LowerBound, UpperBound, percentage_fee)
  })
  
  ### MAIN SIMULATION FUNCTION
  
  simulate_leverage <- function(hourly_df, leverage, lower, upper, rebalancing_fee) {
    # Set initial parameters
    initial_nav <- 10
    initial_price <- hourly_df$price[1]
    basket <- 300
    circulating_supply <- 400000
    target_leverage <- leverage
    current_leverage <- target_leverage
    # Initialize lists to store the results
    rebalancing_count <- 0
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
        rebalancing_count <- rebalancing_count + 1
        current_position <- basket * price_today
        fee <- current_position * rebalancing_fee
        
        target_position <- nav_today * circulating_supply * target_leverage
        
        rebalance_position <- (target_position - current_position) / price_today
        basket <- basket + rebalance_position  # Adjust basket size
        
        # Recalculate actual leverage after rebalancing
        current_leverage <- calculate_actual_leverage(basket, price_today, nav_today, circulating_supply)
        #actual_leverages[length(actual_leverages)] <- current_leverage
      }
    }
    
    # Create a data frame to display the results
    hourly_results_df <- data.frame(
      Date = hourly_df$timestamp,
      Price = hourly_df$price,
      NAV = navs,
      Cumulative_Change_in_NAV = cumulative_changes,
      Actual_Leverage = actual_leverages
    )
    
    # Aggregate the results to daily data
    daily_results_df <- hourly_results_df %>%
      mutate(Date = as.Date(Date)) %>%
      group_by(Date) %>%
      summarize(
        Price = first(Price),
        NAV = first(NAV),
        Cumulative_Change_in_NAV = first(Cumulative_Change_in_NAV),
        Actual_Leverage = first(Actual_Leverage),
        rebalancing_count = rebalancing_count
      )
    return(daily_results_df)
  }
  
  # BTC Price Plot
  output$price_plot <- renderPlotly({
    data <- result_data()
    chart_title <- get_crypto_name()
    btc_price_plot <- ggplot(data, aes(x = Date, y = Price)) +
      geom_line(color = "blue") +
      scale_y_continuous(labels = scales::number_format(accuracy = 1)) +  # Whole numbers without scientific notation
      labs(title = chart_title, y = "Price (USD)") +
      theme_minimal()
  })
  
  # NAV Plot
  output$nav_plot <- renderPlotly({
    data <- result_data()
    leverage <- input$leverage
    nav_plot_title <- paste("Net Asset Value of", leverage,"x Bracketed Leverage Token")
    nav_plot <- ggplot(data, aes(x = Date, y = NAV)) +
      geom_line(color = "green") +
      scale_y_continuous(labels = scales::number_format(accuracy = 1)) +  # Whole numbers without scientific notation
      labs(title = nav_plot_title, y = "NAV") +
      theme_minimal()
    ggplotly(nav_plot)
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
  
  # Current Range
  output$current_range <- renderText({
    leverage <- input$leverage
    LowerBound <- leverage - input$lowerBound
    UpperBound <- leverage + input$upperBound
    paste("Leverage Range:", LowerBound, "-", UpperBound)
  })
  
  # Actual Leverage Plot
  output$leverage_plot <- renderPlotly({
    data <- result_data()
    leverage <- input$leverage
    LowerBound <- leverage - input$lowerBound
    UpperBound <- leverage + input$upperBound
    leverage_plot_title <- paste("Actual Leverage of Token in Range: ", LowerBound, "~", UpperBound, "x")
    leverage_plot <- ggplot(data, aes(x = Date, y = Actual_Leverage)) +
      geom_line(color = "red") +
      geom_hline(yintercept = leverage, linetype = "dashed", color = "grey") +
      geom_hline(yintercept = UpperBound, linetype = "dashed", color = "black") +
      geom_hline(yintercept = LowerBound, linetype = "dashed", color = "black") +
      labs(title = leverage_plot_title, y = "Actual Leverage") +
      theme_minimal()
  })
  
  # Calculate and display risk ratios
  output$sharpe_ratio <- renderText({
    data <- result_data()
    returns <- calculate_daily_returns(data$NAV)
    sharpe_ratio <- calculate_sharpe_ratio(returns, risk_free_rate = 0.01 / 252)
    paste("Sharpe Ratio:", round(sharpe_ratio, 4))
  })
  
  output$sortino_ratio <- renderText({
    data <- result_data()
    returns <- calculate_daily_returns(data$NAV)
    sortino_ratio <- calculate_sortino_ratio(returns, risk_free_rate = 0.01 / 252, mar = 0)
    paste("Sortino Ratio:", round(sortino_ratio, 4))
  })
  
  output$omega_ratio <- renderText({
    data <- result_data()
    returns <- calculate_daily_returns(data$NAV)
    omega_ratio <- calculate_omega_ratio(returns, threshold = 0.01 / 252)
    paste("Omega Ratio:", round(omega_ratio, 4))
  })
  
  output$rebalancing_count <- renderText({
    data <- result_data()
    paste("Number of Rebalances:", data$rebalancing_count[1])
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
