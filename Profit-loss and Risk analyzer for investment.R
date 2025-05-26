# Install and load required packages
if (!require("quantmod")) install.packages("quantmod")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("scales")) install.packages("scales")
if (!require("reshape2")) install.packages("reshape2")
library(quantmod)
library(ggplot2)
library(scales)
library(reshape2)

# Function to validate stock symbol
validate_stock_symbol <- function(symbol) {
  tryCatch({
    test_data <- getSymbols(symbol, auto.assign = FALSE, from = Sys.Date() - 10)
    if (nrow(test_data) > 0) return(TRUE)
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Stock analysis with risk assessment
analyze_stock <- function() {
  cat("\n=== Stock/Index Profit-Loss & Risk Analysis ===\n")
  
  # Get validated symbol
  while (TRUE) {
    symbol <- toupper(readline("Enter stock/index symbol (e.g., AAPL, ^GSPC, BTC-USD): "))
    if (validate_stock_symbol(symbol)) break
    cat("Invalid symbol. Examples:\n",
        "- Stocks: AAPL, TSLA\n",
        "- Indices: ^GSPC (S&P 500), ^IXIC (NASDAQ)\n",
        "- Crypto: BTC-USD\n")
  }
  
  # Get investment parameters
  investment <- as.numeric(readline("Enter investment amount ($): "))
  years <- as.numeric(readline("Enter holding period (years): "))
  
  # Get historical data
  end_date <- Sys.Date()
  start_date <- end_date - (years * 365)
  stock_data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  prices <- Ad(stock_data)
  
  # Calculate returns
  start_price <- as.numeric(first(prices))
  end_price <- as.numeric(last(prices))
  return_pct <- (end_price - start_price) / start_price * 100
  profit_loss <- investment * return_pct/100
  future_value <- investment + profit_loss
  
  # Risk analysis
  daily_returns <- dailyReturn(prices)
  annual_volatility <- sd(daily_returns, na.rm = TRUE) * sqrt(252) * 100
  max_drawdown <- min(daily_returns) * 100
  positive_days <- mean(daily_returns > 0, na.rm = TRUE) * 100
  
  # Risk category
  risk_category <- ifelse(annual_volatility > 30, "High Risk",
                          ifelse(annual_volatility > 15, "Medium Risk", "Low Risk"))
  
  # Create results table
  results <- data.frame(
    Metric = c("Symbol", "Investment", "Start Price", "End Price",
               "Return %", "Profit/Loss ($)", "Future Value",
               "Annual Volatility %", "Max Daily Loss %", 
               "Positive Days %", "Risk Category"),
    Value = c(symbol,
              dollar(investment),
              dollar(start_price),
              dollar(end_price),
              sprintf("%.2f%%", return_pct),
              dollar(profit_loss),
              dollar(future_value),
              sprintf("%.2f%%", annual_volatility),
              sprintf("%.2f%%", max_drawdown),
              sprintf("%.2f%%", positive_days),
              risk_category)
  )
  
  # Print results
  cat("\n=== Analysis Results ===\n")
  print(results, row.names = FALSE)
  
  write.csv(results, sprintf("stock_%s_%s.csv", symbol, Sys.Date()), row.names = FALSE)
  
  # Investment conclusion
  cat("\n=== Investment Conclusion ===\n")
  if (return_pct > 0) {
    if (risk_category == "High Risk") {
      cat(sprintf("WARNING: This investment gained %.2f%% but carries HIGH RISK (Volatility: %.2f%%)\n", 
                  return_pct, annual_volatility))
      cat("Consider smaller position size or hedging strategies.\n")
    } else if (risk_category == "Medium Risk") {
      cat(sprintf("This investment gained %.2f%% with MODERATE RISK (Volatility: %.2f%%)\n",
                  return_pct, annual_volatility))
      cat("Could be suitable for balanced portfolios.\n")
    } else {
      cat(sprintf("GOOD INVESTMENT: Gained %.2f%% with LOW RISK (Volatility: %.2f%%)\n",
                  return_pct, annual_volatility))
      cat("Consider increasing position if it aligns with your strategy.\n")
    }
  } else {
    cat(sprintf("CAUTION: This investment lost %.2f%% with %s\n",
                abs(return_pct), risk_category))
    if (risk_category == "High Risk") {
      cat("Not recommended unless you have very high risk tolerance.\n")
    } else {
      cat("Review fundamentals before considering this investment.\n")
    }
  }
  
  # Create and display plots
  price_df <- data.frame(Date = index(prices), Price = as.numeric(prices))
  p1 <- ggplot(price_df, aes(x = Date, y = Price)) +
    geom_line(color = ifelse(return_pct >= 0, "steelblue", "firebrick"), size = 1) +
    labs(title = paste(symbol, "Price History"), 
         subtitle = sprintf("%.2f%% Return | %s Risk", return_pct, risk_category),
         y = "Price ($)") +
    scale_y_continuous(labels = dollar) +
    theme_minimal()
  
  returns_df <- data.frame(Return = as.numeric(daily_returns))
  p2 <- ggplot(returns_df, aes(x = Return)) +
    geom_histogram(fill = "lightgreen", bins = 30, color = "black") +
    labs(title = "Daily Returns Distribution", 
         subtitle = sprintf("Volatility: %.2f%% | Max Daily Loss: %.2f%%", 
                            annual_volatility, max_drawdown),
         x = "Daily Return", y = "Frequency") +
    theme_minimal()
  
  print(p1)
  invisible(readline(prompt = "Press [enter] to see risk analysis..."))
  print(p2)
}

# Bank savings analysis
analyze_savings <- function() {
  cat("\n=== Bank Savings Profit-Loss & Risk Analysis ===\n")
  
  principal <- as.numeric(readline("Enter initial amount ($): "))
  annual_rate <- as.numeric(readline("Enter annual interest rate (%): ")) / 100
  years <- as.numeric(readline("Enter investment period (years): "))
  
  # Calculate returns
  future_value <- principal * (1 + annual_rate)^years
  profit_loss <- future_value - principal
  return_pct <- (future_value - principal)/principal * 100
  
  # Risk is considered zero for bank savings
  risk_category <- "No Risk"
  
  # Create results table
  results <- data.frame(
    Metric = c("Investment", "Interest Rate", "Period (years)",
               "Return %", "Profit/Loss ($)", "Future Value",
               "Risk Category"),
    Value = c(dollar(principal),
              sprintf("%.2f%%", annual_rate * 100),
              years,
              sprintf("%.2f%%", return_pct),
              dollar(profit_loss),
              dollar(future_value),
              risk_category)
  )
  
  # Print results
  cat("\n=== Analysis Results ===\n")
  print(results, row.names = FALSE)
  
  write.csv(results, sprintf("savings_%s.csv", Sys.Date()), row.names = FALSE)
  
  
  # Investment conclusion
  cat("\n=== Investment Conclusion ===\n")
  if (annual_rate * 100 > 5) {
    cat(sprintf("SAFE INVESTMENT: Earns %.2f%% with %s\n", 
                annual_rate * 100, risk_category))
    cat("Good for capital preservation and low-risk investors.\n")
  } else {
    cat(sprintf("LOW RETURN: Only %.2f%% with %s\n", 
                annual_rate * 100, risk_category))
    cat("Consider other options if you can tolerate some risk.\n")
  }
  
  # Create growth plot
  yearly_growth <- data.frame(
    Year = 0:years,
    Amount = principal * (1 + annual_rate)^(0:years)
  )
  
  growth_plot <- ggplot(yearly_growth, aes(x = Year, y = Amount)) +
    geom_line(color = "darkgreen", size = 1.5) +
    geom_point(color = "forestgreen", size = 3) +
    scale_y_continuous(labels = dollar) +
    labs(title = "Projected Growth", 
         subtitle = sprintf("%.2f%% Annual Interest | %s", annual_rate * 100, risk_category),
         y = "Amount ($)") +
    theme_minimal()
  
  print(growth_plot)
}

# Comparative analysis
compare_investments <- function() {
  cat("\n=== Comparative Investment Analysis ===\n")
  
  # Get stock parameters
  cat("\n[Stock/Index Investment]\n")
  stock_symbol <- toupper(readline("Enter symbol (e.g., AAPL, ^GSPC): "))
  stock_years <- as.numeric(readline("Enter holding period (years): "))
  
  # Get bank parameters
  cat("\n[Bank Savings]\n")
  principal <- as.numeric(readline("Enter initial amount ($): "))
  bank_rate <- as.numeric(readline("Enter annual interest rate (%): ")) / 100
  
  # Calculate stock returns (simplified)
  stock_return <- tryCatch({
    stock_data <- getSymbols(stock_symbol, auto.assign = FALSE, 
                             from = Sys.Date() - (stock_years * 365),
                             to = Sys.Date())
    as.numeric(Ad(last(stock_data))) / as.numeric(Ad(first(stock_data))) - 1
  }, error = function(e) {
    cat("Using default 8% annual return for stock\n")
    0.08
  })
  
  stock_future <- principal * (1 + stock_return)
  stock_profit <- stock_future - principal
  
  # Calculate bank returns
  bank_future <- principal * (1 + bank_rate)^stock_years
  bank_profit <- bank_future - principal
  
  # Risk assessment
  stock_risk <- ifelse(stock_return * 100 > 30, "High",
                       ifelse(stock_return * 100 > 15, "Medium", "Low"))
  bank_risk <- "None"
  
  # Create comparison table
  comparison <- data.frame(
    Metric = c("Initial Amount", "Return %", "Profit/Loss ($)", 
               "Future Value", "Risk Level"),
    Stock = c(dollar(principal),
              sprintf("%.2f%%", stock_return * 100),
              dollar(stock_profit),
              dollar(stock_future),
              stock_risk),
    Bank = c(dollar(principal),
             sprintf("%.2f%%", bank_rate * 100),
             dollar(bank_profit),
             dollar(bank_future),
             bank_risk)
  )
  
  # Print comparison
  cat("\n=== Side-by-Side Comparison ===\n")
  print(comparison, row.names = FALSE)
  
  write.csv(comparison, sprintf("comparison_%s_%s.csv", stock_symbol, Sys.Date()), row.names = FALSE)
  
  # Create comparison plot
  df <- data.frame(
    Option = c("Stock", "Bank"),
    Profit = c(stock_profit, bank_profit),
    Risk = factor(c(stock_risk, bank_risk), levels = c("None", "Low", "Medium", "High"))
  )
  
  p <- ggplot(df, aes(x = Option, y = Profit, fill = Risk)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c("None" = "darkgreen", "Low" = "lightgreen", 
                                 "Medium" = "orange", "High" = "red")) +
    scale_y_continuous(labels = dollar) +
    geom_text(aes(label = dollar(Profit)), vjust = -0.5) +
    labs(title = "Profit Comparison", 
         subtitle = sprintf("%d-Year Investment Period", stock_years),
         y = "Profit/Loss ($)") +
    theme_minimal()
  
  print(p)
  
  # Final recommendation
  cat("\n=== Final Recommendation ===\n")
  if (stock_profit > bank_profit * 1.5 && stock_risk != "High") {
    cat("RECOMMENDATION: Invest in STOCKS for higher returns with", stock_risk, "risk\n")
  } else if (stock_profit > bank_profit * 2) {
    cat("RECOMMENDATION: Consider stocks but with caution (High potential but", stock_risk, "risk)\n")
  } else if (bank_profit > 0) {
    cat("RECOMMENDATION: Choose BANK SAVINGS for safer returns\n")
  } else {
    cat("RECOMMENDATION: Explore other investment options\n")
  }
}

# Main menu
profit_loss_analyzer <- function() {
  while (TRUE) {
    cat("\n=== Profit-Loss & Risk Analyzer for Investment ===\n")
    cat("1. Analyze Stock/Index Investment\n")
    cat("2. Analyze Bank Savings\n")
    cat("3. Compare Both Investments\n")
    cat("4. Exit\n")
    
    choice <- readline("Enter your choice (1-4): ")
    
    if (choice == "1") {
      analyze_stock()
    } else if (choice == "2") {
      analyze_savings()
    } else if (choice == "3") {
      compare_investments()
    } else if (choice == "4") {
      cat("Exiting Profit-Loss & Risk Analyzer...\n")
      break
    } else {
      cat("Invalid choice. Please try again.\n")
    }
    
    invisible(readline(prompt = "Press [enter] to continue..."))
  }
}

# Run the analyzer
profit_loss_analyzer()
