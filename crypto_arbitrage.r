# Cryptocurrency Market Analysis & Arbitrage Detection System
# Author: Sairam Aadhithya
# Description: Real-time arbitrage detection across multiple crypto exchanges

# Load required libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(corrplot)

# ============================================================================
# 1. DATA COLLECTION MODULE
# ============================================================================

# Function to fetch cryptocurrency prices from Binance
get_binance_prices <- function(symbols = c("BTCUSDT", "ETHUSDT", "BNBUSDT", "ADAUSDT", "SOLUSDT")) {
  tryCatch({
    url <- "https://api.binance.com/api/v3/ticker/price"
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      data <- data %>%
        filter(symbol %in% symbols) %>%
        mutate(
          price = as.numeric(price),
          exchange = "Binance",
          timestamp = Sys.time()
        )
      return(data)
    }
  }, error = function(e) {
    warning("Binance API error: ", e$message)
    return(NULL)
  })
}

# Function to fetch prices from Coinbase
get_coinbase_prices <- function(symbols = c("BTC-USD", "ETH-USD", "SOL-USD", "ADA-USD")) {
  tryCatch({
    prices_list <- list()
    
    for (symbol in symbols) {
      url <- paste0("https://api.coinbase.com/v2/prices/", symbol, "/spot")
      response <- GET(url)
      
      if (status_code(response) == 200) {
        data <- fromJSON(content(response, "text", encoding = "UTF-8"))
        prices_list[[symbol]] <- data.frame(
          symbol = symbol,
          price = as.numeric(data$data$amount),
          exchange = "Coinbase",
          timestamp = Sys.time()
        )
      }
    }
    
    if (length(prices_list) > 0) {
      return(bind_rows(prices_list))
    }
  }, error = function(e) {
    warning("Coinbase API error: ", e$message)
    return(NULL)
  })
}

# Function to fetch prices from Kraken
get_kraken_prices <- function(symbols = c("XXBTZUSD", "XETHZUSD", "SOLUSD", "ADAUSD")) {
  tryCatch({
    url <- "https://api.kraken.com/0/public/Ticker"
    response <- GET(url, query = list(pair = paste(symbols, collapse = ",")))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (!is.null(data$result)) {
        prices_list <- list()
        for (sym in names(data$result)) {
          prices_list[[sym]] <- data.frame(
            symbol = sym,
            price = as.numeric(data$result[[sym]]$c[1]),
            exchange = "Kraken",
            timestamp = Sys.time()
          )
        }
        return(bind_rows(prices_list))
      }
    }
  }, error = function(e) {
    warning("Kraken API error: ", e$message)
    return(NULL)
  })
}

# Unified function to collect all exchange data
collect_all_prices <- function() {
  binance_data <- get_binance_prices()
  coinbase_data <- get_coinbase_prices()
  kraken_data <- get_kraken_prices()
  
  all_data <- bind_rows(binance_data, coinbase_data, kraken_data)
  
  # Normalize symbol names for comparison
  all_data <- all_data %>%
    mutate(
      base_symbol = case_when(
        grepl("BTC", symbol, ignore.case = TRUE) ~ "BTC",
        grepl("ETH", symbol, ignore.case = TRUE) ~ "ETH",
        grepl("SOL", symbol, ignore.case = TRUE) ~ "SOL",
        grepl("ADA", symbol, ignore.case = TRUE) ~ "ADA",
        grepl("BNB", symbol, ignore.case = TRUE) ~ "BNB",
        TRUE ~ symbol
      )
    )
  
  return(all_data)
}

# ============================================================================
# 2. ARBITRAGE DETECTION ENGINE
# ============================================================================

detect_arbitrage_opportunities <- function(price_data, min_spread_pct = 0.5) {
  # Calculate price spreads between exchanges for each cryptocurrency
  arbitrage_opps <- price_data %>%
    group_by(base_symbol) %>%
    filter(n() >= 2) %>%  # Need at least 2 exchanges
    summarise(
      exchanges = paste(exchange, collapse = ", "),
      min_price = min(price),
      max_price = max(price),
      min_exchange = exchange[which.min(price)],
      max_exchange = exchange[which.max(price)],
      spread = max_price - min_price,
      spread_pct = ((max_price - min_price) / min_price) * 100,
      avg_price = mean(price),
      timestamp = first(timestamp),
      .groups = "drop"
    ) %>%
    filter(spread_pct >= min_spread_pct) %>%
    arrange(desc(spread_pct))
  
  return(arbitrage_opps)
}

# Calculate trading fees and net profit
calculate_net_arbitrage <- function(arbitrage_data, 
                                    trading_fee_pct = 0.1,
                                    withdrawal_fee_usd = 1) {
  arbitrage_data %>%
    mutate(
      buy_cost = min_price * (1 + trading_fee_pct/100),
      sell_revenue = max_price * (1 - trading_fee_pct/100),
      gross_profit = sell_revenue - buy_cost,
      net_profit = gross_profit - withdrawal_fee_usd,
      net_profit_pct = (net_profit / buy_cost) * 100,
      profitable = net_profit > 0
    )
}

# ============================================================================
# 3. VOLATILITY ANALYSIS
# ============================================================================

analyze_volatility <- function(price_history) {
  # Calculate rolling volatility and returns
  volatility_analysis <- price_history %>%
    group_by(base_symbol, exchange) %>%
    arrange(timestamp) %>%
    mutate(
      returns = (price - lag(price)) / lag(price) * 100,
      rolling_std = zoo::rollapply(returns, width = 10, FUN = sd, 
                                   align = "right", fill = NA, na.rm = TRUE),
      rolling_mean = zoo::rollapply(returns, width = 10, FUN = mean,
                                    align = "right", fill = NA, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(volatility_analysis)
}

# ============================================================================
# 4. VISUALIZATION FUNCTIONS
# ============================================================================

plot_price_comparison <- function(price_data) {
  p <- ggplot(price_data, aes(x = exchange, y = price, fill = exchange)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~base_symbol, scales = "free_y") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Cryptocurrency Prices Across Exchanges",
      subtitle = paste("Data as of:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      x = "Exchange",
      y = "Price (USD)",
      fill = "Exchange"
    ) +
    scale_fill_brewer(palette = "Set2")
  
  return(ggplotly(p))
}

plot_arbitrage_opportunities <- function(arbitrage_data) {
  if (nrow(arbitrage_data) == 0) {
    return(NULL)
  }
  
  p <- ggplot(arbitrage_data, aes(x = reorder(base_symbol, spread_pct), 
                                  y = spread_pct, fill = base_symbol)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sprintf("%.2f%%", spread_pct)), 
              hjust = -0.1, size = 3) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Arbitrage Opportunities by Spread Percentage",
      subtitle = paste("Buy on", arbitrage_data$min_exchange, 
                       "→ Sell on", arbitrage_data$max_exchange),
      x = "Cryptocurrency",
      y = "Price Spread (%)",
      fill = "Crypto"
    ) +
    theme(legend.position = "none")
  
  return(ggplotly(p))
}

plot_correlation_matrix <- function(price_data) {
  # Create wide format for correlation
  price_wide <- price_data %>%
    select(base_symbol, exchange, price) %>%
    pivot_wider(names_from = c(base_symbol, exchange), 
                values_from = price,
                names_sep = "_")
  
  # Calculate correlation matrix
  cor_matrix <- cor(price_wide[, -1], use = "pairwise.complete.obs")
  
  # Plot
  corrplot(cor_matrix, method = "color", type = "upper",
           tl.col = "black", tl.srt = 45,
           title = "Price Correlation Across Crypto & Exchanges",
           mar = c(0,0,2,0))
}

# ============================================================================
# 5. MAIN ANALYSIS EXECUTION
# ============================================================================

run_arbitrage_analysis <- function(min_spread = 0.5) {
  cat("========================================\n")
  cat("CRYPTO ARBITRAGE DETECTION SYSTEM\n")
  cat("========================================\n\n")
  
  # Collect current prices
  cat("📊 Collecting price data from exchanges...\n")
  price_data <- collect_all_prices()
  
  if (is.null(price_data) || nrow(price_data) == 0) {
    cat("❌ No data collected. Please check API connectivity.\n")
    return(NULL)
  }
  
  cat("✅ Collected", nrow(price_data), "price points\n\n")
  
  # Display current prices
  cat("Current Prices:\n")
  print(price_data %>% 
          select(base_symbol, exchange, price) %>%
          arrange(base_symbol, exchange))
  cat("\n")
  
  # Detect arbitrage opportunities
  cat("🔍 Detecting arbitrage opportunities...\n")
  arbitrage_opps <- detect_arbitrage_opportunities(price_data, min_spread)
  
  if (nrow(arbitrage_opps) == 0) {
    cat("📉 No arbitrage opportunities found with spread >", min_spread, "%\n\n")
  } else {
    cat("✅ Found", nrow(arbitrage_opps), "arbitrage opportunities!\n\n")
    
    # Calculate net profit after fees
    arbitrage_with_fees <- calculate_net_arbitrage(arbitrage_opps)
    
    cat("Profitable Arbitrage Opportunities (After Fees):\n")
    print(arbitrage_with_fees %>%
            filter(profitable) %>%
            select(base_symbol, min_exchange, max_exchange, 
                   spread_pct, net_profit_pct) %>%
            arrange(desc(net_profit_pct)))
    cat("\n")
  }
  
  # Return all results
  results <- list(
    price_data = price_data,
    arbitrage_opportunities = arbitrage_opps,
    timestamp = Sys.time()
  )
  
  return(results)
}

# ============================================================================
# 6. EXECUTE ANALYSIS
# ============================================================================

# Run the analysis
cat("Starting Cryptocurrency Arbitrage Analysis...\n\n")
results <- run_arbitrage_analysis(min_spread = 0.3)

# Generate visualizations if data is available
if (!is.null(results)) {
  cat("\n📈 Generating visualizations...\n")
  
  # Price comparison plot
  price_plot <- plot_price_comparison(results$price_data)
  print(price_plot)
  
  # Arbitrage opportunities plot
  if (nrow(results$arbitrage_opportunities) > 0) {
    arb_plot <- plot_arbitrage_opportunities(results$arbitrage_opportunities)
    if (!is.null(arb_plot)) {
      print(arb_plot)
    }
  }
  
  cat("\n✅ Analysis complete!\n")
  cat("========================================\n")
}

# Save results to CSV for further analysis
if (!is.null(results)) {
  write.csv(results$price_data, 
            paste0("crypto_prices_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
            row.names = FALSE)
  
  if (nrow(results$arbitrage_opportunities) > 0) {
    write.csv(results$arbitrage_opportunities,
              paste0("arbitrage_opps_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
              row.names = FALSE)
  }
  
  cat("\n💾 Results saved to CSV files\n")
}


