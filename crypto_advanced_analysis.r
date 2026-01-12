# Advanced Cryptocurrency Market Analysis
# Volatility Modeling, Correlation Analysis & Predictive Insights
# Author: Sairam Aadhithya

library(tidyverse)
library(lubridate)
library(zoo)
library(tseries)
library(forecast)
library(corrplot)
library(ggplot2)
library(plotly)
library(scales)

# ============================================================================
# 1. SIMULATED HISTORICAL DATA GENERATION
# ============================================================================
# Since we're working with real-time data, let's create functions to 
# collect and analyze historical patterns

generate_sample_historical_data <- function(days = 30, interval_hours = 1) {
  # Generate sample historical price data for demonstration
  # In production, this would be replaced with actual historical data collection
  
  n_points <- days * 24 / interval_hours
  timestamps <- seq(Sys.time() - days*24*3600, Sys.time(), length.out = n_points)
  
  # Simulate price movements with realistic volatility
  set.seed(123)
  
  cryptos <- c("BTC", "ETH", "SOL", "ADA")
  exchanges <- c("Binance", "Coinbase", "Kraken")
  
  data_list <- list()
  
  for (crypto in cryptos) {
    base_price <- case_when(
      crypto == "BTC" ~ 94000,
      crypto == "ETH" ~ 3500,
      crypto == "SOL" ~ 180,
      crypto == "ADA" ~ 1.2
    )
    
    for (exchange in exchanges) {
      # Generate price series with random walk + drift
      returns <- rnorm(n_points, mean = 0.0001, sd = 0.02)
      prices <- base_price * cumprod(1 + returns)
      
      # Add exchange-specific bias
      exchange_bias <- case_when(
        exchange == "Binance" ~ 0.998,
        exchange == "Coinbase" ~ 1.001,
        exchange == "Kraken" ~ 1.000
      )
      
      prices <- prices * exchange_bias
      
      data_list[[paste(crypto, exchange, sep = "_")]] <- data.frame(
        timestamp = timestamps,
        crypto = crypto,
        exchange = exchange,
        price = prices,
        stringsAsFactors = FALSE
      )
    }
  }
  
  historical_data <- bind_rows(data_list)
  return(historical_data)
}

# ============================================================================
# 2. VOLATILITY ANALYSIS
# ============================================================================

calculate_volatility_metrics <- function(price_data) {
  volatility_analysis <- price_data %>%
    arrange(timestamp) %>%
    group_by(crypto, exchange) %>%
    mutate(
      # Calculate returns
      returns = (price - lag(price)) / lag(price),
      log_returns = log(price / lag(price)),
      
      # Rolling volatility (different windows)
      vol_1h = rollapply(returns, width = 24, FUN = sd, 
                        align = "right", fill = NA, na.rm = TRUE),
      vol_6h = rollapply(returns, width = 24*6, FUN = sd,
                        align = "right", fill = NA, na.rm = TRUE),
      vol_24h = rollapply(returns, width = 24*24, FUN = sd,
                         align = "right", fill = NA, na.rm = TRUE),
      
      # Annualized volatility
      annual_vol = vol_24h * sqrt(365),
      
      # Rolling mean
      rolling_mean = rollapply(price, width = 24, FUN = mean,
                              align = "right", fill = NA, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(volatility_analysis)
}

# Calculate realized volatility by period
calculate_realized_volatility <- function(price_data, by_period = "day") {
  realized_vol <- price_data %>%
    mutate(
      period = case_when(
        by_period == "hour" ~ floor_date(timestamp, "hour"),
        by_period == "day" ~ floor_date(timestamp, "day"),
        by_period == "week" ~ floor_date(timestamp, "week")
      )
    ) %>%
    group_by(crypto, exchange, period) %>%
    summarise(
      mean_price = mean(price),
      std_price = sd(price),
      min_price = min(price),
      max_price = max(price),
      price_range = max_price - min_price,
      range_pct = (price_range / mean_price) * 100,
      n_obs = n(),
      .groups = "drop"
    )
  
  return(realized_vol)
}

# ============================================================================
# 3. SPREAD ANALYSIS
# ============================================================================

analyze_spread_dynamics <- function(price_data) {
  spread_analysis <- price_data %>%
    group_by(crypto, timestamp) %>%
    filter(n() >= 2) %>%
    summarise(
      min_price = min(price),
      max_price = max(price),
      mean_price = mean(price),
      spread = max_price - min_price,
      spread_pct = (spread / mean_price) * 100,
      min_exchange = exchange[which.min(price)],
      max_exchange = exchange[which.max(price)],
      .groups = "drop"
    ) %>%
    group_by(crypto) %>%
    mutate(
      # Rolling spread statistics
      avg_spread = rollapply(spread_pct, width = 24, FUN = mean,
                            align = "right", fill = NA, na.rm = TRUE),
      spread_volatility = rollapply(spread_pct, width = 24, FUN = sd,
                                   align = "right", fill = NA, na.rm = TRUE),
      
      # Identify spread regime
      spread_regime = case_when(
        spread_pct > quantile(spread_pct, 0.75, na.rm = TRUE) ~ "High",
        spread_pct > quantile(spread_pct, 0.25, na.rm = TRUE) ~ "Medium",
        TRUE ~ "Low"
      )
    ) %>%
    ungroup()
  
  return(spread_analysis)
}

# ============================================================================
# 4. CORRELATION ANALYSIS
# ============================================================================

calculate_correlation_matrix <- function(price_data) {
  # Pivot data to wide format
  price_wide <- price_data %>%
    select(timestamp, crypto, exchange, price) %>%
    pivot_wider(
      names_from = c(crypto, exchange),
      values_from = price,
      names_sep = "_"
    ) %>%
    select(-timestamp)
  
  # Calculate correlation matrix
  cor_matrix <- cor(price_wide, use = "pairwise.complete.obs")
  
  return(cor_matrix)
}

calculate_rolling_correlation <- function(price_data, crypto1, crypto2, window = 24) {
  # Calculate rolling correlation between two cryptocurrencies
  crypto_pair <- price_data %>%
    filter(crypto %in% c(crypto1, crypto2)) %>%
    select(timestamp, crypto, exchange, price) %>%
    group_by(timestamp, crypto) %>%
    summarise(avg_price = mean(price), .groups = "drop") %>%
    pivot_wider(names_from = crypto, values_from = avg_price)
  
  if (ncol(crypto_pair) < 3) {
    warning("Insufficient data for correlation calculation")
    return(NULL)
  }
  
  crypto_pair <- crypto_pair %>%
    arrange(timestamp) %>%
    mutate(
      rolling_cor = rollapply(
        cbind(.[[2]], .[[3]]),
        width = window,
        FUN = function(x) cor(x[,1], x[,2]),
        by.column = FALSE,
        align = "right",
        fill = NA
      )
    )
  
  return(crypto_pair)
}

# ============================================================================
# 5. ARBITRAGE OPPORTUNITY METRICS
# ============================================================================

calculate_arbitrage_statistics <- function(spread_data) {
  arb_stats <- spread_data %>%
    group_by(crypto) %>%
    summarise(
      # Frequency metrics
      total_observations = n(),
      opportunities_count = sum(spread_pct >= 0.5, na.rm = TRUE),
      opportunity_rate = opportunities_count / total_observations * 100,
      
      # Spread metrics
      mean_spread = mean(spread_pct, na.rm = TRUE),
      median_spread = median(spread_pct, na.rm = TRUE),
      max_spread = max(spread_pct, na.rm = TRUE),
      std_spread = sd(spread_pct, na.rm = TRUE),
      
      # Distribution metrics
      q25_spread = quantile(spread_pct, 0.25, na.rm = TRUE),
      q75_spread = quantile(spread_pct, 0.75, na.rm = TRUE),
      iqr_spread = q75_spread - q25_spread,
      
      # Persistence metrics
      avg_duration = mean(rle(spread_pct >= 0.5)$lengths[rle(spread_pct >= 0.5)$values], 
                         na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    arrange(desc(opportunity_rate))
  
  return(arb_stats)
}

# ============================================================================
# 6. VISUALIZATION FUNCTIONS
# ============================================================================

plot_volatility_comparison <- function(volatility_data) {
  p <- volatility_data %>%
    filter(!is.na(annual_vol)) %>%
    ggplot(aes(x = timestamp, y = annual_vol, color = exchange)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~crypto, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(
      title = "Annualized Volatility Over Time",
      subtitle = "24-hour rolling window",
      x = "Time",
      y = "Annualized Volatility",
      color = "Exchange"
    ) +
    scale_y_continuous(labels = percent_format()) +
    theme(legend.position = "bottom")
  
  return(p)
}

plot_spread_distribution <- function(spread_data) {
  p <- ggplot(spread_data, aes(x = spread_pct, fill = crypto)) +
    geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
    geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
    facet_wrap(~crypto, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(
      title = "Distribution of Price Spreads Across Exchanges",
      subtitle = "Red line indicates 0.5% arbitrage threshold",
      x = "Spread (%)",
      y = "Frequency",
      fill = "Cryptocurrency"
    ) +
    theme(legend.position = "none")
  
  return(p)
}

plot_correlation_heatmap <- function(cor_matrix) {
  corrplot(cor_matrix, 
           method = "color",
           type = "upper",
           order = "hclust",
           tl.col = "black",
           tl.srt = 45,
           tl.cex = 0.7,
           addCoef.col = "black",
           number.cex = 0.5,
           title = "Cross-Exchange Price Correlation Matrix",
           mar = c(0, 0, 2, 0))
}

plot_spread_time_series <- function(spread_data) {
  p <- spread_data %>%
    ggplot(aes(x = timestamp, y = spread_pct, color = crypto)) +
    geom_line(alpha = 0.7) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "darkred") +
    theme_minimal() +
    labs(
      title = "Price Spread Evolution Over Time",
      subtitle = "Dashed lines indicate 0.5% and 1.0% thresholds",
      x = "Time",
      y = "Spread (%)",
      color = "Cryptocurrency"
    ) +
    theme(legend.position = "bottom")
  
  return(ggplotly(p))
}

plot_rolling_correlation <- function(rolling_cor_data, crypto1, crypto2) {
  if (is.null(rolling_cor_data)) {
    return(NULL)
  }
  
  p <- ggplot(rolling_cor_data, aes(x = timestamp, y = rolling_cor)) +
    geom_line(color = "steelblue", size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(
      title = paste("Rolling Correlation:", crypto1, "vs", crypto2),
      subtitle = "24-hour window",
      x = "Time",
      y = "Correlation Coefficient"
    ) +
    ylim(-1, 1)
  
  return(p)
}

# ============================================================================
# 7. MAIN ANALYSIS PIPELINE
# ============================================================================

run_advanced_analysis <- function(historical_data = NULL) {
  cat("==========================================\n")
  cat("ADVANCED CRYPTOCURRENCY MARKET ANALYSIS\n")
  cat("==========================================\n\n")
  
  # Generate or load historical data
  if (is.null(historical_data)) {
    cat("📊 Generating sample historical data (30 days)...\n")
    historical_data <- generate_sample_historical_data(days = 30)
  }
  
  cat("✅ Loaded", nrow(historical_data), "historical data points\n\n")
  
  # 1. Volatility Analysis
  cat("📈 Calculating volatility metrics...\n")
  volatility_data <- calculate_volatility_metrics(historical_data)
  realized_vol <- calculate_realized_volatility(historical_data, "day")
  
  cat("✅ Volatility analysis complete\n")
  cat("   - Average annualized volatility:", 
      round(mean(volatility_data$annual_vol, na.rm = TRUE) * 100, 2), "%\n\n")
  
  # 2. Spread Analysis
  cat("💹 Analyzing spread dynamics...\n")
  spread_data <- analyze_spread_dynamics(historical_data)
  
  cat("✅ Spread analysis complete\n")
  cat("   - Average spread:", 
      round(mean(spread_data$spread_pct, na.rm = TRUE), 3), "%\n")
  cat("   - Max spread observed:", 
      round(max(spread_data$spread_pct, na.rm = TRUE), 3), "%\n\n")
  
  # 3. Arbitrage Statistics
  cat("🎯 Calculating arbitrage opportunity metrics...\n")
  arb_stats <- calculate_arbitrage_statistics(spread_data)
  
  cat("✅ Arbitrage statistics:\n")
  print(arb_stats %>% 
          select(crypto, opportunity_rate, mean_spread, max_spread) %>%
          mutate(across(where(is.numeric), ~round(., 2))))
  cat("\n")
  
  # 4. Correlation Analysis
  cat("🔗 Computing correlation matrices...\n")
  cor_matrix <- calculate_correlation_matrix(historical_data)
  
  cat("✅ Correlation analysis complete\n\n")
  
  # 5. Generate Visualizations
  cat("📊 Generating visualizations...\n\n")
  
  # Volatility plot
  vol_plot <- plot_volatility_comparison(volatility_data)
  print(vol_plot)
  
  # Spread distribution
  spread_dist_plot <- plot_spread_distribution(spread_data)
  print(spread_dist_plot)
  
  # Correlation heatmap
  plot_correlation_heatmap(cor_matrix)
  
  # Spread time series
  spread_ts_plot <- plot_spread_time_series(spread_data)
  print(spread_ts_plot)
  
  # Rolling correlation (BTC vs ETH)
  rolling_cor <- calculate_rolling_correlation(historical_data, "BTC", "ETH")
  if (!is.null(rolling_cor)) {
    cor_plot <- plot_rolling_correlation(rolling_cor, "BTC", "ETH")
    print(cor_plot)
  }
  
  cat("\n✅ All visualizations generated\n\n")
  
  # 6. Summary Report
  cat("==========================================\n")
  cat("SUMMARY REPORT\n")
  cat("==========================================\n\n")
  
  cat("🔹 Data Coverage:\n")
  cat("   - Period:", 
      format(min(historical_data$timestamp), "%Y-%m-%d"), "to",
      format(max(historical_data$timestamp), "%Y-%m-%d"), "\n")
  cat("   - Cryptocurrencies:", 
      length(unique(historical_data$crypto)), "\n")
  cat("   - Exchanges:", 
      length(unique(historical_data$exchange)), "\n\n")
  
  cat("🔹 Top Arbitrage Opportunities:\n")
  top_crypto <- arb_stats %>% 
    slice_max(opportunity_rate, n = 1)
  cat("   - Best asset:", top_crypto$crypto, "\n")
  cat("   - Opportunity rate:", round(top_crypto$opportunity_rate, 2), "%\n")
  cat("   - Average spread:", round(top_crypto$mean_spread, 3), "%\n\n")
  
  cat("🔹 Market Insights:\n")
  high_vol_crypto <- volatility_data %>%
    group_by(crypto) %>%
    summarise(avg_vol = mean(annual_vol, na.rm = TRUE)) %>%
    slice_max(avg_vol, n = 1)
  cat("   - Most volatile:", high_vol_crypto$crypto, 
      "(", round(high_vol_crypto$avg_vol * 100, 1), "% annualized)\n")
  
  # Return analysis results
  results <- list(
    volatility_data = volatility_data,
    spread_data = spread_data,
    arbitrage_stats = arb_stats,
    correlation_matrix = cor_matrix,
    realized_volatility = realized_vol
  )
  
  return(results)
}

# ============================================================================
# 8. EXECUTE ANALYSIS
# ============================================================================

cat("Initializing Advanced Cryptocurrency Market Analysis...\n\n")

# Run the complete analysis
analysis_results <- run_advanced_analysis()

# Save results
cat("\n💾 Saving analysis results...\n")
saveRDS(analysis_results, 
        paste0("analysis_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))

cat("✅ Analysis complete! Results saved.\n")
cat("==========================================\n")