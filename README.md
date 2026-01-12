#🚀 Crypto Arbitrage & Market Intelligence Platform

Real-time arbitrage detection, volatility modeling, correlation analytics, and interactive dashboards for cryptocurrency markets.

Built using R (quantitative analysis) and Python (Streamlit dashboard), this project provides a full-stack crypto trading intelligence system: from raw price ingestion to arbitrage detection, risk metrics, and live visualization.

📌 What This Project Does

This repository combines three powerful components:

Layer	File	Purpose
📊 Quant Research Engine	crypto_advanced_analysis.r - Volatility modeling, rolling correlations, spread dynamics, arbitrage statistics
⚡ Live Arbitrage Scanner	crypto_arbitrage.r - 	Pulls real-time prices from Binance, Coinbase & Kraken and finds profitable arbitrage
💻 Trading Dashboard	crypto_dashboard_final.py	- Interactive Streamlit web app for monitoring arbitrage in real time

Together, they form a professional-grade crypto market intelligence system.

🧠 Core Capabilities
🔹 1. Real-Time Exchange Price Feeds

Prices are fetched live from:

Binance
Coinbase
Kraken

Symbols supported:

BTC, ETH, SOL, ADA, BNB

All symbols are normalized for cross-exchange comparison 


🔹 2. Arbitrage Detection Engine

For each cryptocurrency:

Finds cheapest exchange
Finds most expensive exchange

Computes:

Dollar spread
Percentage spread
Trading fees
Withdrawal costs
Net profit & ROI
Opportunities are filtered using a configurable spread threshold (default ≈ 0.3% – 0.5%).

A trade is only marked profitable if:

Sell Revenue − Buy Cost − Fees − Withdrawal Fee > 0



🔹 3. Advanced Quantitative Analytics

The R research engine builds a full statistical picture of the crypto market:

Volatility
Rolling 1-hour, 6-hour, 24-hour volatility
Annualized volatility
Realized daily / weekly volatility
Spread Dynamics
Cross-exchange price spreads
Spread regimes (Low / Medium / High)
Spread persistence
Correlations
Cross-exchange price correlations
Rolling BTC–ETH correlation
Heatmap clustering
Arbitrage Statistics
Opportunity frequency
Spread distribution
Max & median spreads
Opportunity duration


🔹 4. Professional Trading Dashboard

The Streamlit app (crypto_dashboard_final.py) provides:

Live KPI Panel
Number of cryptocurrencies
Number of exchanges
Arbitrage opportunities
Maximum spread detected

Interactive Tabs
Tab	What you see
📊 Dashboard	Live arbitrage table + spread charts
💵 Prices	Exchange-wise live prices
🎯 Opportunities	Full arbitrage breakdown
📈 Analytics	ROI, profit & distribution charts

Auto-refresh runs every 30 seconds, giving you a live crypto trading terminal 


🛠 How to Run This Project

1️⃣ Clone the repository
git clone https://github.com/sairam-aadhithya/crypto-arbitrage-detection-and-analysis-engine/
cd crypto-arbitrage-detection-and-analysis-engine

2️⃣ Run the Quant Research (R)

Install required libraries in R:

install.packages(c("tidyverse","lubridate","zoo","forecast","corrplot","plotly","scales"))


Run:

source("crypto_advanced_analysis.r")


This will:

Generate or load historical data
Run volatility, spread & correlation analysis
Save results to .rds files
Render multiple quantitative plots


3️⃣ Run Live Arbitrage Scanner (R)
source("crypto_arbitrage.r")


This:

Pulls real-time prices from all exchanges
Detects arbitrage
Saves price & opportunity CSVs

4️⃣ Launch the Dashboard (Python)

Install dependencies:

pip install streamlit pandas numpy requests plotly


Run:

streamlit run crypto_dashboard_final.py


Open browser →
http://localhost:8501

🧪 When APIs Fail

If Binance or Coinbase APIs are unavailable, the system automatically switches to realistic demo market data so the dashboard and arbitrage logic keep working 

.

👤 Author

Sairam Aadhithya
IIT Madras BS in Data Science
IIFT Business Analytics
