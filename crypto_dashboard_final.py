"""
Cryptocurrency Arbitrage Detection Dashboard
Real-time monitoring across multiple exchanges
Author: Sairam Aadhithya
"""

import streamlit as st
import pandas as pd
import numpy as np
import requests
import plotly.express as px
import plotly.graph_objects as go
from datetime import datetime
import time

# Page configuration
st.set_page_config(
    page_title="Crypto Arbitrage Monitor",
    page_icon="💰",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Custom CSS - Dark Mode Only
st.markdown("""
    <style>
    .main {
        padding: 0rem 1rem;
        background-color: #0e1117;
        color: #ffffff;
    }
    .stMetric {
        background-color: #1a1a1a;
        padding: 15px;
        border-radius: 10px;
        box-shadow: 2px 2px 5px rgba(0,0,0,0.3);
    }
    [data-testid="metric-container"] {
        background-color: #1a1a1a;
        padding: 15px;
        border-radius: 10px;
    }
    [data-testid="metric-container"] > div:first-child {
        color: #ffffff !important;
        font-size: 14px !important;
    }
    [data-testid="metric-container"] > div:last-child {
        color: #00d4ff !important;
        font-size: 32px !important;
        font-weight: bold !important;
    }
    h1 {
        color: #1f77b4;
        font-weight: bold;
    }
    h2 {
        color: #ffffff;
    }
    .stTabs [data-baseweb="tab-list"] {
        background-color: transparent;
    }
    .stTabs [aria-selected="true"] {
        border-bottom: 3px solid #00d4ff;
    }
    .stDataFrame {
        background-color: #1a1a1a;
    }
    .reportview-container .main .block-container {
        max-width: 1400px;
    }
    </style>
""", unsafe_allow_html=True)

# ============================================================================
# DATA FETCHING FUNCTIONS
# ============================================================================

@st.cache_data(ttl=30)  # Cache for 30 seconds
def fetch_binance_prices(symbols=None):
    """Fetch real-time prices from Binance"""
    if symbols is None:
        symbols = ['BTCUSDT', 'ETHUSDT', 'BNBUSDT', 'SOLUSDT', 'ADAUSDT']
    
    try:
        url = "https://api.binance.com/api/v3/ticker/price"
        response = requests.get(url, timeout=10)
        
        if response.status_code == 200:
            data = response.json()
            df = pd.DataFrame(data)
            df = df[df['symbol'].isin(symbols)]
            df['price'] = df['price'].astype(float)
            df['exchange'] = 'Binance'
            df['timestamp'] = datetime.now()
            return df
        else:
            st.warning(f"Binance API returned status {response.status_code}")
            return None
    except Exception as e:
        st.error(f"Binance API error: {str(e)}")
        return None

@st.cache_data(ttl=30)
def fetch_coinbase_prices(symbols=None):
    """Fetch real-time prices from Coinbase"""
    if symbols is None:
        symbols = ['BTC-USD', 'ETH-USD', 'SOL-USD', 'ADA-USD']
    
    prices_list = []
    
    try:
        for symbol in symbols:
            url = f"https://api.coinbase.com/v2/prices/{symbol}/spot"
            response = requests.get(url, timeout=10)
            
            if response.status_code == 200:
                data = response.json()
                prices_list.append({
                    'symbol': symbol,
                    'price': float(data['data']['amount']),
                    'exchange': 'Coinbase',
                    'timestamp': datetime.now()
                })
            time.sleep(0.2)  # Rate limiting
        
        if prices_list:
            return pd.DataFrame(prices_list)
        else:
            return None
    except Exception as e:
        st.error(f"Coinbase API error: {str(e)}")
        return None

def normalize_symbols(df):
    """Normalize cryptocurrency symbols across exchanges"""
    df['crypto'] = df['symbol'].str.extract(r'(BTC|ETH|BNB|SOL|ADA)')[0]
    return df

def collect_all_prices():
    """Collect prices from all exchanges"""
    with st.spinner('Fetching real-time data from exchanges...'):
        binance_data = fetch_binance_prices()
        coinbase_data = fetch_coinbase_prices()
        
        dataframes = []
        
        if binance_data is not None:
            dataframes.append(binance_data)
        
        if coinbase_data is not None:
            dataframes.append(coinbase_data)
        
        if not dataframes:
            st.error("❌ Could not fetch data from any exchange. Using demo data...")
            return generate_demo_data()
        
        all_data = pd.concat(dataframes, ignore_index=True)
        all_data = normalize_symbols(all_data)
        all_data = all_data[all_data['crypto'].notna()]
        
        return all_data

def generate_demo_data():
    """Generate demo data if APIs fail"""
    np.random.seed(42)
    
    base_prices = {
        'BTC': 94000,
        'ETH': 3500,
        'SOL': 180,
        'ADA': 1.2,
        'BNB': 650
    }
    
    exchanges = ['Binance', 'Coinbase', 'Kraken']
    data = []
    
    for crypto, base_price in base_prices.items():
        for exchange in exchanges:
            bias = {
                'Binance': 0.998,
                'Coinbase': 1.002,
                'Kraken': 1.000
            }[exchange]
            
            price = base_price * bias * np.random.uniform(0.998, 1.002)
            
            data.append({
                'symbol': f"{crypto}USDT",
                'crypto': crypto,
                'price': price,
                'exchange': exchange,
                'timestamp': datetime.now()
            })
    
    return pd.DataFrame(data)

# ============================================================================
# ARBITRAGE DETECTION
# ============================================================================

def detect_arbitrage(price_data, min_spread=0.1, trading_fee=0.1, withdrawal_fee=2.0):
    """Detect arbitrage opportunities"""
    if price_data.empty:
        return pd.DataFrame()
    
    arbitrage_list = []
    
    for crypto in price_data['crypto'].unique():
        crypto_data = price_data[price_data['crypto'] == crypto]
        
        if len(crypto_data) < 2:
            continue
        
        min_price = crypto_data['price'].min()
        max_price = crypto_data['price'].max()
        min_exchange = crypto_data.loc[crypto_data['price'].idxmin(), 'exchange']
        max_exchange = crypto_data.loc[crypto_data['price'].idxmax(), 'exchange']
        
        spread = max_price - min_price
        spread_pct = (spread / min_price) * 100
        
        if spread_pct >= min_spread:
            # Calculate net profit after fees
            buy_cost = min_price * (1 + trading_fee/100)
            sell_revenue = max_price * (1 - trading_fee/100)
            net_profit = sell_revenue - buy_cost - withdrawal_fee
            net_profit_pct = (net_profit / buy_cost) * 100
            
            signal = '🟢 STRONG' if spread_pct >= 1.0 else '🟡 MODERATE' if spread_pct >= 0.5 else '🔴 WEAK'
            
            arbitrage_list.append({
                'Crypto': crypto,
                'Buy From': min_exchange,
                'Sell To': max_exchange,
                'Buy Price': min_price,
                'Sell Price': max_price,
                'Spread $': spread,
                'Spread %': spread_pct,
                'Net Profit': net_profit,
                'ROI %': net_profit_pct,
                'Signal': signal,
                'Profitable': net_profit > 0
            })
    
    if not arbitrage_list:
        return pd.DataFrame()
    
    return pd.DataFrame(arbitrage_list).sort_values('Spread %', ascending=False)

# ============================================================================
# MAIN APP
# ============================================================================

def main():
    # Header
    st.title("💰 Cryptocurrency Arbitrage Detection Dashboard")
    st.markdown("**Real-time monitoring across multiple exchanges**")
    st.markdown("---")
    
    # Sidebar controls
    st.sidebar.header("⚙️ Configuration")
    
    min_spread = st.sidebar.slider(
        "Minimum Spread (%)",
        min_value=0.05,
        max_value=5.0,
        value=0.1,
        step=0.05
    )
    
    trading_fee = st.sidebar.number_input(
        "Trading Fee (%)",
        min_value=0.0,
        max_value=1.0,
        value=0.1,
        step=0.05
    )
    
    withdrawal_fee = st.sidebar.number_input(
        "Withdrawal Fee ($)",
        min_value=0.0,
        max_value=10.0,
        value=2.0,
        step=0.5
    )
    
    auto_refresh = st.sidebar.checkbox("Auto-refresh (30s)", value=True)
    
    if st.sidebar.button("🔄 Refresh Now", use_container_width=True):
        st.cache_data.clear()
        st.rerun()
    
    st.sidebar.markdown("---")
    st.sidebar.info("💡 **Tip:** Lower the minimum spread to see more opportunities")
    
    # Fetch data
    price_data = collect_all_prices()
    
    if price_data.empty:
        st.error("❌ No data available. Please check your internet connection.")
        return
    
    # Detect arbitrage
    arbitrage_opps = detect_arbitrage(price_data, min_spread, trading_fee, withdrawal_fee)
    
    # Calculate metrics - safely handle empty DataFrame
    total_cryptos = price_data['crypto'].nunique()
    total_exchanges = price_data['exchange'].nunique()
    total_opportunities = len(arbitrage_opps)
    
    if not arbitrage_opps.empty:
        max_spread = arbitrage_opps['Spread %'].max()
    else:
        max_spread = 0.0
    
    # Display metrics
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        st.metric(
            label="🪙 Cryptocurrencies",
            value=f"{total_cryptos}"
        )
    
    with col2:
        st.metric(
            label="🏢 Exchanges",
            value=f"{total_exchanges}"
        )
    
    with col3:
        st.metric(
            label="⚡ Opportunities",
            value=f"{total_opportunities}"
        )
    
    with col4:
        st.metric(
            label="📈 Max Spread",
            value=f"{max_spread:.2f}%" if max_spread > 0 else "0.00%"
        )
    
    # System status
    st.success(f"✅ System operational | Last update: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    # Main content tabs
    tab1, tab2, tab3, tab4 = st.tabs([
        "📊 Dashboard",
        "💵 Current Prices",
        "🎯 Arbitrage Opportunities",
        "📈 Analytics"
    ])
    
    # TAB 1: Dashboard
    with tab1:
        st.header("Real-Time Arbitrage Alerts")
        
        if arbitrage_opps.empty:
            st.warning(f"⚠️ No arbitrage opportunities found with spread ≥ {min_spread}%. Try lowering the threshold.")
        else:
            # Display opportunities table
            display_df = arbitrage_opps[[
                "Crypto",
                "Buy From",
                "Sell To",
                "Buy Price",
                "Sell Price",
                "Spread $",
                "Spread %",
                "Net Profit",
                "ROI %",
                "Signal",
                "Profitable"
            ]].reset_index(drop=True)
            
            # Format numeric columns for display
            display_df['Buy Price'] = display_df['Buy Price'].map(lambda x: f"${x:,.2f}")
            display_df['Sell Price'] = display_df['Sell Price'].map(lambda x: f"${x:,.2f}")
            display_df['Spread $'] = display_df['Spread $'].map(lambda x: f"${x:,.2f}")
            display_df['Spread %'] = display_df['Spread %'].map(lambda x: f"{x:.2f}%")
            display_df['Net Profit'] = display_df['Net Profit'].map(lambda x: f"${x:,.2f}")
            display_df['ROI %'] = display_df['ROI %'].map(lambda x: f"{x:.2f}%")
            
            st.dataframe(display_df, use_container_width=True, height=400)
            
            # Spread visualization
            st.subheader("📊 Spread Visualization")
            
            fig = px.bar(
                arbitrage_opps,
                x='Crypto',
                y='Spread %',
                color='Spread %',
                color_continuous_scale='RdYlGn',
                text='Spread %',
                title='Arbitrage Spreads by Cryptocurrency',
                labels={'Spread %': 'Spread Percentage (%)'}
            )
            
            fig.update_traces(texttemplate='%{text:.2f}%', textposition='outside')
            fig.update_layout(height=500, template='plotly_dark')
            st.plotly_chart(fig, use_container_width=True)
    
    # TAB 2: Current Prices
    with tab2:
        st.header("💵 Current Prices Across Exchanges")
        
        price_pivot = price_data.pivot_table(
            index='crypto',
            columns='exchange',
            values='price',
            aggfunc='first'
        )
        
        st.dataframe(price_pivot.style.format("{:.2f}"), use_container_width=True)
    
    # TAB 3: Arbitrage Opportunities
    with tab3:
        st.header("🎯 Detailed Arbitrage Opportunities")
        
        if not arbitrage_opps.empty:
            st.dataframe(arbitrage_opps, use_container_width=True)
        else:
            st.info("No opportunities available with current settings.")
    
    # TAB 4: Analytics
    with tab4:
        st.header("📈 Analytics & Insights")
        
        if not arbitrage_opps.empty:
            col1, col2 = st.columns(2)
            
            with col1:
                fig1 = px.pie(
                    arbitrage_opps,
                    names='Crypto',
                    values='Spread %',
                    title='Spread Distribution by Crypto'
                )
                fig1.update_layout(template='plotly_dark')
                st.plotly_chart(fig1, use_container_width=True)
            
            with col2:
                fig2 = px.bar(
                    arbitrage_opps,
                    x='Crypto',
                    y='Net Profit',
                    color='Net Profit',
                    color_continuous_scale='Viridis',
                    title='Net Profit by Opportunity'
                )
                fig2.update_layout(template='plotly_dark')
                st.plotly_chart(fig2, use_container_width=True)
        else:
            st.info("No analytics available with current settings.")
    
    # Auto-refresh logic
    if auto_refresh:
        time.sleep(30)
        st.rerun()

if __name__ == "__main__":
    main()