# back testing ------
# [Backtesting Strategies with R](https://timtrice.github.io/backtesting-strategies/)
# 

# - quantstrat 0.9.1739
# - blotter 0.9.1741

# - quantmod 0.4-5
# - TTR 0.23-1
# - FinancialInstrument
# - PerformanceAnalytics

# Settings & Variables
# Sys.setenv(TZ = "UTC")
# currency('USD')
# init_date <- "2007-12-31"
# start_date <- "2008-01-01"
# end_date <- "2009-12-31"
# init_equity <- 1e4 # $10,000
# adjustment <- TRUE


# Terminology ------
# - BTO: Buy to Open (open long positions)
# - BTC: Buy to close (close short positions)
# - SL: Stop-limit order
# - STO: Sell to open (open short positions)
# - STC: Sell to close (close long positions)
# - TS: Trailing-stop order


# ref: 
# - https://timtrice.github.io/backtesting-strategies/index.html
# - http://www.r-programming.org/home


# strategy setup: symbols, currency, multiplier, initDate, initEq
# - portfolio, 
# - account 
# - orders

# indicator

# signal
# - sigComparison
# - sigCrossover
# - sigFormula
# - sigPeak
# - sigThreshold
# - sigTimestamp

# rule


pq_backtest = function(
    type = 'long', # 'short'
    
    stop_limit
) {
    
}