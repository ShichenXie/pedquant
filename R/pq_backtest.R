# back testing ------
# [Backtesting Strategies with R](https://timtrice.github.io/backtesting-strategies/)
# https://www.google.com/url?q=https://docs.google.com/presentation/d/1fGzDc-LFfCQJKHHzaonspuX1_TTm1EB5hlvCEDsz7zw/pub?&sa=D&ust=1553613215848000&usg=AFQjCNGxM3xCdMK4Lao1yUo_9lblAqFqEA

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
# - STC: Sell to close (close long positions)
# - STO: Sell to open (open short positions)
# - BTC: Buy to close (close short positions)
# - SL: Stop-limit order
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
## - gt greater than
## - lt less than
## - eq equal to
## - gte greater than or equal to
## - lte less than or equal to
# - sigCrossover
# - sigFormula
# - sigPeak
# - sigThreshold
# - sigTimestamp

# greater than
gt  = function(a, b) setDT(list(a=a,b=b))[, a >  b]
# less than
lt  = function(a, b) setDT(list(a=a,b=b))[, a <  b]
# equal than
eq  = function(a, b) setDT(list(a=a,b=b))[, a == b]
# greater than or equal to
gte = function(a, b) setDT(list(a=a,b=b))[, a >= b]
# less than or equal to
lte = function(a, b) setDT(list(a=a,b=b))[, a <= b]
# crossover
co  = function(a, b) setDT(list(a=a,b=b))[, a>b & shift(b>=a,type='lag')]

# rule type: risk, order, rebalance, exit, enter, chain
# order type: limit, stoplimit, stoptrailing, market, iceberg


# Strategy backtesting
# 
# \code{pq_backtest} provides a simple way to backtest a trade strategy.
# 
# @param dt a list/dataframe of time series dataset
# @param addti list of technical indicators or numerical columes in dt. For technical indicator, it is calculated via \code{pq_addti}, which including overlay and oscillator indicators.
# @param init_equity initial equity
# @param date_range date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
# @param from the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.
# @param to the end date. Default is the current date.
# @param rule rules of trade strategy.
# @param stp_lmt_pct stop limit percent
# @param show_plot show plot
# @param ... Additional parameters
# 
# @export
pq_backtest = function(
    dt,
    addti = NULL, 
    init_equity = NULL,
    date_range = 'max',
    from = NULL,
    to = Sys.Date(),
    rule = list(
        long = list(
            enter = NULL,
            exit  = NULL,
            price = NULL,
            stop_limit = NULL,
            position = NULL
        )
    ),
    stp_lmt_pct = 0.05,
    show_plot = TRUE, ... 
) {
    . = symbol = price = type = ssec = value = NULL
    
    if (inherits(dt, 'list')) dt = rbindlist(dt)
    dt = setDT(dt)
    
    
    dat = dt
    if (!is.null(addti) & inherits(addti, 'list')) dat = do.call(pq_addti, args = c(list(dt=dat), addti))
    if (inherits(dat, 'list')) dat = rbindlist(dat)
    
    if ('long' %in% names(rule)) {
        cat('[INFO] backtesting ... \n')
        
        dat = dat[, bto := eval(parse(text = rule$long$enter))
          ][, stc := eval(parse(text = rule$long$exit))
          ][, stp := eval(parse(text = rule$long$stop_limit))]
        
        position = rule$long$position
        
        
        w_long = setDT(list(long_short='0',type='0',symbol='0',date=as.Date('1-1-1-1'),price=0,position=0))[.0]
        bto = NULL
        stc = NULL
        stp = NULL
        stplmt = NULL
        
        for (i in dat[,.I]) {
            if (!is.null(bto) && bto[,.N==1]) {
                w_long = rbind(w_long, dat[i, .(long_short='long', type='bto', symbol, date, price=open, position = position)])
                stplmt = dat[i,open*(1-stp_lmt_pct)]
            }
            if (((!is.null(stc) && stc[,.N==1]) 
                 || (!is.null(stp) && stp[,.N==1]) 
                 || (dat[i, open < stplmt])) 
                && any(w_summary(w_long)[,position>0])) w_long = rbind(w_long, dat[i, .(long_short='long', type='stc', symbol, date, price=open, position = w_summary(w_long)[, -position])])
            
            
            bto = dat[i][which(bto)]
            stc = dat[i][which(stc)]
            stp = dat[i][which(stp)]
        }
        
        w = copy(w_long)
    }
    
    if (show_plot) {
        w2 = w[,.(date, w_price=price, w_position=position, w_type=type)]
        perf = pq_perf(pq_portfolio(dt, w, init_equity = init_equity))[['equity']][,.(date, performance=value)]
        
        addti_lst = list(w=list(), performance=list())
        show_ti = list(...)$show_ti
        if (is.null(show_ti)) show_ti = FALSE
        if (show_ti) addti_lst = c(addti_lst, addti)
        
        p = pq_plot(
            dt = Reduce(function(x,y) merge(x,y, all=TRUE, by='date'), list(dt, w2, perf)),
            addti = addti_lst, date_range = 'max')
        print(p)
    }
    return(w)
}


