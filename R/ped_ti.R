# Technical Indicators and Overlays

# # technial indicator ------
# - Support & resistance
# - Trend 
# - Momentum 
# - Volume 
# - Volatility 
# - Breadth	
# - Other


# # leading: 
# - Commodity Channel Index (CCI), 
# - Momentum, 
# - Relative Strength Index (RSI), 
# - Stochastic Oscillator 
# - Williams %R.
# # lagging: 
# - moving averages (exponential, simple, weighted, variable)
# - MACD


# # reference
# # https://en.wikipedia.org/wiki/Technical_analysis
# # https://stockcharts.com/school/doku.php?id=chart_school:technical_indicators



###### Trend ######
# moving max and min, mm
ti_mm = function(dat, n=20, m=NULL, y = "close|value") { 
    # num of min
    if (is.null(m)) m = n
    
    mmax = function(p, n) {
        price = NULL
        
        data.table(price = p)[
            , shift(price, n=0:(n-1), fill=NA, type="lag")
            ][, apply(.SD, 1, max)]
    }
    mmin = function(p, n) {
        price = NULL
        
        data.table(price = p)[
            , shift(price, n=0:(n-1), fill=NA, type="lag")
            ][, apply(.SD, 1, min)]
    }
    
    setkeyv(dat, "date")
    y = names(dat)[grepl(y, names(dat))][1]
    
    dat[, (paste0("mm_max_",n)) := lapply(.SD, mmax, n=n), .SDcols = y
      ][, (paste0("mm_min_",n)) := lapply(.SD, mmin, n=m), .SDcols = y]
    
    return(dat)
}

# Simple Moving Average, SMA
# Daily Closing Prices: 11,12,13,14,15,16,17
# First day of 5-day SMA: (11 + 12 + 13 + 14 + 15) / 5 = 13
# Second day of 5-day SMA: (12 + 13 + 14 + 15 + 16) / 5 = 14
# Third day of 5-day SMA: (13 + 14 + 15 + 16 + 17) / 5 = 15
sma = function(p, n = 20) {
    price = NULL
    
    data.table(price = p)[
        , shift(price, n=0:(n-1), fill=NA, type="lag")
        ][, rowMeans(.SD)]
}
ti_sma = function(dat, n = 20, y = "close|value") {
    setkeyv(dat, "date")
    y = names(dat)[grepl(y, names(dat))][1]
    sma_name = paste0("sma_", n)
    
    dat[, (sma_name) := lapply(.SD, sma, n = n), .SDcols = y]
    return(dat)
}

# Exponential Moving Average, EMA
# Initial SMA: 10-period sum / 10 
# Multiplier: (2 / (Time periods + 1) ) = (2 / (10 + 1) )
# EMA: {Close - EMA(previous day)} x multiplier + EMA(previous day). 
ema = function(p, n = 20) {
    ema_n = price = NULL
    
    k = 2/(n+1)
    dt = data.table(price = p)
    dt[n, ema_n := dt[1:n, mean(price)]]
    
    for (r in dt[,.I][-(1:n)]) {
        dt[r, ema_n := k*price + (1-k)*dt[r-1, ema_n]]
    }

    # cumsum(p*k/(1-k)^(1:length(p)))*(1-k)^(1:length(p))
    return(dt[, ema_n])
}
ti_ema = function(dat, n = 20, y = "close|value") {
    setkeyv(dat, "date")
    y = names(dat)[grepl(y, names(dat))][1]
    ema_name = paste0("ema_", n)

    dat[, (ema_name) := lapply(.SD, ema, n=n), .SDcols = y]
    return(dat)
}

# Smoothed Moving Average
# The first value of this smoothed moving average is calculated as the simple moving average (SMA): SUM.1 = SUM(CLOSE, n) SMMA.1 = SUM.1 / n 
# The second and succeeding moving averages are calculated according to this formula: SMMA = (SUM.1 - SMMA.1+CLOSE) / n
smma = function(p, n = 14) {
  price = NULL
  
  ddtt = data.table(price = p)
  ddtt[n, smma := ddtt[1:n, sum(price, na.rm=TRUE)]/n]
  
  for (r in ddtt[,.I][-(1:n)]) {
    ddtt[r, smma := (ddtt[r-1, smma]*(n-1) + price)/n]
  }
  return(ddtt[, smma])
}
ti_smma = function(dat, n = 14, y = "close|value") {
    setkeyv(dat, "date")
    y = names(dat)[grepl(y, names(dat))][1]
    smma_name = paste0("smma_", n)
    
    dat[, (smma_name) := lapply(.SD, smma, n=n), .SDcols = y]
    return(dat)
}

# Bollinger Bands, BB
# Middle Band = 20-day simple moving average (SMA)
# Upper Band = 20-day SMA + (20-day standard deviation of price x 2) 
# Lower Band = 20-day SMA - (20-day standard deviation of price x 2)
#' @importFrom stats sd
ti_bb = function(dat, n=26, m=2, y="close|value") {
    boll = NULL
    bb = function(p, n=26, m=2) {
        price = NULL
        
        sma_n = sma(p, n)
        sd_lastn = data.table(price = p)[, shift(price, n=0:(n-1), fill=NA, type="lag")][, apply(.SD, 1, function(x) sd(x, na.rm = TRUE))]
        
        upper_band = sma_n + m*sd_lastn
        lower_band = sma_n - m*sd_lastn
        
        rt = data.table(sma_n, upper_band, lower_band)
        setnames(rt, paste0("bb_",c(paste0("sma_", n), "upper", "lower")) )
        return(rt)
    }
    
    setkeyv(dat, "date")
    y = names(dat)[grepl(y, names(dat))][1]
    
    dat = cbind(dat, boll(dat[[y]], n=n, m=m))
    return(dat)
}

# Stop-And-Reversal, SAR
# https://blog.csdn.net/xmuecor/article/details/78383352
# sar = sar_1 + af*(ep - sar_1)
ti_sar = function(dat, n=4, step = 0.02, max_step = 0.2) {
    . = high = low = updown = sar = NULL
    # rising: SAR < CL AND SAR.1 >= CL.1
    # falling: SAR > CL AND SAR.1 <= CL.1

    # extreme point (ep), highest high of the current uptrend, or lowest low of the current downtrend
    # Acceleration Factor (af), starting at 0.02, increases by .02 each time the ep makes a new high or low
    setkeyv(dat, "date")
    dt = dat[,.(date, open, high, low, close)]

    # initial sar
    initial_sar = function(dt, i) {
        updown = af = low = high = NULL
        
        if (i == n+1) dt[i, updown := ifelse(dt[i-1, close] > dt[i-n, close], "up", "down")]
        updown0 = dt[i, updown]
        
        dt[i, af := 2L]
        if (updown0=="up") {
            dt[i, `:=`(sar = dt[(i-n):(i-1), min(low)],
                       ep = high#dt[(i-n+1):i, max(high)] 
                       )]
        } else {
            dt[i, `:=`(sar = dt[(i-n):(i-1), max(high)],
                       ep = low#dt[(i-n+1):i, min(low)] 
                       )]
        }
        
        return(dt)
    }
    # update sar
    update_sar = function(dt, i) {
        ep = af = updown = sar = high = low = NULL
        
        ep_1 = dt[i-1, ep]
        af_1 = dt[i-1, af]
        updown0 = dt[i, updown]
    
        dt[i, sar := dt[i-1, sar+af/100*(ep-sar)]]
        if (updown0 == "up") {
            dt[i, ep := max(high, ep_1)#dt[(i-n+1):i, max(high)] 
             ][i, af := ifelse(ep>ep_1 & af_1+2L<20L, af_1+2, af_1)]
        } else {
            dt[i, ep := min(low, ep_1)#dt[(i-n+1):i, min(low)] 
             ][i, af := ifelse(ep<ep_1 & af_1+2L<20L, af_1+2, af_1)]
        }
        
        return(dt)
    }
    
    
    # sar in the following rows
    for (i in dt[,.I][-(1:n)]) {
        if (i == n+1) {
            dt = initial_sar(dt, i)
        } else if (dt[i, updown] != dt[i-1, updown]) {
            dt = initial_sar(dt, i)
        } else if (dt[i, updown] == dt[i-1, updown]) {
            dt = update_sar(dt, i)
        }
        
        if (i < dt[,.N]) {
            if (dt[i, updown] == "up") {
                if (dt[i, sar>low]) {
                    dt[i+1, updown := "down"]
                    dt[i, sar := low]
                } else {
                    dt[i+1, updown := "up"]
                }
                # dt[i+1, updown := dt[i, ifelse(sar > low, "down", "up")]] 
            } else {
                if (dt[i, sar<high]) {
                    dt[i+1, updown := "up"]
                    dt[i, sar := high]
                } else {
                    dt[i+1, updown := "down"]
                }
                # dt[i+1, updown := dt[i, ifelse(sar < high, "up", "down")]] 
            }
        }
        
    }
    
    return(dat[,sar := dt[,sar]])
}




###### Momentum/Oscillators ######

# # Centered Oscillators ------
# direction of price momentum
# - momentum is positive (bullish) when a centered oscillator is trading above its center line 
# - and negative (bearish) when the oscillator is trading below its center line

# Moving Average Convergence Divergence, MACD
# MACD Line: (12-day EMA - 26-day EMA)
# Signal Line: 9-day EMA of MACD Line
# MACD Histogram: MACD Line - Signal Line
ti_macd = function(dat, n = 12, n1 = 26, m = 9, y="close") {
    macd = macd_signal = macd_hist = NULL
  
    setkeyv(dat, "date")
    y = names(dat)[grepl(y, names(dat))][1]
    
    dat = dat[, macd := ema(close, n) - ema(close, n1)
            ][, macd_signal := ema(macd, m)
            ][, macd_hist := macd - macd_signal]
    
    return(dat)
}

# Rate of Change, ROC
ti_roc = function(dat, n = 20, y="close|value") {
    prev_close = change_pct = . = roc1 = lag_price = price = NULL
  
    setkeyv(dat, "date")
    y = names(dat)[grepl(y, names(dat))][1]
    
    if ("change_pct" %in% names(dat)) {
        rocn = dat[, prev_close := shift(close, 1, type="lag")
          ][is.na(change_pct), change_pct := (close/prev_close-1)*100
          ][, .(roc1 = change_pct/100+1)
          ][, shift(roc1, n=0:(n-1), fill=NA, type="lag")
          ][, apply(.SD, 1, function(x) (Reduce(prod, x))-1)*100]
        
    } else {
        dt = dat[, y, with=FALSE]
        setnames(dt, "price")
        
        rocn = dt[, lag_price := shift(price, n, type="lag")
         ][, (price-lag_price)/lag_price*100]
    }
    
    return(dat[, (paste0("roc_",n)) := rocn])
}


# Banded Oscillators ------ 
# oversold, overbought

# Percentage Price Oscillator, PPO
ti_ppo = function(dat, n = 12, n1 = 26, m = 9, y="close") {
    ppo = ema_n = ema_n1 = ppo_signal = ppo_hist = NULL
  
    setkeyv(dat, "date")
    
    dat = dat[,`:=`(
        ema_n = ema(close, n), ema_n1 = ema(close, n1)
    )][, ppo := (ema_n - ema_n1)/ema_n1*100
     ][!is.na(ppo), ppo_signal := ema(ppo, m)
     ][, ppo_hist := ppo - ppo_signal
     ][, (c("ema_n", "ema_n1")) := NULL]
    
    return(dat)
}


# Relative Strength Index, RSI
# RSI = 100 - 100/(1+RS)
# RS = Average Gain / Average Loss
ti_rsi = function(dat, n=14, y="close|value") {
    rsi = function(p, n=14) {
        price_prev = price = avgG = avgL = NULL
        
        data.table(price = p)[
            , price_prev := shift(price, 1L, type="lag")
            ][price == price_prev, `:=`(G = 0, L = 0)
            ][price >  price_prev, `:=`(G = price-price_prev, L = 0)
            ][price <  price_prev, `:=`(G = 0, L = price_prev-price)
            ][, (c("avgG", "avgL")) := lapply(.SD, smma, n=n), .SDcols = c("G", "L")
            ][, 100 - 100/(1 + avgG/avgL)]
    }
    
    setkeyv(dat, "date")
    y = names(dat)[grepl(y, names(dat))][1]
    rsi_name = paste0("rsi_", n)
    
    dat[, (rsi_name) := lapply(.SD, rsi, n=n), .SDcols = y]
    return(dat)
}


# Commodity Channel Index, CCI
# CCI = (Typical Price  -  20-period SMA of TP) / (.015 x Mean Deviation)
# Typical Price (TP) = (High + Low + Close)/3
ti_cci = function(dat, n = 14) {
    tp = high = low = NULL
    cci = function(p, n = 14) {
        price = sman = md = NULL
      
        sma_n = sma(p, n)
        
        mean_devi = data.table(price = p)[
            , shift(price, n=0:(n-1), fill=NA, type="lag")
            ][, lapply(.SD, function(x) sma_n-x)
              ][, rowMeans(abs(.SD))]
        
        data.table(
            price = p, md = mean_devi
        )[, sman := sma_n
          ][, (price-sman)/(0.015*md)]
    }
    
    setkeyv(dat, "date")
    sma_name = paste0("cci_",n)
    
    dat[, tp := (high + low + close)/3
      ][, (sma_name) := cci(tp, n)][, tp := NULL]
    
    return(dat)
}

# - KDJ
# - WRI
# - BIAS
# - DMI





###### Volume ######
# Accumulation/distribution line Ease of movement (EMV), 
# Force index (FI), 
# Negative volume index (NVI), 
# On-balance volume (OBV), 
# Put/call ratio (PCR), 
# Volume–price trend (VPT)

# - OBV

###### Volatility ######
# Average true range (ATR), 
# Bollinger Bands (BB), 
# Donchian channel, 
# Keltner channel, 
# CBOE Market Volatility Index (VIX), 
# Standard deviation (σ)

# Average True Range, ATR
ti_atr = function(dat, n=26) {
    prev_close = high = low = TR = ATR = NULL
  
    setkeyv(dat, "date")
    sma_name = paste0("cci_",n)
    
    dat[, prev_close := shift(close, 1, type="lag")
      ][, `:=`(
          h_l = high - low, 
          h_cp = abs(high - prev_close),
          l_cp = abs(low - prev_close)
      )][, TR := do.call(max, list(.SD, na.rm=TRUE)), by="date", .SDcols = c("h_l", "h_cp", "l_cp")
       ][, ATR := sma(TR, n)
       ][, (c("h_l", "h_cp", "l_cp")) := NULL]
    
    return(dat)
}



ped1_addti = function(dat, ti=list(sma=20, sma=50)) {
  for (i in 1:length(ti)) {
    arg = list(dat)
    if (!is.null(ti[[i]])) {
      if (!is.list(ti[[i]])) ti_i = list(ti[[i]])
      arg = c(arg, ti_i)
    }
    
    func = paste0("ti_",names(ti[i]))
    do.call(func, arg)
  }
  
  return(dat)
}
#' create technical indicators
#' 
#' `ped_addti` adds technical indicators to a dataset
#' 
#' @param dt time series datasets
#' @param ti list of technical indicators, overlay indicators include mm, sma, ema, smma, bb, sar, and oscillators indicators such as macd, ppo, roc, rsi, cci. 
#' @param print_step A non-negative integer, which will print symbol name by each print_step iteration. Default is 1. 
#' 
#' @examples 
#' dt = getmd("^000001", source=163)
#' 
#' dt_ti = ped_addti(dt, ti=list(sma=20, sma=50))
#' 
#' @export
ped_addti = function(dt, ti=list(sma=20, sma=50), print_step=1L) {
  dt_list = list()
  
  if (is.list(dt) & !is.data.frame(dt)) {
    dt = lapply(dt, setDT)
    dt_names = names(dt)
    len_names = length(dt_names)
    
    for (i in 1:len_names) {
      if ((print_step>0) & (i %% print_step == 0)) cat(paste0(format(c(i,len_names)),collapse = "/"), dt_names[i],"\n")
      
      dt_list[[i]] = do.call(ped1_addti, args = list(dat=dt[[i]], ti=ti))
    }
    
  } else if (is.data.frame(dt)) {
    setDT(dt)
    i = 1
    dt_list[[i]] =do.call(ped1_addti, args = list(dat=dt, ti=ti))
    
  }
  
  return(dt_list)
}

# main indicators ------
# 黑色：焦炭、螺纹
# 农产：棉花、豆粕
# IC, IF, IH, 
# 50ETF, 沪深300ETF, 中证500, 创业板指数


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


