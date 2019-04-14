# Technical,Indicators and Overlays

### technial indicator
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

# library(RcppRoll)
# # rolling max, mean, median, min, prod, sd, sum, var
# run_max = function(x, n = 10, ...) {
#   return(roll_max(x, n = n, align = "right", fill = NA, ...))
# }

bias = function(x, n=10, maType='SMA') {
  bias_orig = (x/do.call(maType, list(x=x, n=n))-1)*100
  
  bias_orig/runSD(bias_orig, n = 1, cumulative = TRUE)
}

maroc = function(x, n=10, m=3, maType='SMA') {
  ROC(do.call(maType, list(x=x, n=n)), n=m)*100
}

# [1] "adjRatios" "growth" "lags" "rollSFM" "runPercentRank"  
# Technical Overlays / Indicators
ti_overlays_indicators = function() {
  list(
    overlays = c('SMA', 'EMA', 'DEMA', 'WMA', 'EVWMA', 'ZLEMA', 'VWAP', 'VMA', 'HMA', 'ALMA', 
                 'runMin', 'runMax', 'runMean', 'runMedian', 
                 'BBands', 'PBands', 
                 'DonchianChannel', 'SAR', 'ZigZag'),
    indicators = c('runSD', 'runMAD', 'aroon', 'CCI', 'VHF', 'TDI', 'ADX', 'ATR', 'EMV', 'chaikinVolatility', 'volatility', 'OBV', 'chaikinAD', 'CLV', 'CMF', 'MFI', 'williamsAD', 'ROC', 'momentum', 'KST', 'TRIX', 'MACD', 'DPO', 'DVI', 'ultimateOscillator', 'RSI', 'CMO', 'stoch', 'SMI', 'WPR', 'bias', 'maroc')
  )
}

# hline for technical indicators in pq_plot
ti_idicators_hline = function() {list(
  bbands = c(0.5, 0, 1),
  aroon  = c(50),
  cci    = c(0, -100, 100),
  tdi    = c(0),
  emv    = c(0),
  chaikinvolatility = c(0),
  clv    = c(0),
  cmf    = c(0),
  mfi    = c(20, 80),
  roc    = c(0),
  momentum = c(0),
  kst    = c(0),
  trix   = c(0),
  macd   = c(0),
  dpo    = c(0),
  dvi    = c(0.5),
  ultimateoscillator = c(50, 30, 70),
  rsi    = c(50, 30, 70),
  cmo    = c(0),
  stoch  = c(0.5),
  smi    = c(0),
  wpr    = c(0.5)
)}



# number of columns returned from ti 
# BBands, PBands, DonchianChannel, 
# aroon, TDI, ADX, ATR, EMV, KST, TRIX, MACD, DVI, stoch, SMI 
# numcol = sapply(unlist(ti_oi()), function(x) {
#   arg_lst = list()
#   arg_lst[['dt']] = dt
#   arg_lst[[x]] = list()
#   
#   ncol(do.call(pq_addti, args = arg_lst)[[1]])
# } )
# unlist(ti_oi())[which(numcol>1)]

###### 
# Technical Overlays

# Moving Averages (maType) ------
# SMA(x, n = 10, ...): Simple Moving Average
# EMA(x, n = 10, wilder = FALSE, ratio = NULL, ...): Exponential Moving Average
# DEMA(x, n = 10, v = 1, wilder = FALSE, ratio = NULL): Double Exponential Moving Average
# WMA(x, n = 10, wts = 1:n, ...): Weighted Moving Average
# EVWMA(price, volume, n = 10, ...): Elastic, Volume-Weighted Moving Average
# ZLEMA(x, n = 10, ratio = NULL, ...): Zero Lag Exponential Moving Average
# VWAP(price, volume, n = 10, ...): Volume-Weighted Moving Average Price
# VMA(x, w, ratio = 1, ...): Variable-Length Moving Average
# HMA(x, n = 20, ...): Hull Moving Average
# ALMA(x, n = 9, offset = 0.85, sigma = 6, ...): Arnaud Legoux Moving Average
# GMMA(x, short = c(3, 5, 8, 10, 12, 15), long = c(30, 35, 40, 45, 50, 60), maType): Guppy Multiple Moving Averages

# rolling function over a n-period moving window ------
# runMin(x, n = 10, cumulative = FALSE): returns minimums 
# runMax(x, n = 10, cumulative = FALSE): returns maximums 
# runMean(x, n = 10, cumulative = FALSE): returns means 
# runMedian(x, n = 10, non.unique = "mean", cumulative = FALSE): returns medians 
# runCov(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE): returns covariances 
# runCor(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE): returns correlations 
# runVar(x, y = NULL, n = 10, sample = TRUE, cumulative = FALSE): returns variances 
# runSD(x, n = 10, sample = TRUE, cumulative = FALSE): returns standard deviations 
# runMAD(x, n = 10, center = NULL, stat = "median", constant = 1.4826, non.unique = "mean", cumulative = FALSE): returns median/mean absolute deviations 
# runSum(x, n = 10, cumulative = FALSE): returns sums 
# wilderSum(x, n = 10): retuns a Welles Wilder style weighted sum 

# Bands/Channels ------
# BBands(HLC, n = 20, maType, sd = 2, ...): Bollinger Bands
# PBands(prices, n = 20, maType = "SMA", sd = 2, ..., fastn = 2, centered = FALSE, lavg = FALSE): Construct volatility bands around prices
# DonchianChannel(HL, n = 10, include.lag = FALSE): Donchian Channel


# SAR(HL, accel = c(0.02, 0.2)): Parabolic Stop-and-Reverse
# ZigZag(HL, change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE): Zig Zag


######
# Technical Indicators

# trend direction/strength ------
# aroon(HL, n = 20): Aroon
# CCI(HLC, n = 20, maType, c = 0.015, ...): Commodity Channel Index
# ADX(HLC, n = 14, maType, ...): Welles Wilder’s Directional Movement Index
# TDI(price, n = 20, multiple = 2): Trend Detection Index
# VHF(price, n = 28): Vertical Horizontal Filter
# EMV(HL, volume, n = 9, maType, vol.divisor = 10000, ...): Arms' Ease of Movement Value


# volatility measure ------
# ATR(HLC, n = 14, maType, ...): True Range / Average True Range
# chaikinVolatility(HL, n = 10, maType, ...): Chaikin Volatility
# volatility(OHLC, n = 10, calc = "close", N = 260, mean0 = FALSE, ...): Volatility
# SNR(HLC, n, ...): Signal to Noise Ratio

# measure of the money flowing into or out of a security ------
# OBV(price, volume): On Balance Volume
# chaikinAD(HLC, volume): Chaikin Accumulation / Distribution
# CLV(HLC): Close Location Value
# CMF(HLC, volume, n = 20): Chaikin Money Flow
# MFI(HLC, volume, n = 14): Money Flow Index
# williamsAD(HLC): Williams Accumulation / Distribution




# Rate of Change / Momentum: ------
# ROC(x, n = 1, type = c("continuous", "discrete"), na.pad = TRUE): Rate of Change
# momentum(x, n = 1, na.pad = TRUE): Momentum
# KST(price, n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), nSig = 9, maType, wts = 1:NROW(n), ...): Know Sure Thing
# TRIX(price, n = 20, nSig = 9, maType, percent = TRUE, ...): Triple Smoothed Exponential Oscillator


# Oscillator ------
# MACD(x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, ...)
# DPO(x, n = 10, maType, shift = n/2 + 1, percent = FALSE, ...): De-Trended Price Oscillator
# DVI(price, n = 252, wts = c(0.8, 0.2), smooth = 3, magnitude = c(5, 100, 5), stretch = c(10, 100, 2), exact.multiplier = 1): DV Intermediate Oscillator
# ultimateOscillator(HLC, n = c(7, 14, 28), wts = c(4, 2, 1)): The Ultimate Oscillator

# Relative Strength Index: ------
# RSI(price, n = 14, maType, ...): Relative Strength Index
# CMO(x, n = 14): Chande Momentum Oscillator


# Stochastic Oscillator / Momentum Index: ------
# stoch(HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType, bounded = TRUE, smooth = 1, ...): Stochastic Oscillator
# WPR(HLC, n = 14): William's %R
# SMI(HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9, maType, bounded = TRUE, ...): Stochastic Momentum Index



######
ti_fst_arg = function() {
  list(
    OHLC = 'volatility',
    HLC  = c('CCI', 'ADX', 'ATR', 'chaikinAD', 'CLV', 'CMF', 'MFI', 'williamsAD', 'BBands', 'ultimateOscillator', 'stoch', 'WPR', 'SMI', 'SNR'),
    HL   = c('aroon', 'EMV', 'chaikinVolatility', 'DonchianChannel', 'SAR', 'ZigZag'), 
    price  = c('TDI', 'VHF', 'OBV', 'KST', 'TRIX', 'EVWMA', 'VWAP', 'DVI', 'RSI'),
    prices = 'PBands',
    x = c('ROC','momentum','SMA','EMA','DEMA','WMA','ZLEMA','VMA','HMA','ALMA','GMMA','runSum','runMin','runMax','runMean','runMedian','runCov','runCor','runVar','runSD','runMAD','wilderSum','MACD','DPO','CMO', 'bias', 'maroc')
  )
}
ti_sec_arg = function() {
  list(
    volume = c('EMV','OBV','chaikinAD','CMF','MFI','EVWMA','VWAP'),
    y = c('runCov', 'runCor', 'runVar'), 
    w = 'VMA'
  )
}

# add one technical indicator
#' @import TTR
addti1 = function(dt, ti, col_formula = FALSE, ...) {
  .=high=low=volume=adx_dx=adx_adx=adx_dip=adx_din=formula_str=NULL
  
  dt = setDT(copy(dt))
  setnames(dt, tolower(names(dt)))
  
  # arguments
  arg_lst = list()
  ## first argument ## OHLC, HLC, HL, price, prices, x
  if        (ti %in% ti_fst_arg()$OHLC) { # OHLC
    arg_lst = c(arg_lst, list(OHLC=dt[,.(open,high,low,close)]))
  } else if (ti %in% ti_fst_arg()$HLC ) { # HLC
    arg_lst = c(arg_lst, list(HLC =dt[,.(high,low,close)]))
  } else if (ti %in% ti_fst_arg()$HL  ) { # HL
    arg_lst = c(arg_lst, list(HL  =dt[,.(high,low)]))
  } else {
    price_x_arg = intersect(c('price', 'prices', 'x'), names(list(...)))
    if (length(price_x_arg) > 0) {
      cols_price_x = list(...)[[price_x_arg[1]]]
    } else {
      cols_price_x = intersect(c('close', 'value'), names(dt))  
    }
    
    if (ti %in% ti_fst_arg()$price) {
      arg_lst = c(arg_lst, list(price = dt[,cols_price_x[1],with=FALSE]))
    } else if (ti %in% ti_fst_arg()$prices) {
      arg_lst = c(arg_lst, list(prices= dt[,cols_price_x,   with=FALSE]))
    } else if (ti %in% ti_fst_arg()$x) {
      arg_lst = c(arg_lst, list(x     = dt[,cols_price_x[1],with=FALSE]))
    }
  }
  ## second argument ## volume, y, w
  if (ti %in% ti_sec_arg()$volume) {
    arg_lst = c(arg_lst, list(volume=dt[,.(volume)]))
  } else if (ti %in% ti_sec_arg()$y) {
    y = list(...)[['y']]
    if (!is.null(y)) y = dt[,y,with=FALSE]
    arg_lst = c(arg_lst, list(y=y))
  } else if (ti %in% ti_sec_arg()$w) {
    w = list(...)[['w']]
    if (length(w)==1 & inherits(w, "character")) w = dt[,w,with=FALSE]
    arg_lst = c(arg_lst, list(w=w))
  }
  arg_lst = c(
    arg_lst, 
    list(...)[setdiff(names(list(...)), c('color', 'position', 'hline', 'height'))]
  )
  arg_lst = arg_lst[unique(names(arg_lst))]
  dtti = data.table(do.call(ti, args = arg_lst))
  
  
  # parameters
  param = c('n', 'sd', 'v', 'nFast', 'nSlow', 'nSig', 'accel')
  param_list = list(...)[intersect(param, names(list(...)))]
  if (ti == 'KST') param_list = NULL
  
  # setnames for dtti
  ncol_dtti = ncol(dtti) 
  if (ncol_dtti == 1) {
    par_str = paste(unlist(param_list), collapse='_') 
    setnames(dtti, tolower(paste(ti, par_str, sep = '_')))
  } else {
    setnames(dtti, tolower(paste(ti, names(dtti), sep = '_')))
  }
  if (tolower(ti) == 'adx') dtti = dtti[,.(adx_dx, adx_adx, adx_dip, adx_din)]
  
  # formula
  if (col_formula) {
    par_str = paste(unlist(param_list), collapse=',') 
    formula_string = sprintf('%s(%s)', ti, par_str)
    dtti = dtti[, formula_str := formula_string]
  }
  
  return(dtti)
}

# add technical indcators for one dataset
pq1_addti = function(dt, ...) {
  col_formula = FALSE
  if ("col_formula" %in% names(list(...)))  col_formula = list(...)[["col_formula"]]
  col_kp = TRUE
  if ("col_kp" %in% names(list(...)))  col_kp = list(...)[["col_kp"]]
  
  
  ti_lst =list(...) #setdiff(names(), c('col_kp','col_formula'))
  dtti_list = list()
  for (i in seq_len(length(ti_lst))) {
    ti_name = names(ti_lst)[i]
    if (ti_name %in% c('col_kp','col_formula')) next
    # check ti 
    if (!(ti_name %in% unlist(ti_fst_arg()))) 
      ti_name = unlist(ti_fst_arg())[which(tolower(ti_name) == tolower(unlist(ti_fst_arg())))]
    if (length(ti_name) == 0) {
      warning(sprintf('The technical indicator %s is not available in TTR package.', names(ti_lst)[i]))
      next
    }
    
    arg_lst = list(dt=dt, ti=ti_name, col_formula = col_formula)
    dtti_list[[i]] = do.call(addti1, args = c(arg_lst, ti_lst[[i]]))
    # print(ti_name)
  }
  ti_df = setDT(unlist(dtti_list, recursive = FALSE))[] # do.call(cbind, dtti_list) # 
  
  # merge ti-df with dt
  if (is.logical(col_kp) & isTRUE(col_kp)) {
    ti_df = cbind(dt, ti_df)
  } else {
    col_inter = intersect(names(dt), col_kp)
    if (!is.null(col_kp) & length(col_inter) > 0) {
      ti_df = cbind(dt[,col_inter,with=FALSE], ti_df)
    }
  }
  
  return(ti_df)
}



#' adding technical indicators
#' 
#' `pq_addti` creates technical indicators on provided datasets use TTR package.
#' 
#' @param dt a list/dataframe of time series datasets.
#' @param ... list of technical indicator parameters: sma = list(n=50), macd = list().
#' 1. There are four types of parameters. 
#' \itemize{
#'    \item set by default and do not required, such as 'OHLC', 'HLC', 'HL' and 'volume'.
#'    \item set by default and can be modified, such as 'price', 'prices', 'x'. Its default value is 'close' or 'value' column.
#'    \item always required, such as 'y', 'w'.
#'    \item numeric parameters, such as 'n', 'sd', 'v', 'nFast', 'nSlow', 'nSig', 'accel'. These parameters should be provided, otherwise using default values in corresponding function.
#' }
#' 2. TTR functions are summarised in below. See TTR package's help document for more detailed parameters. 
#' \itemize{
#'    \item moving averages: SMA, EMA, DEMA, WMA, EVWMA, ZLEMA, VWAP, VMA, HMA, ALMA, GMMA
#'    \item rolling functions: runMin, runMax, runMean, runMedian; runCov, runCor; runVar, runSD, runMAD; runSum, wilderSum
#'    \item bands / channels: BBands, PBands, DonchianChannel
#'    \item SAR, ZigZag
#'    \item trend direction/strength: aroon, CCI, ADX, TDI, VHF, EMV
#'    \item volatility measures: ATR, chaikinVolatility, volatility, SNR
#'    \item money flowing into/out: OBV, chaikinAD, CLV, CMF, MFI, williamsAD
#'    \item rate of change / momentum: ROC, momentum, KST, TRIX
#'    \item oscillator: MACD, DPO, DVI, ultimateOscillator; RSI, CMO; stoch, SMI, WPR
#' }
#'
#' @examples 
#' \donttest{
#' # load data
#' dt = md_stock("^000001", source='163', date_range = 'max')
#' 
#' # add technical indicators
#' dt_ti1 = pq_addti(dt, sma=list(n=20), sma=list(n=50), macd = list())
#' 
#' # only technical indicators
#' dt_ti2 = pq_addti(dt, sma=list(n=20), sma=list(n=50), macd = list(), col_kp = FALSE)
#' }
#' 
#' @export
pq_addti = function(dt, ...) {
  # col_kp, col_formula
  symbol = NULL
  
  # bind list of dataframes
  if (is.list(dt) & !is.data.frame(dt)) {
    dt = rbindlist(dt, fill = TRUE)
  }
  
  ## single series
  dt_list = list()
  sybs = dt[, unique(symbol)]
  for (s in sybs) {
    dt_s = dt[symbol == s]
    setkeyv(dt_s, "date")
    dt_list[[s]] = pq1_addti(dt=dt_s, ...)
  }
  return(dt_list)
}






