% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pq_addti.R
\name{pq_addti}
\alias{pq_addti}
\title{adding technical indicators}
\usage{
pq_addti(dt, ...)
}
\arguments{
\item{dt}{a list/dataframe of time series datasets.}

\item{...}{list of technical indicator parameters: sma = list(n=50), macd = list().
\enumerate{
\item There are four types of parameters.
\itemize{
\item set by default and do not required, such as 'OHLC', 'HLC', 'HL' and 'volume'.
\item set by default and can be modified, such as 'price', 'prices', 'x'. Its default value is 'close' or 'value' column.
\item always required, such as 'y', 'w'.
\item numeric parameters, such as 'n', 'sd', 'v', 'nFast', 'nSlow', 'nSig', 'accel'. These parameters should be provided, otherwise using default values in corresponding function.
}
\item TTR functions are summarized in below. See TTR package's help document for more detailed parameters.
\itemize{
\item moving averages: SMA, EMA, DEMA, WMA, EVWMA, ZLEMA, VWAP, VMA, HMA, ALMA, GMMA
\item rolling functions: runMin, runMax, runMean, runMedian; runCov, runCor; runVar, runSD, runMAD; runSum, wilderSum
\item bands / channels: BBands, PBands, DonchianChannel
\item SAR, ZigZag
\item trend direction/strength: aroon, CCI, ADX, TDI, VHF, EMV
\item volatility measures: ATR, chaikinVolatility, volatility, SNR
\item money flowing into/out: OBV, chaikinAD, CLV, CMF, MFI, williamsAD
\item rate of change / momentum: ROC, momentum, KST, TRIX
\item oscillator: MACD, DPO, DVI, ultimateOscillator; RSI, CMO; stoch, SMI, WPR
}
}}
}
\description{
\code{pq_addti} creates technical indicators using the functions provided in TTR package.
}
\examples{
\donttest{
# load data
data('dt_ssec')

# add technical indicators
dt_ti1 = pq_addti(dt_ssec, sma=list(n=20), sma=list(n=50), macd = list())
dt_tinam = pq_addti(dt_ssec, sma=list(n=20), sma=list(n=50), macd = list(), tinam_prefix = TRUE)

# specify the price column x
dt_ti11 = pq_addti(dt_ssec, sma=list(n=20, x='open'), sma=list(n=50, x='open'))
dt_ti12 = pq_addti(dt_ssec, x='open', sma=list(n=20), sma=list(n=50))

# only technical indicators
dt_ti2 = pq_addti(
  dt_ssec, sma=list(n=20), sma=list(n=50), macd = list(), 
  col_kp = c('symbol', 'name')
)

dt_ti3 = pq_addti(
  dt_ssec, sma=list(n=20), sma=list(n=50), macd = list(), 
  col_kp = NULL
)

# self-defined technical indicators
bias = function(x, n=50, maType='SMA') {
    library(TTR)
    (x/do.call(maType, list(x=x, n=n))-1)*100
}

dt_ti3 = pq_addti(dt_ssec, bias = list(n = 200))

}
}
