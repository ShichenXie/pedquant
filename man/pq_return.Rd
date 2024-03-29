% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pq_return.R
\name{pq_return}
\alias{pq_return}
\title{calculating returns by frequency}
\usage{
pq_return(dt, x, freq = "daily", n = 1, date_type = "eop",
  method = "arithmetic", cumreturns = FALSE, rcol_name = NULL,
  cols_keep = NULL, date_range = "max", from = NULL, to = Sys.Date(),
  ...)
}
\arguments{
\item{dt}{a list/dataframe of daily series.}

\item{x}{the column name of adjusted asset price.}

\item{freq}{the frequency of returns. It supports 'daily', 'weekly', 'monthly', 'quarterly', 'yearly' and 'all'. Defaults to daily.}

\item{n}{the number of preceding periods used as the base value, defaults to 1, which means based on the previous period value.}

\item{date_type}{the available date type are eop (end of period) and bop (beginning of period), defaults to the eop.}

\item{method}{the method to calculate asset returns, the available methods including arithmetic and log, defaults to arithmetic.}

\item{cumreturns}{logical, whether to return cumulative returns. Defaults to FALSE.}

\item{rcol_name}{setting the column name of returns, defaults to NULL.}

\item{cols_keep}{the columns keep in the return data. The columns of symbol, name and date will always kept if they are exist in the input data.}

\item{date_range}{date range. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.}

\item{from}{the start date. Default is NULL. If it is NULL, then calculate using date_range and end date.}

\item{to}{the end date. Default is the current date.}

\item{...}{ignored}
}
\description{
\code{pq_return} calculates returns for daily series based on specified column, frequency and method type.
}
\examples{
\donttest{
# load data and adjust
data(dt_banks)
datadj = md_stock_adjust(dt_banks)

# set freq
dts_returns1 = pq_return(datadj, x = 'close_adj', freq = 'all')

# set method
dts_returns2 = pq_return(datadj, x = 'close_adj', method = 'log')

# set cols_keep
dts_returns3 = pq_return(datadj, x = 'close_adj', cols_keep = 'cap_total')

# cumulative returns
dts_cumreturns = pq_return(datadj, x = 'close_adj', from = '2012-01-01', cumreturns = TRUE)
e1 = pq_plot(dts_cumreturns, y = 'cumreturns.daily', title='cumreturns', 
        arrange = list(rows=1, cols=1))
e1[[1]]
}

}
