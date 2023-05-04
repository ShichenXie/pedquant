<!-- README.md is generated from README.Rmd. Please edit that file -->

# pedquant

[![CRAN
status](https://www.r-pkg.org/badges/version/pedquant)](https://cran.r-project.org/package=pedquant)
[![](http://cranlogs.r-pkg.org/badges/grand-total/pedquant)](https://cran.r-project.org/package=pedquant)
[![](http://cranlogs.r-pkg.org/badges/pedquant)](https://cran.r-project.org/package=pedquant)

`pedquant` (Public Economic Data and QUANTitative analysis) provides an
interface to access public economic and financial data for economic
research and quantitative analysis. The functions are grouped into three
main categories,

-   ed\_\* (economic data) functions load economic data from
    [NBS](http://www.stats.gov.cn/) and
    [FRED](https://fred.stlouisfed.org/);
-   md\_\* (market data) functions load the forex, money, bond, stock,
    future market data from public data sources, including 163, Sina, qq
    finance and etc.
-   pq\_\* (quantitative analysis) functions create technical
    indicators, visualization charts and industrial index etc for time
    series data.

The functions in this package are designed to write minimum codes for
some common tasks in quantitative analysis process. Since the parameters
to get data can be interactively specify, it’s very easy to start. The
loaded data have been carefully cleansed and provided in a unified
format.

`pedquant` package has advantages on multiple aspects, such as the
format of loaded data is a list of data frames, which can be easily
manipulated in [data.table](https://rdatatable.gitlab.io/data.table) or
[tidyverse](https://www.tidyverse.org) packages; high performance on
speed by using [data.table](https://rdatatable.gitlab.io/data.table) and
[TTR](https://github.com/joshuaulrich/TTR); and interactive charts by
using [echarts4r](https://echarts4r.john-coene.com). Similar works
including [tidyquant](https://github.com/business-science/tidyquant) or
[quantmod](https://github.com/joshuaulrich/quantmod).

## Installation

-   Install the release version of `pedquant` from CRAN with:

<!-- -->

    install.packages("pedquant")

-   Install the developing version of `pedquant` from
    [github](https://github.com/shichenXie/pedquant) with:

<!-- -->

    devtools::install_github("shichenxie/pedquant")

## Example

The following examples show you how to import data.

    library(pedquant)
    # loading data
    ## import eocnomic data
    dat1 = ed_fred('GDPCA')
    #> 1/1 GDPCA
    dat2 = ed_nbs(geo_type='nation', freq='quarterly', symbol='A010101')

    ## import market data
    FAAG = md_stock(c('META', 'AMZN', 'AAPL', 'GOOG'), date_range = '10y')
    #> 1/4 meta
    #> 2/4 amzn
    #> 3/4 aapl
    #> 4/4 goog
    INDX = md_stock(c('^000001','^399001'), date_range = '10y')
    #> 1/2 ^000001
    #> 2/2 ^399001

    # double moving average strategy
    ## add technical indicators
    data("dt_banks")
    dtbnkti = pq_addti(dt_banks, x='close_adj', sma=list(n=200), sma=list(n=50))

    ## crossover signals
    library(data.table)
    dtorders = copy(dtbnkti[['601988.SS']])[
       sma_50 %x>% sma_200, `:=`(type = 'buy',  prices = close_adj)
     ][sma_50 %x<% sma_200, `:=`(type = 'sell', prices = close_adj)
     ][order(date)
     ][, (c('type', 'prices')) := lapply(.SD, shift), .SDcols = c('type', 'prices')
     ][,.(symbol, name, date, close_adj, type, prices)]
    head(dtorders[!is.na(type)])
    #>       symbol     name       date close_adj type prices
    #> 1: 601988.SS 中国银行 2019-01-30      5.69  buy   5.70
    #> 2: 601988.SS 中国银行 2019-10-14      5.93 sell   5.90
    #> 3: 601988.SS 中国银行 2019-10-31      5.97  buy   5.95
    #> 4: 601988.SS 中国银行 2020-01-17      5.91 sell   5.91
    #> 5: 601988.SS 中国银行 2020-08-31      5.71  buy   5.74
    #> 6: 601988.SS 中国银行 2020-09-11      5.63 sell   5.63

    # charting
    e = pq_plot(dt_banks,  y='close_adj', addti = list(sma=list(n=200), sma=list(n=50)), orders = dtorders[!is.na(type)])
    # e[['601988.SS']]

## Issues and Contributions

This package still on the developing stage. If you have any issue when
using this package, please update to the latest version from github. If
the issue still exists, report it at [github
page](https://github.com/ShichenXie/pedquant/issues). Contributions in
any forms to this project are welcome.
