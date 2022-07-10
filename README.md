<!-- README.md is generated from README.Rmd. Please edit that file -->

# pedquant

[![Travis build
status](https://travis-ci.org/ShichenXie/pedquant.svg?branch=master)](https://travis-ci.org/ShichenXie/pedquant)
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
using [echarts4r](https://echarts4r.john-coene.com/). Similar works
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
    ## import eocnomic data
    dat1 = ed_fred('GDPCA')
    #> 1/1 GDPCA
    dat2 = ed_nbs(geo_type='nation', freq='quarterly', symbol='A010101')

    ## import market data
    FAAG = md_stock(c('FB', 'AMZN', 'AAPL', 'GOOG'), date_range = '10y')
    #> 1/4 fb
    #> 2/4 amzn
    #> 3/4 aapl
    #> 4/4 goog
    INDX = md_stock(c('^000001','^399001'), date_range = '10y')
    #> 1/2 ^000001
    #> 2/2 ^399001

    # moving average crossover strategy
    library(data.table)

    # load data
    data("dt_banks")
    dtboc = md_stock_adjust(setDT(dt_banks)[symbol=='601988.SS'])
    # added technical indicators
    bocti = pq_addti(dtboc, x='close_adj', sma=list(n=200), sma=list(n=50))

    # crossover signal 
    dtorders = copy(bocti[[1]])[,.(symbol, name, date, close_adj, sma_50, sma_200)
     ][sma_50 %x>% sma_200, `:=`(
        type = 'buy', prices = close_adj
    )][sma_50 %x<% sma_200, `:=`(
        type = 'sell', prices = close_adj
    )][order(date)
     ][, (c('type', 'prices')) := lapply(.SD, shift), .SDcols = c('type', 'prices')]
    head(dtorders[!is.na(type)])
    #>       symbol     name       date close_adj   sma_50  sma_200 type   prices
    #> 1: 601988.SS 中国银行 2009-04-27  2.175686 2.159212 2.154464  buy 2.169398
    #> 2: 601988.SS 中国银行 2010-03-23  2.682837 2.685811 2.690783 sell 2.689302
    #> 3: 601988.SS 中国银行 2010-05-06  2.618190 2.695508 2.694150  buy 2.669908
    #> 4: 601988.SS 中国银行 2010-05-20  2.514756 2.677794 2.679346 sell 2.521220
    #> 5: 601988.SS 中国银行 2011-04-27  2.332291 2.302393 2.299774  buy 2.318449
    #> 6: 601988.SS 中国银行 2011-07-21  2.231820 2.285567 2.288007 sell 2.239067

    # charting
    e = pq_plot(dtboc,  y='close_adj', addti = list(sma=list(n=200), sma=list(n=50)), orders = dtorders[!is.na(type)])
    e[[1]]
    #> NULL

## Issues and Contributions

This package still on the developing stage. If you have any issue when
using this package, please update to the latest version from github. If
the issue still exists, report it at [github
page](https://github.com/ShichenXie/pedquant/issues). Contributions in
any forms to this project are welcome.
