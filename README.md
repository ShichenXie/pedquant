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
    [NBS](https://www.stats.gov.cn/) and
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
    packageVersion('pedquant')
    #> [1] '0.2.4'
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
    dtorders = copy(dtbnkti[['601988.SH']])[
       sma_50 %x>% sma_200, `:=`(side = 1,  prices = close_adj)
     ][sma_50 %x<% sma_200, `:=`(side = -1, prices = close_adj)
     ][order(date)
     ][, (c('side', 'prices')) := lapply(.SD, shift), .SDcols = c('side', 'prices')
     ][,.(symbol, name, date, side, prices)
     ][!is.na(side)]
    head(dtorders)
    #>       symbol     name       date side prices
    #> 1: 601988.SH 中国银行 2021-04-20    1   5.76
    #> 2: 601988.SH 中国银行 2021-08-19   -1   5.67
    #> 3: 601988.SH 中国银行 2021-11-18    1   5.70
    #> 4: 601988.SH 中国银行 2021-11-25   -1   5.71
    #> 5: 601988.SH 中国银行 2022-01-18    1   5.72

    # charting
    e = pq_plot(setDT(dt_banks)[symbol=='601988.SH'],  y='close_adj', addti = list(sma=list(n=200), sma=list(n=50)), orders = dtorders)
    # e[['601988.SS']]

## Issues and Contributions

This package still on the developing stage. If you have any issue when
using this package, please update to the latest version from github. If
the issue still exists, report it at [github
page](https://github.com/ShichenXie/pedquant/issues). Contributions in
any forms to this project are welcome.
