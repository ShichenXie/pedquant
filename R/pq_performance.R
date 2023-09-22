#' performance functions
#' 
#' A complete list of performance functions from `PerformanceAnalytics` package.
#' 
#'@export
pq_performance_funs = function() {
    perf_metric_fun = sort(getNamespaceExports("PerformanceAnalytics"))
    
    funs_table = perf_metric_fun[grepl('^table', perf_metric_fun) & !grepl('(Drawdowns|CalendarReturns|ProbOutPerformance)$', perf_metric_fun)]
    
    funs_capm = c(perf_metric_fun[grepl('^CAPM', perf_metric_fun)], 'TimingRatio', 'MarketTiming')
    
    funs_sfm = perf_metric_fun[grepl('^SFM', perf_metric_fun)]
    
    funs_var = c("VaR", "ES", "ETL", "CDD", "CVaR")
    
    funs_descriptive = perf_metric_fun[grepl('^mean', perf_metric_fun)] # "mean", "sd", "min", "max", "cor"
    
    funs_annualized = perf_metric_fun[grepl('annualized', perf_metric_fun)]
    
    funs_moments = c(perf_metric_fun[grepl('^Co|BetaCo', perf_metric_fun)], "M3.MM", "M4.MM") # "var", "cov"
    
    funs_drawdown = c("AverageDrawdown", "AverageLength", "AverageRecovery", "DrawdownDeviation", "DrawdownPeak", "maxDrawdown")
    
    funs_risk = c("MeanAbsoluteDeviation", "Frequency", "SharpeRatio", "MSquared", "MSquaredExcess", "HurstIndex")
    
    funs_regression = c(
        "CAPM.alpha", "CAPM.beta", "CAPM.epsilon", "CAPM.jensenAlpha", "SystematicRisk", 
        "SpecificRisk", "TotalRisk", "TreynorRatio", "AppraisalRatio", "FamaBeta", 
        "Selectivity", "NetSelectivity")
    
    funs_rel_risk = c("ActivePremium", "ActiveReturn", "TrackingError", "InformationRatio")
    
    funs_drw_dn = c("PainIndex", "PainRatio", "CalmarRatio", "SterlingRatio", "BurkeRatio", "MartinRatio", "UlcerIndex")
    
    funs_dside_risk = c(
        "DownsideDeviation", "DownsidePotential", "DownsideFrequency", "SemiDeviation", "SemiVariance", 
        "UpsideRisk", "UpsidePotentialRatio", "UpsideFrequency", 
        "BernardoLedoitRatio", "DRatio", "Omega", "OmegaSharpeRatio", "OmegaExcessReturn", "SortinoRatio", "M2Sortino", "Kappa", 
        "VolatilitySkewness", "AdjustedSharpeRatio", "SkewnessKurtosisRatio", "ProspectRatio")
    
    funs_misc = c("KellyRatio", "Modigliani", "UpDownRatios")
    
    fun_list = list(table.funs                     = funs_table,
                       CAPM.funs                      = funs_capm,
                       SFM.funs                       = funs_sfm,
                       descriptive.funs               = funs_descriptive,
                       annualized.funs                = funs_annualized,
                       VaR.funs                       = funs_var,
                       moment.funs                    = funs_moments,
                       drawdown.funs                  = funs_drawdown,
                       Bacon.risk.funs                = funs_risk,
                       Bacon.regression.funs          = funs_regression,
                       Bacon.relative.risk.funs       = funs_rel_risk,
                       Bacon.drawdown.funs            = funs_drw_dn,
                       Bacon.downside.risk.funs       = funs_dside_risk,
                       misc.funs                      = funs_misc)
    
    return(fun_list)
}

#' @import PerformanceAnalytics
pq1_performance = function(dt1, Ra, Rb=NULL, perf_fun, col_date='date', ...) {
    perffun_arglst = formals(perf_fun)
    perffun_argnam = names(perffun_arglst)
    
    additional_arglst = list(...)
    additional_argnam = names(additional_arglst)
    
    arg_lst = list()
    arg_lst[[perffun_argnam[1]]] = dt1[, c(col_date,Ra), with=FALSE]
    if (!is.null(Rb)) arg_lst[[perffun_argnam[2]]] = dt1[, c(col_date,Rb), with=FALSE]
    if ('...' %in% perffun_argnam) {
        arg_lst = c(arg_lst, additional_arglst)
    } else {
        arg_lst = c(arg_lst, additional_arglst[intersect(additional_argnam, perffun_argnam)])
    }
    
    ret = do.call(perf_fun, args = arg_lst)
    ret = setDT(as.data.frame(t(ret)), keep.rownames=FALSE)[]
    return(ret)
}

#' calculating performance metrics 
#' 
#' `pq_performance` calculates performance metrics based on returns of market price or portfolio. The performance analysis functions are calling from `PerformanceAnalytics` package, which includes many widely used performance metrics.
#' 
#' @param dt a list/dataframe of time series datasets.
#' @param Ra the column name of asset returns.
#' @param Rb the column name of baseline returns, defaults to NULL.
#' @param perf_fun performance function from `PerformanceAnalytics` package, see `pq_perf_funs`.
#' @param ... additional parameters, the arguments used in `PerformanceAnalytics` functions.
#' 
#' @examples  
#' \dontrun{
#' library(pedquant) 
#' library(data.table)
#' 
#' # load data
#' data(dt_banks)
#' data(dt_ssec)
#' 
#' # calculate returns
#' datret1 = pq_return(dt_banks, 'close', freq = 'monthly', rcol_name = 'Ra')
#' datret2 = pq_return(dt_ssec, 'close', freq = 'monthly', rcol_name = 'Rb')
#' 
#' # merge returns of assets and baseline
#' datRaRb = merge(
#'     rbindlist(datret1)[, .(date, symbol, Ra)], 
#'     rbindlist(datret2)[, .(date, Rb)],
#'     by = 'date', all.x = TRUE
#' )
#' 
#' # claculate table.CAPM metrics
#' perf_capm = pq_performance(datRaRb, Ra = 'Ra', Rb = 'Rb', perf_fun = 'table.CAPM')
#' rbindlist(perf_capm, idcol = 'symbol')
#' }
#' 
#' @export
#' 
pq_performance = function(dt, Ra, Rb=NULL, perf_fun, ...) {
    # list to data.table
    dt = check_dt(dt)
    
    ret_list = lapply(
        split(dt, by = 'symbol'), 
        function(dts) {do.call(
            'pq1_performance', 
            args = c(list(dt1=dts, Ra=Ra, Rb=Rb, perf_fun=perf_fun), list(...))
        )}
    )
    return(ret_list)
}


pq_perfeva = function(dt, x, orders, addti=NULL, init_fund=NULL) {
    tid = bs = s = b = NULL 
    
    if (!('quantity' %in% names(orders))) {
        orders = odr_addvol(orders)
    }
    orders = check_odr(orders)
    
    dt = pq_portfolio(dt, x=x, orders=orders, cols_keep = 'all', init_fund = init_fund)
    addti = c(addti, list(cumreturns=list(), portfolio=list()))
    
    e = do.call('pq_plot', args = list(
        dt=dt, y=x, chart_type='line', 
        addti = addti,
        orders = orders
    ) )
    
    # orders summary
    odr_summary = dcast(copy(orders)[, tid := cumsum(bs == 'b') ], 'tid ~ bs', value.var = 'prices')
    # winning ratio
    wr = odr_summary[, round(mean(s>b, na.rm = TRUE),3)*100]
    print(list(winning_ratio = wr))
    
    return(e)
}

