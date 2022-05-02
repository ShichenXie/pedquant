
pp_dtpre = function(dt, x='date', y='close', 
                    addti = NULL, markline=TRUE, 
                    orders, order_y = 'price', order_type='type') {
    sybnam = symbol = name = markline_value = NULL
    
    dt = setorderv(copy(dt), c('symbol', x))[, sybnam := sprintf('%s %s', symbol, name[.N]), by = 'symbol']
    if (!is.null(addti)) {
        dt = suppressWarnings(do.call('pq_addti', args = c(list(dt=dt), addti)))
        dt = rbindlist(dt)
        rmcols = intersect(c('bbands_pctb', 'adx_dx', 'atr_tr', 'atr_truehigh', 'atr_truelow', 'aroon_oscillator'), names(dt))
        
        # if (names(addti[i]) == 'bbands') dtti = dtti[,(c('bbands_dn', 'bbands_mavg', 'bbands_up')) := NULL]
        
        if (length(rmcols) > 0) dt = dt[, (rmcols) := NULL]
    }
    if (isTRUE(markline)) dt = dt[, markline_value := get(y)[.N], by = 'symbol']
    
    if (is.null(orders)) return(dt)
    
    dt = merge(
        dt, 
        dcast(orders, 
              sprintf('%s ~ %s', x, order_type), 
              value.var = order_y), 
        by = x, all = TRUE
    )
    
    return(dt)
}
pp_title = function(dt, title=NULL) {
    symbol = name = NULL 
    if (is.null(title) & dt[,length(unique(symbol))==1]) title = setDT(dt)[.N, paste(symbol, name)]
    return(title)
}
pp_xstart = function(dt, x = 'date', date_range = 'max') {
    to0 = max(dt[[x]])
    from0 = min(dt[[x]])
    from = check_from(date_range, from = NULL, to = to0)
    
    if (date_range == 'max') {
        xstart = 0
    } else {
        xstart = as.numeric(from - from0)/as.numeric(to0 - from0) * 100
    }
    return(xstart)
}


p_theme = function(e, xstart = 0, xend = 100, yaxis_log = FALSE, title = 'none', theme = 'default') {
    if (isTRUE(yaxis_log)) yaxis_type = 'log' else yaxis_type = 'value'
    
    e |>  
        e_title(title, left='30') |> 
        e_tooltip(trigger='axis', axisPointer = list(type = 'line', show = TRUE)) |> 
        e_datazoom(x_index = 0, start = xstart, end = xend) |> 
        e_y_axis(type = yaxis_type, position = 'right', axisLabel = list(rotate = 90) ) |> 
        e_toolbox(right='30') |> 
        e_toolbox_feature(c("restore", "dataZoom", "saveAsImage")) |> 
        e_legend(type = "plain", orient = "vertical", left='30', top='30') |> 
        e_grid(show=TRUE, left='30', right = '30', top='30') |>
        e_theme(theme) 
        
}
p_markline = function(e, dt, markline = TRUE) {
    markline_value = NULL 
    
    if (isFALSE(markline)) return(e)
    
    # e = e |>
    #     e_line_('markline_value', legend=FALSE, symbol='none', 
    #             lineStyle = list(type = 'dashed', width = 1, color='gray'))
    
    markline_yvals = dt[, unique(markline_value)]
    for (yv in markline_yvals) {
        e = e |>
            e_mark_line(data = list(yAxis = yv), symbol = 'none',
                        lineStyle = list(type = 'dashed', color = 'gray'))
    }
    return(e)
}
# orders
p_orders = function(e, orders, color_up = "#CF002F", color_down = "#000000") {
    if (is.null(orders)) return(e)
    e = e |> 
        e_scatter_('buy', symbol = 'triangle', symbolSize = 12, color = color_up, legend = FALSE) |> 
        e_scatter_('sell', symbol = 'triangle', symbolSize = 12, symbolRotate=180, color = color_down, legend = FALSE) 
    
    for (o in split(orders,by='type')) {
        for (i in o[,.I]) {
            e = e |>
                e_mark_line(
                    data = list(xAxis = o[i,date]), 
                    symbol = 'none', 
                    label = list(show=FALSE), 
                    lineStyle = list(type = 'dotted', color = 'gray')
                )
        }
    }
    return(e)
}
# lm
pp_dtlm = function(dt, x, y, yaxis_log = FALSE, nsd_lm = NULL) {
    rid = NULL 
    
    ylm = function(x, y, yaxis_log, num_sd) {
        if (!yaxis_log) {
            y_sd = dt[, sd(y, na.rm = TRUE)]
            predict(lm(y ~ x)) + num_sd * y_sd
        } else {
            y_sd = dt[, sd(log10(y), na.rm = TRUE)]
            10^(predict(lm(log10(y) ~ x)) + num_sd * y_sd)
        }
    }
    
    if (is.null(nsd_lm)) {
        return(dt)
    } else {
        dt = copy(setorderv( dt, c('symbol', x) ))[, rid := seq_len(.N), by = 'symbol']
        for (i in seq_along(nsd_lm)) {
            nsd = nsd_lm[i]
            dt = dt[, (paste0('lmy',i)) := ylm(get(x), get(y), yaxis_log = yaxis_log, num_sd = nsd), by = 'symbol']
        }
        return(dt)
    }
}
p_lm = function(e, x='date', y='close', nsd_lm = NULL) {
    if (is.null(nsd_lm)) {
        return(e)
    } else {
        for (lmyi in paste0('lmy',seq_along(nsd_lm))) {
            e = e |>
                e_line_(lmyi, legend=FALSE, symbol='none', 
                        lineStyle = list(type = 'dashed', width = 1))
        }
        return(e)
    }
}
# addti
fun_filter_overlays = function(addti) {
    names(addti) <- tolower(names(addti))
    ti_not_topbottom = names(addti)[sapply(addti, function(x) !any(x[['position']] %in% c('top','bottom')))]
    ti_overlay = intersect( ti_not_topbottom, tolower(pq_addti_funs()[['overlays']]) ) 
    return(ti_overlay)
}

p_addti_overlay = function(e, dt, addti = NULL) {
    if (is.null(addti)) return(e)
    # overlay: mm, sma, ema, smma, bb, sar
    # overlay technical indicators
    ti_overlay = fun_filter_overlays(addti)
    for (ti in ti_overlay) {
        serie_type = 'line'
        serie_symbol = 'none'
        if (ti %in% c('runmax', 'runmin')) {
            serie_type = 'step'
        } else if (ti == 'sar') {
            serie_type = 'scatter'
            serie_symbol = 'circle'
        }

        ti_cols = names(dt)[grep(sprintf('^%s', ti), names(dt))]
        for (ticol in ti_cols) {
            if (ticol %in% c('bbands_dn', 'bbands_up', 'pbands_dn', 'pbands_up')) next
            # tiname = sprintf('%s(%s)', gsub('[0-9._]+', '', ticol), gsub('_', ',', gsub('[a-z]+_', '', ticol)))
            e = do.call(
                sprintf('e_%s_', serie_type), 
                args = list(
                    e = e, serie = ticol, symbol = serie_symbol, 
                    lineStyle = list(width = 1)
                ))
        }
        
        if (ti == 'bbands') {
            e = e |>
                e_band2_('bbands_dn', 'bbands_up', color = 'lightgrey', itemStyle=list(borderWidth=0), legend = FALSE)
        } else if (ti == 'pbands') {
            e = e |>
                e_band2_('pbands_dn', 'pbands_up', color = 'lightgrey', itemStyle=list(borderWidth=0), legend = FALSE)
        }
    }
    return(e)
    # oscillator: macd, ppo, roc, rsi, cci
    
}

p_addti_indicator = function(e, dt, addti = NULL, x = 'date', theme = 'default') {
    if (is.null(addti)) return(e)
    
    ti_indicators = setdiff(tolower(names(addti)), fun_filter_overlays(addti))
    len_ti = length(ti_indicators)
    if (len_ti == 0) return(e)
    
    elst = lapply(
        ti_indicators, 
        function(ti) {
            ti_cols = names(dt)[grep(sprintf('^%s', ti), names(dt))]
            e = dt |> 
                e_charts_(x, height = 150, dispose = FALSE) 
            for (ticol in ti_cols) {
                 e = e |> 
                     e_line_(ticol, symbol='none', lineStyle = list(width = 1)) |> 
                     e_y_axis(position = 'right', axisLabel = list(rotate = 90, hideOverlap=TRUE)) |> 
                     e_legend(type = "plain", orient = "vertical", left='30', top='10') |> 
                     e_datazoom(show=FALSE) |>
                     e_toolbox(show=FALSE) |>
                     e_grid(show = TRUE, top = 10, bottom=20, 
                            left='30', right = '30') |>
                     e_tooltip(trigger='axis', axisPointer = list(type = 'line', show = TRUE)) |> 
                     e_group('ind') |>
                     e_theme(theme) 
            }
            return(e)
        }
    )
    elst[[len_ti]] = elst[[len_ti]]  |> 
        e_connect_group("ind")
    elst = c(list(e_group(e, 'ind')), elst)
    
    do.call('e_arrange', args = c(elst, list(cols=1)))
}

pp_base = function(dt, x = 'date', h='100%') {
    symbol = name = NULL 
    
    if (dt[,length(unique(symbol))] > 1) {
        e = dt |> 
            group_by(sybnam) |> 
            e_charts_(x) 
    } else {
        sybnam = dt[.N, paste(symbol, name)]
        e = dt |> 
            e_charts_(x) 
    }
}

pp_line = function(
    dt, x = 'date', y = 'close', date_range = 'max', yaxis_log = FALSE, title = NULL, 
    color_up = "#CF002F", color_down = "#000000", theme = 'default', 
    markline = TRUE, nsd_lm = NULL, addti = NULL, 
    orders = NULL, order_y = 'price', order_type = 'type', ...
) {
    dt = pp_dtpre(dt, x, y, addti, markline, orders, order_y, order_type) |>
        pp_dtlm(x, y, yaxis_log, nsd_lm)
    title  = pp_title(dt, title)
    xstart = pp_xstart(dt, x, date_range)
    
    
    e = pp_base(dt, x) |> 
        p_orders(orders, color_up, color_down) |>
        e_line_(serie = y, legend=TRUE, symbol='none') |>
        p_markline(dt = dt, markline = markline) |> 
        p_lm(x=x, y=y, nsd_lm=nsd_lm) |>
        p_addti_overlay(dt = dt, addti = addti) |>
        p_theme(xstart = xstart, xend = 100, yaxis_log = yaxis_log, title = title, theme = theme) |> 
        p_addti_indicator(dt = dt, addti = addti, x = x, theme = theme)
        
    return(e)
} 

pp_step = function(
    dt, x = 'date', y = 'close', date_range = 'max', yaxis_log = FALSE, title = NULL, 
    color_up = "#CF002F", color_down = "#000000", theme = 'default', 
    markline = TRUE, nsd_lm = NULL, addti = NULL, 
    orders = NULL, order_y = 'price', order_type = 'type', ...
) {
    dt = pp_dtpre(dt, x, y, addti, markline, orders, order_y, order_type) |>
        pp_dtlm(x, y, yaxis_log, nsd_lm)
    title  = pp_title(dt, title)
    xstart = pp_xstart(dt, x, date_range)
    
    e = pp_base(dt, x) |> 
        p_orders(orders, color_up, color_down) |> 
        e_step_(serie = y, symbol='none') |> 
        p_markline(dt = dt, markline = markline) |> 
        p_lm(x=x, y=y, nsd_lm=nsd_lm) |>
        p_addti_overlay(dt = dt, addti = addti) |>
        p_theme(xstart = xstart, xend = 100, yaxis_log = yaxis_log, title = title, theme = theme) |>
        p_addti_indicator(dt = dt, addti = addti, x = x, theme = theme)
    
    return(e)
} 

pp_candle = function(
    dt, x = 'date', y = 'close', date_range = 'max', yaxis_log = FALSE, title = NULL, 
    color_up = "#CF002F", color_down = "#000000", theme = 'default', 
    markline = TRUE, nsd_lm = NULL, addti = NULL, 
    orders = NULL, order_y = 'price', order_type = 'type', ...
) {
    dt = pp_dtpre(dt, x, y, addti, markline, orders, order_y, order_type) |>
        pp_dtlm(x, y, yaxis_log, nsd_lm)
    title  = pp_title(dt, title)
    xstart = pp_xstart(dt, x, date_range)
    
    dt = copy(dt)[, date := as.factor(date)]
    e = pp_base(dt, x) |> 
        p_orders(orders, color_up, color_down)  |> 
        e_candle_('open', 'close', 'low', 'high', 
                  itemStyle = list(color = color_up, borderColor = color_up,
                                   color0 = color_down, borderColor0 = color_down)) |> 
        p_markline(dt = dt, markline = markline) |> 
        p_lm(x=x, y=y, nsd_lm=nsd_lm) |>
        p_addti_overlay(dt = dt, addti = addti) |>
        p_theme(xstart = xstart, xend = 100, yaxis_log = yaxis_log, title = title, theme = theme) |> 
        p_addti_indicator(dt = dt, addti = addti, x = x, theme = theme)
    
    return(e)
}  


#' creating charts for time series
#' 
#' \code{pq_plot} provides an easy way to create interactive charts for time series dataset based on predefined formats.
#' 
#' @param dt a list/dataframe of time series dataset
#' @param chart_type chart type, including line, step, candle.
#' @param x column name of x axis 
#' @param y column name of y axis
#' @param date_range date range of x axis to display. Available value includes '1m'-'11m', 'ytd', 'max' and '1y'-'ny'. Default is max.
#' @param yaxis_log whether to display y axis values in log. Default is FALSE.
#' @param title chart title. It will added to the front of chart title if it is specified.
#' @param addti list of technical indicators or numerical columns in dt. For technical indicator, it is calculated via \code{pq_addti}, which including overlays and indicators.
#' @param nsd_lm number of standard deviation from linear regression fitting values. 
#' @param markline whether to display markline. Default is TRUE. 
#' @param orders the orders data, including transaction date, price and type. 
#' @param arrange a list. Number of rows and columns charts to connect. Default is NULL.
#' @param theme name of echarts theme, see details in \code{\link{e_theme}}
#' @param ... ignored
#' 
#' @examples 
#' \donttest{
#' # single serie
#' library(data.table)
#' library(pedquant)
#' data(dt_ssec)
#' 
#' # line chart (default)
#' e1 = pq_plot(dt_ssec, chart_type = 'line') # line chart (default)
#' e1[[1]]
#' 
#' # add technical indicators
#' e2 = pq_plot(dt_ssec, addti = list(
#'         sma = list(n = 200), 
#'         sma = list(n = 50), 
#'         volume = list(),
#'         macd = list()
#' ))
#' e2[[1]]
#' 
#' # linear trend with yaxis in log
#' e3 = pq_plot(dt_ssec, nsd_lm = c(-0.8, 0, 0.8), markline=FALSE)
#' e3[[1]]
#' 
#' # multiple series
#' data(dt_banks)
#' dt_banksadj = md_stock_adjust(dt_banks, adjust = TRUE)
#' 
#' # linear trend
#' elist = pq_plot(dt_banksadj)
#' e4 = pq_plot(dt_banksadj, arrange = list(rows=1, cols=1))
#' e4[[1]]
#' 
#' # orders 
#' boc = dt_banks[symbol == '601988.SS']
#' bocorders = boc[sample(.N, 10), .(date, price=close, 
#'                 type=sample(c('buy','sell'), 10, replace=TRUE))]
#' e5 = pq_plot(boc, orders=bocorders)
#' e5[[1]]
#' 
#' }
#' 
#' @import echarts4r 
#' @importFrom stats lm sd predict
#' @export
pq_plot = function(
    dt, chart_type = 'line', x = 'date', y = 'close', 
    date_range = 'max', yaxis_log = FALSE, title = NULL, 
    addti = NULL, nsd_lm = NULL, markline = TRUE, orders = NULL, 
    arrange = list(rows=NULL, cols=NULL), 
    theme = 'default', 
    ...) {
    symbol = NULL
    # color_up = "#CF002F", color_down = "#000000", 
    # order_y = 'price', order_type = 'type', 
    if (!interactive()) return(invisible())
    # arguments
    args = list(...)
    if (!is.null(args[['multi_series']])) {
        ms = args[['multi_series']]
        arrange = list(rows = ms$nrow, cols=ms$ncol)
    }
    if (!is.null(args[['linear_trend']])) {
        nsd_lm = args[['linear_trend']]
    }
    if (!(chart_type %in% c('line', 'step', 'candle'))) {
        chart_type = 'line'
        warning('The chart_type has set to line.')
    }
    
    # arrange row col
    arrange_rowcol_allnull = all(sapply(arrange, is.null))
    arrange_rowcol_all1 = all(sapply(list('rows', 'cols'), function(x) any(arrange[[x]] == 1)))
    
    dt = check_dt(dt)
    arglst = list(dt=dt, x=x, y=y, date_range=date_range, yaxis_log=yaxis_log, title=title, addti=addti, nsd_lm=nsd_lm, markline=markline, orders=orders, arrange=arrange, theme=theme, ...)
    
    if (arrange_rowcol_all1) {
        if (dt[,length(unique(symbol))>1] & chart_type=='candle') {
            chart_type = 'line'
            warning('Multible candlestick series on one graphic is not supported yet. The chart_type has set to the default (line).')
        }
        e = do.call(sprintf('pp_%s', chart_type), args = arglst)
    } else {
        e = lapply(split(dt, by = 'symbol'), function(dat) {
            arglst[['dt']] <- dat
            do.call(sprintf('pp_%s', chart_type), args = arglst)
        })
        
        if (!arrange_rowcol_allnull) e = do.call('e_arrange', args = c(e, arrange))
    }
    if (!inherits(e, 'list')) e = list(e)
    
    return(e)
}