# ref 
# - [Introduction to Candlesticks](https://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:introduction_to_candlesticks)
# - [ggplot-for-ohlc-chart-using-bars-not-candles](https://stackoverflow.com/questions/28201587/ggplot-for-ohlc-chart-using-bars-not-candles)

# color_up, color_down
# - stockcharts: "black", "#CF002F"
# - A share: "red", "green"
# - ggplot2: "#18C0C4", "#F6736D"
# rgb()

# candlstick
pp_candle = function(dt, color_up = "#F6736D", color_down = "#18C0C4", title = title, 
                     rm_weekend = TRUE, rm_xaxis = FALSE, yaxis_log = FALSE, y = "open|high|low|close", 
                     linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
                     x_scale = 0.6, ...) {
    prev_close = change = change_pct = high = low = x = updn_1day = updn_2day = symbol = V1 = NULL
    
    dt = copy(dt)
    if (!is.logical(rm_weekend)) rm_weekend = TRUE
    # x and updn
    if (rm_weekend) dt[, x := rowid]
    dt[, updn_1day := ifelse(close > open, "hollow", paste("filled", updn_2day, sep = "_"))]
    
    # subtitle string
    subtitle_str = 
        dt[, .SD[.N], by = symbol
           ][, subtitle_str := sprintf("atop(bold('O')~'%.2f'~bold('H')~'%.2f'~bold('L')~'%.2f'~bold('C')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", open, high, low, close, change, change_pct), by = symbol]
    
    
    # plot
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = all(sapply(multi_series, function(x) any(x==1)))
    
    if (!multi_series_all1) {
        p = ggplot(dt, aes(color = updn_2day)) +
            geom_segment(aes(x = x, y = low, xend = x, yend = high)) + 
            geom_rect(aes(xmin = x - x_scale / 2, xmax = x + x_scale / 2, ymin = open, ymax = close, fill = updn_1day)) + 
            scale_fill_manual(values = c("hollow" = "white", "filled_up" = color_up, "filled_down" = color_down)) +
            scale_color_manual(values = c("up" = color_up, "down" = color_down)) + 
            geom_text(x = dt[1, x], y = Inf, aes(label = date), data = dt[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = "gray", na.rm = TRUE) +
            geom_text(x = dt[.N, x], y = Inf, aes(label = subtitle_str), data=subtitle_str, hjust = 1, vjust = 1, color = "gray", parse = TRUE, na.rm = TRUE) +
            annotate("text", label="@getpedr", x=dt[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
            guides(fill = FALSE, color = FALSE) + 
            labs(x=NULL, y=NULL) + theme_bw()
        
        # facet 
        if (num_syb>1) {
            p = p + facet_wrap(~ symbol, nrow = multi_series$nrow, ncol = multi_series$ncol, scales = "free_y")
        }
        
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt=dt, rm_weekend=rm_weekend, linear_trend=linear_trend))
    }
    
    
    # set xaxis
    if (rm_xaxis) {
        p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else {
        p = pp_set_xaxis(p, dt, rm_weekend = rm_weekend)
    }
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dt, title = title)
    
                
    return(p)
}

# bar
pp_bar    = function(dt, color_up = "#F6736D", color_down = "#18C0C4", title = title, 
                     rm_weekend = TRUE, rm_xaxis = FALSE, yaxis_log = FALSE, y = "open|high|low|close", 
                     linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), 
                     x_scale = 0.6, ...) {
    prev_close = change = change_pct = high = low = x = updn_2day = symbol = V1 = NULL
    
    dt = copy(dt)
    if (!is.logical(rm_weekend)) rm_weekend = TRUE
    # x
    if (rm_weekend) dt[, x := rowid]
    
    
    # # add prev close, change and change_pct
    # if (!("prev_close" %in% names(dt))) dt = dt[, prev_close := shift(close, 1, type = "lag")]
    # if (!("change"     %in% names(dt))) dt = dt[, change := close - prev_close]
    # if (!("change_pct" %in% names(dt))) dt = dt[, change_pct := change/close*100]
    
    # subtitle string
    subtitle_str = 
        dt[, .SD[.N], by = symbol
           ][, subtitle_str := sprintf("atop(bold('O')~'%.2f'~bold('H')~'%.2f'~bold('L')~'%.2f'~bold('C')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", open, high, low, close, change, change_pct), by = symbol]
    
    
    # plot
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = all(sapply(multi_series, function(x) any(x==1)))
    
    if (!multi_series_all1) {
        p = ggplot(dt, aes(color = updn_2day)) + 
            geom_linerange(aes(x = x, ymin = low, ymax = high)) +
            geom_segment(aes(x = x, y = open, xend = x - x_scale / 2, yend = open)) + 
            geom_segment(aes(x = x, y = close, xend = x + x_scale / 2, yend = close)) + 
            scale_color_manual(values = c("up" = color_up, "down" = color_down)) +
            geom_text(x = dt[1, x], y = Inf, aes(label = date), data = dt[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = "gray", na.rm = TRUE) +
            geom_text(x = dt[.N, x], y = Inf, aes(label = subtitle_str), data=subtitle_str, hjust = 1, vjust = 1, color = "gray", parse = TRUE, na.rm = TRUE) +
            annotate("text", label="@getpedr", x=dt[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
            guides(color = FALSE) + 
            labs(x=NULL, y=NULL) + theme_bw()
        
        # facet 
        if (num_syb>1) {
            p = p + facet_wrap(~ symbol, nrow = multi_series$nrow, ncol = multi_series$ncol, scales = "free_y")
        }
        
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt=dt, rm_weekend=rm_weekend, linear_trend=linear_trend))
    }
    
    
    # set xaxis
    if (rm_xaxis) {
        p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else {
        p = pp_set_xaxis(p, dt, rm_weekend = rm_weekend)
    }
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dt, title = title)
    
    
    return(p)
}

# line
pp_line   = function(dt, color_up = "#F6736D", color_down = "#18C0C4", title = title, 
                     rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, y = "close|value", 
                     linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), ...) {
    prev_close = symbol = change = change_pct = high = low = x = prev_x = updn_2day = x1 = y1 = x2 = y2 = V1 = NULL
    
    dt = copy(dt)
    if (!is.logical(rm_weekend)) rm_weekend = FALSE
    # x and updn
    if (rm_weekend) dt[, x := rowid]
    dt[, prev_x := shift(x, 1, type="lag"), by = symbol
       ][updn_2day=="down", `:=`(x1 = prev_x, x2 = x, y1 = prev_close, y2 = close)]
    
    
    # subtitle string
    if (all(c("open","high","low","close") %in% names(dt))) {
        subtitle_str = 
            dt[, .SD[.N], by = symbol
               ][, subtitle_str := sprintf("atop(bold('O')~'%.2f'~bold('H')~'%.2f'~bold('L')~'%.2f'~bold('C')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", open, high, low, close, change, change_pct), by = symbol]
    } else {
        subtitle_str = 
            dt[, .SD[.N], by = symbol
               ][, subtitle_str := sprintf("atop(bold('%s')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", y, close, change, change_pct), by = symbol]
    }
    
    
    
    
    
    # plot
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = all(sapply(multi_series, function(x) any(x==1)))
    
    if (multi_series_all1) {
        p = ggplot(data = dt) + 
            geom_line(aes(x = x, y = close, color = symbol)) + 
            scale_color_discrete(labels = subtitle_str[, paste(symbol, date, round(close,2), sep=", ")]) + 
            annotate("text", label="@getpedr", x=dt[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
            labs(x=NULL, y=NULL, color=NULL) + theme_bw() +
            theme(
                legend.position = c(0,1), 
                legend.justification=c(0,1),
                legend.background = element_blank(), 
                legend.key = element_blank()
            )
    } else {
        p = ggplot(dt) + 
            geom_line(aes(x = x, y = close), color = color_up) +
            geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = color_down, na.rm = TRUE) +
            geom_text(x = dt[1, x], y = Inf, aes(label = date), data = dt[,.SD[.N], by=symbol], hjust = 0, vjust = 1, color = "gray", na.rm = TRUE) +
            geom_text(x = dt[.N, x], y = Inf, aes(label = subtitle_str), data=subtitle_str, hjust = 1, vjust = 1, color = "gray", parse = TRUE, na.rm = TRUE) +
            annotate("text", label="@getpedr", x=dt[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
            labs(x=NULL, y=NULL) + theme_bw()
        
        # facet 
        if (num_syb>1) {
            p = p + facet_wrap(~ symbol, nrow = multi_series$nrow, ncol = multi_series$ncol, scales = "free_y")
        }
        
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt=dt, rm_weekend=rm_weekend, linear_trend=linear_trend))
    }
    
    # set xaxis
    if (rm_xaxis) {
        p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else {
        p = pp_set_xaxis(p, dt, rm_weekend = rm_weekend)
    }
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dt, title = title)
    
    return(p)
}

# step
pp_step   = function(dt, color_up = "#F6736D", color_down = "#18C0C4", title = title, 
                     rm_weekend = FALSE, rm_xaxis = FALSE, yaxis_log = FALSE, y = "close|value", 
                     linear_trend = NULL, multi_series = list(nrow=NULL,ncol=NULL), ...) {
    prev_close = symbol = change = change_pct = high = low = x = prev_x = updn_2day = x1 = y1 = x2 = y2 = V1 = NULL
    
    dt = copy(dt)
    if (!is.logical(rm_weekend)) rm_weekend = FALSE
    # x and updn
    if (rm_weekend) dt[, x := rowid]
    dt[, prev_x := shift(x, 1, type="lag"), by = symbol
       ][updn_2day=="down", `:=`(x1 = prev_x, x2 = x, y1 = prev_close, y2 = close)]
    
    
    # subtitle string
    if (all(c("open","high","low","close") %in% names(dt))) {
        subtitle_str = 
            dt[, .SD[.N], by = symbol
               ][, subtitle_str := sprintf("atop(bold('O')~'%.2f'~bold('H')~'%.2f'~bold('L')~'%.2f'~bold('C')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", open, high, low, close, change, change_pct), by = symbol]
    } else {
        subtitle_str = 
            dt[, .SD[.N], by = symbol
               ][, subtitle_str := sprintf("atop(bold('%s')~'%.2f'~bold('Chg')~'%.2f(%.2f%%)')", y, close, change, change_pct), by = symbol]
    }
    
    
    # last row
    dt_N = dt[,.SD[.N], by=symbol]
    # add new row
    dtN_update = copy(dt_N)[, `:=`(
        date = Sys.Date(), rowid = rowid+1, prev_x = x
    )][, x := sapply(x, function(x) {
        x = ifelse(rm_weekend, rowid, date)
        return(x)
    })]
    dt = rbindlist(list(dt, dtN_update), fill = TRUE)
    setkeyv(dt, c("symbol", "date"))
    
    # plot
    num_syb = dt[, length(unique(symbol))]
    multi_series_all1 = all(sapply(multi_series, function(x) any(x==1)))
    
    if (multi_series_all1) {
        p = ggplot(data = dt) + 
            geom_step(aes(x = x, y = close, color = symbol)) + 
            scale_color_discrete(labels = subtitle_str[, paste(symbol, date, round(close,2), sep=", ")]) + 
            annotate("text", label="@getpedr", x=dt[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
            labs(x=NULL, y=NULL, color=NULL) + theme_bw() +
            theme(
                legend.position = c(0,1), 
                legend.justification=c(0,1),
                legend.background = element_blank(), 
                legend.key = element_blank()
            )
    } else {
        p = ggplot(dt) + 
            geom_step(aes(x = x, y = close), color = color_up) +
            geom_segment(aes(x = x1, y = y1, xend = x2, yend = y1), color = color_down, na.rm = TRUE) +
            geom_segment(aes(x = x2, y = y1, xend = x2, yend = y2), color = color_down, na.rm = TRUE) +
            geom_text(x = dt[1, x], y = Inf, aes(label = date), data=dt_N, hjust = 0, vjust = 1, color = "gray", na.rm = TRUE) +
            geom_text(x = dt[.N, x], y = Inf, aes(label = subtitle_str), data=subtitle_str, hjust = 1, vjust = 1, color = "gray", parse = TRUE, na.rm = TRUE) +
            annotate("text", label="@getpedr", x=dt[, x[.N %/% 2], by=symbol][,V1[1]], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
            labs(x=NULL, y=NULL) + theme_bw()
        
        # facet 
        if (num_syb>1) {
            p = p + facet_wrap(~ symbol, nrow = multi_series$nrow, ncol = multi_series$ncol, scales = "free_y")
        }
        
        # add linear trend line
        if (!is.null(linear_trend)) 
            p = do.call(pp_add_linear_trend, args = list(p=p, dt=dt, rm_weekend=rm_weekend, linear_trend=linear_trend))
    }
    
    
    # set xaxis
    if (rm_xaxis) {
        p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else {
        p = pp_set_xaxis(p, dt, rm_weekend = rm_weekend)
    }
    # set yaxis
    p = pp_set_yaxis(p, yaxis_log = yaxis_log)
    # set title 
    p = pp_set_title(p, dt, title = title)
    
    
    return(p)
}



# set xaxis 
pp_set_xaxis = function(p, dt, rm_weekend = TRUE, ...) {
    . = yl = ql = ml = wl = y = m = w = d = brks = freq = NULL
    
    xfreqs = c("y", "q", "m", "w")
    
    xticks = dt[
        , .(date = unique(date))
    ][, `:=`(
        y = year(date), q=quarter(date), m = month(date), w = isoweek(date), d = mday(date), rowid = .I
    )][, (paste0(xfreqs,"l")) := lapply(.SD, function(x) {
        xi = x - shift(x, 1, type="lag")
        xi[xi != 0] <- 1
        return(xi)
    }), .SDcols = xfreqs
    ][yl==1 | ql==1 | ml==1 | wl==1, .(date,rowid, y,q,m,w,d, yl,ql,ml,wl)]
    
    # breaks and labels
    xbrk_lab = data.table()
    xbrk_nrow = xbrk_lab[,.N]
    num_xfreqs = 1
    while (xbrk_nrow <= 15 & num_xfreqs <= 3) {
        xfreq = xfreqs[num_xfreqs]
        xlab = paste0(xfreq,"l")
        if (xfreq == "q") {
            xfreq = "m"
        } else if (xfreq == "w") {
            xfreq = "d"
        }
        # print(xfreq)
        
        xbrk_lab = rbind(
            xbrk_lab, 
            xticks[eval(parse(text = xlab)) == 1, .(date, brks = rowid, labs = eval(parse(text = xfreq)), freq=xfreqs[num_xfreqs])])
        xbrk_lab = unique(xbrk_lab, by = "brks", fromLast = FALSE) 
        
        xbrk_nrow = xbrk_lab[, .N] + xticks[eval(parse(text=xlab)) == 1, .N]
        num_xfreqs = num_xfreqs+1
    }
    xbrk_lab = xbrk_lab[order(brks)]
    main_brks = xbrk_lab
    
    if (xfreq == "y") {
        brk_num = c(2, 5, 10, 25, 50, 100)
        i = 1
        while (main_brks[,.N] > 15) {
            minor_brks = main_brks
            main_brks  = main_brks[, rowid := .I][rowid %% brk_num[i] == 0]
            i = i+1
        }
    }
    if (!exists("minor_brks")) {
        # minor_breaks
        xfreq = xfreqs[num_xfreqs]
        xlab = paste0(xfreq,"l")
        if (xfreq == "q") {
            xfreq = "m"
        } else if (xfreq == "w") {
            xfreq = "d"
        }
        
        minor_brks = xticks[
            eval(parse(text = xlab)) == 1, 
            .(date, brks = rowid, labs = eval(parse(text = xfreq)))]
    }
    
    
    # set x axis
    if (rm_weekend) {
        p = p + scale_x_continuous(
            breaks = main_brks[, brks], labels = main_brks[, labs],
            expand = expand_scale(add=1), 
            minor_breaks = minor_brks[, brks]) + 
            theme(axis.text.x = element_text(face = main_brks[, ifelse(freq=="y", "bold", ifelse(freq=="q", "bold.italic", "plain"))]))
    } else {
        p = p + scale_x_date(
            breaks = main_brks[, date], labels = main_brks[, labs],
            expand = expand_scale(add=1), 
            minor_breaks = minor_brks[, date]) + 
            theme(axis.text.x = element_text(face = main_brks[, ifelse(freq=="y", "bold", ifelse(freq=="q", "bold.italic", "plain"))]))
    }
    
    return(p)
}
# set yaxis
pp_set_yaxis = function(p, yaxis_log = FALSE) {
    if (yaxis_log) {
        p = p + scale_y_continuous(trans='log10', sec.axis = dup_axis()) +
            #     scale_y_log10(
            #     breaks = trans_breaks("log10", function(x) 10^x),
            #     labels = trans_format("log10", math_format(10^.x)),
            #     position = "right"
            # ) +
            annotation_logticks(sides = "rl") 
    } else {
        p = p + scale_y_continuous(sec.axis = dup_axis()) 
    }
    p = p + theme(
        axis.text.y.left  = element_text(angle = -90), 
        axis.text.y.right = element_text(angle =  90)
    )
    
    return(p)
}
# add linear trend line
pp_add_linear_trend = function(p, dt, rm_weekend = TRUE, linear_trend = c(0, 1, -1), ...) {
    x = NULL
    add1_linear_trend = function(p, dt, num_sd, rm_weekend) {
        symbol = NULL
        
        # add x column
        if (!("x" %in% names(dt))) {
            dt[, x := date]
            if (rm_weekend) dt[, x := seq_len(.N), by=symbol]
        }
        
        # line color
        line_color = "blue"
        if (num_sd == 0) line_color = "red"
        
        # plot
        p = p + 
            geom_smooth(data = dt, aes(x = x, y = close), 
                        method = lm, formula = y+num_sd*sd(y, na.rm = TRUE)~x, 
                        na.rm = TRUE, se = FALSE, color = line_color, size = 0.2)
        
        return(p)
    }
    
    for (i in linear_trend) p = add1_linear_trend(p, dt, i, rm_weekend)
    
    return(p)
}
# set title
pp_set_title = function(p, dt, title = NULL) {
    symbol = NULL
    
    if (is.null(title)) title = ""
    
    # plot title 
    if ("symbol" %in% names(dt) & dt[, length(unique(symbol))]==1) {
        title = paste(dt[1, symbol], title)
    }
    # if ("name" %in% names(dt)) title = paste(dt[1, name], title)
    
    # title string
    title_str = sprintf("[%s/%s] %s", dt[1,date], dt[.N,date], title)
    
    # adding title and annotation
    p = p + 
        ggtitle(label = title_str) + 
        theme(plot.title = element_text(margin = margin(b=0))) 
    
    return(p)
}



# add technical indicators
pp_add_ti = function(p, dt, ti = list()) {
    
}



#' create a chart for timeseries data
#' 
#' `ped_plot` creates a charts for a time series dataset. 
#' 
#' @param dt a time series dataset
#' @param chart_type chart type, including line, step, candle.
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is max.
#' @param from the start date. Default is max date in input data.
#' @param to the end date. Default is min date in data.
#' @param y the variable display on chart
#' @param yaxis_log logical, default is FALSE.
#' @param color_up the color indicates price going up
#' @param color_down the color indicates price going down
#' @param linear_trend a vector of numeric. Default is NULL, which donot show linear trend line. If it is not 0, show linear_trend*sd from linear trend line. 
#' @param perf logical, whether to show the performance of input data. Default is FALSE. 
#' @param multi_series logical, whether to show multiple series. Default is FALSE. 
#' @param rm_weekend weather to remove weekend in xaxis. The default is TRUE for candle and bar chart, and FALSE for line and step chart.
#' @param title chart title. If it is not specified, the symbol of dataset will be used as chart title.
#' 
#' @examples 
#' \dontrun{
#' ssec = getmd("^000001", source="163")
#' 
#' p = ped_plot(ssec, title="SSEC")
#' print(p)
#' }
#' 
#' @import ggplot2
#' @export
ped_plot = function(
    dt, chart_type = "line", 
    date_range="max", from = NULL, to = Sys.Date(), 
    y = "close|value", yaxis_log = FALSE, 
    color_up = "#F6736D", color_down = "#18C0C4", 
    linear_trend = NULL, 
    multi_series = list(nrow=NULL, ncol=NULL), perf = FALSE, 
    rm_weekend = NULL, 
    title = NULL) {
    
    prev_close = updn_2day = symbol = NULL
    # check arguments
    ## chart type
    chart_type = check_arg(chart_type, c("candle", "bar", "line", "step"), default = "line")
    ## date range
    date_range = check_date_range(date_range, default = "max")
    
    # change data to performance
    if (perf) {
        if (chart_type %in% c("candle", "bar")) {
            chart_type = "line"
            warning("The `chart_type` should be line or step, if you want to display performance trend. It is set to 'line'.")
        }
        dt = ped_perf(dt, y=y, date_range=date_range, from=from, to=to)
        title = paste(title, "perf")
    }
    # bind list of dataframes
    if (is.list(dt) & !is.data.frame(dt)) {
        dt = rbindlist(dt, fill = TRUE)
    }
    # have open, close; no high, low;
    if (all(c("open", "close") %in% names(dt))) {
        if (!("high" %in% names(dt))) dt[["high"]] <- NA
        if (!("low" %in% names(dt))) dt[["low"]] <- NA
    }
    # y
    y = names(dt)[grepl(y, names(dt))]
    if (anyNA(y)) {
        stop("The specified y is not existed in dataframe.")
    } else if (length(y) > 1) {
        y = y[1]
        if (y != "close") {
            dt[["close"]] = dt[[y]]
            warning(sprintf("%s is choosen to display on the chart.", y[1]))
        }
    }
    
    # add columns
    dt[, prev_close := shift(close, 1, type = "lag"), by = symbol
     ][, `:=`(
         change = close - prev_close,
         change_pct = (1-prev_close/close)*100,
         x = date
     )][, `:=`(
         updn_2day = ifelse(close > prev_close, "up", "down"), 
         rowid = seq_len(.N)
     ), by = symbol]
    if (all(c("close", "open") %in% names(dt))) dt[is.na(updn_2day), updn_2day := ifelse(close > open, "up", "down")]
    
    # plot graphic
    multi_series_allnull = all(sapply(multi_series, is.null))
    multi_series_all1 = all(sapply(multi_series, function(x) any(x==1)))
    
    plist = list()
    if (!multi_series_allnull) {
        if (multi_series_all1) {
            if (chart_type %in% c("candle", "bar")) {
                chart_type = "line"
                warning("The `chart_type` should be line or step, if you want to display multiple series in one chart. It is set to the default 'line'.")
            }
        }
        
        
        ## multiple series
        title = "multiple series"
        plist[["multi_series"]] = 
            do.call(paste0("pp_",chart_type), args = list(
                dt = dt, 
                date_range = date_range, from = from, to = to, 
                y = y, yaxis_log = yaxis_log, 
                color_up = color_up, color_down = color_down, 
                rm_weekend = rm_weekend, title = title, 
                multi_series = multi_series,
                linear_trend = linear_trend
            ))
    } else {
        if (!all(c("open", "high", "low", "close") %in% names(dt))) {
            if (chart_type %in% c("candle", "bar")) {
                chart_type = "line"
                warning("The `chart_type` is set to 'line'.")
            }
        }
        
        ## single series
        sybs = dt[, unique(symbol)]
        for (s in sybs) {
            dt_s = dt[symbol == s]
            setkeyv(dt_s, "date")
            
            plist[[s]] = 
                do.call(paste0("pp_",chart_type), args = list(
                    dt = dt_s, 
                    date_range = date_range, from = from, to = to, 
                    y = y, yaxis_log = yaxis_log, 
                    color_up = color_up, color_down = color_down, 
                    rm_weekend = rm_weekend, title = title, 
                    multi_series = multi_series,
                    linear_trend = linear_trend
                ))
        }
    }
    
    
    
    
    return(plist)
}


#' @importFrom stats lm predict
ped_plot_rp = function(dt, type = "line", title=NULL) {
    log_close = ggplot = geom_line = aes = lm0 = lm_p2 = lm_p1 = lm_m1 = scale_y_continuous = scale_x_date = labs = theme_bw = NULL
    
    
    dt = dt[, log_close := log10(close)
            ][, `:=`(
                lm0 = predict(lm(log_close~date)),
                lm_p2 = predict(lm((log_close+sd(log_close)*2)~date)),
                lm_p1 = predict(lm((log_close+sd(log_close))~date)),
                lm_m1 = predict(lm((log_close-sd(log_close))~date))
                # log_close_p2sd = log_close + sd(log_close)*2,
                # log_close_p1sd = log_close + sd(log_close),
                # log_close_m1sd = log_close - sd(log_close)
            )]
    
    title_str = sprintf("[%s/%s]", dt[,date[1]], dt[,date[.N]])
    if (!is.null(title)) title_str = paste(title_str,title)
    ggplot(data = dt) + 
        geom_line(aes(x=date, y=log_close)) +
        geom_line(aes(date, lm0), color="red") +
        geom_line(aes(date, lm_p2), color="grey") +
        geom_line(aes(date, lm_p1), color="grey") +
        geom_line(aes(date, lm_m1), color="grey") + 
        scale_y_continuous(position = "right") +
        scale_x_date(breaks = seq(as.Date("1990-01-01"), as.Date("2020-01-01"), by="5 year"), labels = seq(1990,2020,5)) +
        labs(title=title_str, x=NULL) +
        theme_bw()
}
# ssec_real = ped_rp1(getmd("^000001", source="163", from="1900-01-01"))
# ped_plot_rp(ssec_real, title="ssec")
# szsec_real = ped_rp1(getmd("^399001", source="163", from="1900-01-01"))
# ped_plot_rp(szsec_real, title="szsec")
# ped_plot(szsec_real, title="szsec")

