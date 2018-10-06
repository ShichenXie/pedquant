# ref 
# - [Introduction to Candlesticks](https://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:introduction_to_candlesticks)
# - [ggplot-for-ohlc-chart-using-bars-not-candles](https://stackoverflow.com/questions/28201587/ggplot-for-ohlc-chart-using-bars-not-candles)

# color_up, color_down
# - stockcharts: "black", "#CF002F"
# - A share: "red", "green"
# - ggplot2: "#18C0C4", "#F6736D"
# rgb()

# candlstick
ped1_candle = function(dt, x_scale = 0.6, color_up = "#F6736D", color_down = "#18C0C4", rm_weekend = TRUE, rm_xaxis = FALSE, linear_trend = NULL, ...) {
    dt = copy(dt)
    if (!is.logical(rm_weekend)) rm_weekend = TRUE

    # add prev close
    if (!("prev_close" %in% names(dt))) dt = dt[, prev_close := shift(close, n=1, type = "lag")]
    # add change and change_pct column into data
    if (!("change"     %in% names(dt))) dt = dt[, change := close - prev_close]
    if (!("change_pct" %in% names(dt))) dt = dt[, change_pct := change/close*100]
    if (all(c("change", "change_pct") %in% names(dt))) chg_str = sprintf("'%s(%s%%)'", dt[.N,round(change,2)], dt[.N,round(change_pct,2)])
    
    # subtitle string
    subtitle_str = paste(
        paste0("bold('", c("O","H","L","C", "Chg"), "')"), 
        c(dt[.N, sapply(.SD, round, 2), .SDcols = c("open","high","low","close")], chg_str), 
        sep = "~")
    subtitle_str = paste0("atop(", paste(subtitle_str, collapse = "~"),")")
    
    # add up down columns
    dt[, `:=`(
        cl_clprev = ifelse(close > prev_close, "up", "down"), 
        rowid = .I, 
        x = date
    )]
    if (rm_weekend) dt[, x := rowid]
    dt[, cl_op := ifelse(close > open, "hollow", paste("filled", cl_clprev, sep = "_"))
     ][is.na(cl_clprev), cl_clprev := ifelse(close > open, "up", "down")]
    
    # plot
    p = ggplot(dt, aes(color = cl_clprev)) +
        geom_segment(aes(x = x, y = low, xend = x, yend = high)) + 
        geom_rect(aes(xmin = x - x_scale / 2, xmax = x + x_scale / 2, ymin = open, ymax = close, fill = cl_op)) + 
        scale_fill_manual(values = c("hollow" = "white", "filled_up" = color_up, "filled_down" = color_down)) +
        scale_color_manual(values = c("up" = color_up, "down" = color_down)) + 
        annotate("text", x = dt[1, x], y = Inf,
                 label = dt[.N, date], hjust = 0, vjust = 1, color = "gray") + 
        annotate("text", x = dt[.N, x], y = Inf,
                 label = subtitle_str, hjust = 1, vjust = 1, color = "gray", parse = TRUE) +
        annotate("text", label="@getpedr", x=dt[.N %/% 2, x], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
        guides(fill = FALSE, color = FALSE) + 
        labs(x=NULL, y=NULL) + theme_bw()
    
    # set xaxis
    if (rm_xaxis) {
        p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else {
        p = ped1_set_xaxis(p, dt, rm_weekend = rm_weekend)
    }
    
    # add linear trend line
    if (!is.null(linear_trend)) do.call(ped1_add_linear_trend, args = list(p=p, dt=dt, rm_weekend=rm_weekend, linear_trend=linear_trend))
                
    return(p)
}

# bar
ped1_bar    = function(dt, x_scale = 0.6, color_up = "#F6736D", color_down = "#18C0C4", rm_weekend = TRUE, rm_xaxis = FALSE, linear_trend = NULL, ...) {
    dt = copy(dt)
    if (!is.logical(rm_weekend)) rm_weekend = TRUE
    
    # add prev close
    if (!("prev_close" %in% names(dt))) dt = dt[, prev_close := shift(close, 1, type = "lag")]
    # add change and change_pct column into data
    if (!("change"     %in% names(dt))) dt = dt[, change := close - prev_close]
    if (!("change_pct" %in% names(dt))) dt = dt[, change_pct := change/close*100]
    if (all(c("change", "change_pct") %in% names(dt))) chg_str = sprintf("'%s(%s%%)'", dt[.N,round(change,2)], dt[.N,round(change_pct,2)])
    
    # subtitle string
    subtitle_str = paste(
        paste0("bold('", c("O","H","L","C", "Chg"), "')"), 
        c(dt[.N, sapply(.SD, round, 2), .SDcols = c("open","high","low","close")], chg_str), 
        sep = "~")
    subtitle_str = paste0("atop(", paste(subtitle_str, collapse = "~"),")")
    
    # add up down columns
    dt[, `:=`(
        cl_clprev = ifelse(close > prev_close, "up", "down"), 
        rowid = .I, 
        x = date
    )]
    if (rm_weekend) dt[, x := rowid]
    
    # plot
    p = ggplot(dt, aes(color = cl_clprev)) + 
        geom_linerange(aes(x = x, ymin = low, ymax = high)) +
        geom_segment(aes(x = x, y = open, xend = x - x_scale / 2, yend = open)) + 
        geom_segment(aes(x = x, y = close, xend = x + x_scale / 2, yend = close)) + 
        scale_color_manual(values = c("up" = color_up, "down" = color_down)) +
        annotate("text", x = dt[1, x], y = Inf,
                 label = dt[.N, date], hjust = 0, vjust = 1, color = "gray") + 
        annotate("text", x = dt[.N, x], y = Inf,
                 label = subtitle_str, hjust = 1, vjust = 1, color = "gray", parse = TRUE) +
        annotate("text", label="@getpedr", x=dt[.N %/% 2, x], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
        guides(color = FALSE) + 
        labs(x=NULL, y=NULL) + theme_bw()
    
    # set xaxis
    if (rm_xaxis) {
        p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else {
        p = ped1_set_xaxis(p, dt, rm_weekend = rm_weekend)
    }
    
    # add linear trend line
    if (!is.null(linear_trend)) do.call(ped1_add_linear_trend, args = list(p=p, dt=dt, rm_weekend=rm_weekend, linear_trend=linear_trend))
    
    return(p)
}

# line
ped1_line   = function(dt, color_up = "#F6736D", color_down = "#18C0C4", rm_weekend = FALSE, rm_xaxis = FALSE, y = "close|value", linear_trend = NULL, ...) {
    dt = copy(dt)
    if (!is.logical(rm_weekend)) rm_weekend = FALSE
    
    y = names(dt)[grepl(y, names(dt))][1]
    if (is.na(y)) stop("The specified y is not existed.")
    if (y != "close") dt[["close"]] = dt[[y]]
    
    # add prev close
    if (!("prev_close" %in% names(dt))) dt = dt[, prev_close := shift(close, 1, type = "lag")]
    # add change and change_pct column into data
    if (!("change"     %in% names(dt))) dt = dt[, change := close - prev_close]
    if (!("change_pct" %in% names(dt))) dt = dt[, change_pct := change/close*100]
    if (all(c("change", "change_pct") %in% names(dt))) chg_str = sprintf("'%s(%s%%)'", dt[.N,round(change,2)], dt[.N,round(change_pct,2)])
    
    # subtitle string
    if (all(c("open","high","low","close") %in% names(dt))) {
        subtitle_str = paste(
            paste0("bold('", c("O","H","L","C", "Chg"), "')"), 
            c(dt[.N, sapply(.SD, round, 2), .SDcols = c("open","high","low","close")], chg_str), 
            sep = "~")
    } else {
        subtitle_str = paste(
            paste0("bold('", c(y, "Chg"), "')"), 
            c(dt[.N, sapply(.SD, round, 2), .SDcols = c(y)], chg_str), 
            sep = "~")
    }
    subtitle_str = paste0("atop(", paste(subtitle_str, collapse = "~"),")")
    
    # add up down columns
    dt[, `:=`(
        cl_clprev = ifelse(close > prev_close, "up", "down"), 
        rowid = .I, 
        x = date
    )]
    if (rm_weekend) dt[, x := rowid]
    dt[, prev_x := shift(x, 1, type="lag")
     ][cl_clprev=="down", `:=`(x1 = prev_x, x2 = x)
     ][cl_clprev=="down", `:=`(y1 = prev_close, y2 = close)]
    
    
    # plot
    p = ggplot(dt) + 
        geom_line(aes(x = x, y = close), color = color_up) +
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), color = color_down, na.rm = TRUE) +
        annotate("text", x = dt[1, x], y = Inf,
                 label = dt[.N, date], hjust = 0, vjust = 1, color = "gray") + 
        annotate("text", x = dt[.N, x], y = Inf,
                 label = subtitle_str, hjust = 1, vjust = 1, color = "gray", parse = TRUE) +
        annotate("text", label="@getpedr", x=dt[.N %/% 2, x], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
        labs(x=NULL, y=NULL) + theme_bw()
    
    # set xaxis
    if (rm_xaxis) {
        p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else {
        p = ped1_set_xaxis(p, dt, rm_weekend = rm_weekend)
    }
    
    # add linear trend line
    if (!is.null(linear_trend)) do.call(ped1_add_linear_trend, args = list(p=p, dt=dt, rm_weekend=rm_weekend, linear_trend=linear_trend))
    
    return(p)
}

# step
ped1_step   = function(dt, color_up = "#F6736D", color_down = "#18C0C4", rm_weekend = FALSE, rm_xaxis = FALSE, y = "close|value", linear_trend = NULL, ...) {
    dt = copy(dt)
    if (!is.logical(rm_weekend)) rm_weekend = FALSE
    
    y = names(dt)[grepl(y, names(dt))][1]
    if (is.na(y)) stop("The specified y is not existed.")
    if (y != "close") dt[["close"]] = dt[[y]]
    
    # add prev close
    if (!("prev_close" %in% names(dt))) dt = dt[, prev_close := shift(close, 1, type = "lag")]
    # add change and change_pct column into data
    if (!("change"     %in% names(dt))) dt = dt[, change := close - prev_close]
    if (!("change_pct" %in% names(dt))) dt = dt[, change_pct := change/close*100]
    if (all(c("change", "change_pct") %in% names(dt))) chg_str = sprintf("'%s(%s%%)'", dt[.N,round(change,2)], dt[.N,round(change_pct,2)])
    
    # subtitle string
    if (all(c("open","high","low","close") %in% names(dt))) {
        subtitle_str = paste(
            paste0("bold('", c("O","H","L","C", "Chg"), "')"), 
            c(dt[.N, sapply(.SD, round, 2), .SDcols = c("open","high","low","close")], chg_str), 
            sep = "~")
    } else {
        subtitle_str = paste(
            paste0("bold('", c(y, "Chg"), "')"), 
            c(dt[.N, sapply(.SD, round, 2), .SDcols = c(y)], chg_str), 
            sep = "~")
    }
    subtitle_str = paste0("atop(", paste(subtitle_str, collapse = "~"),")")
    
    # add up down columns
    dt[, `:=`(
        cl_clprev = ifelse(close > prev_close, "up", "down"), 
        rowid = .I, 
        x = date
    )]
    if (rm_weekend) dt[, x := rowid]
    dt[, prev_x := shift(x, 1, type="lag")
     ][cl_clprev=="down", `:=`(x1 = prev_x, y1 = prev_close, x2 = x, y2 = close)]
    
    # last row
    dt_N = dt[.N]
    
    # add new row
    dt = rbindlist(list(dt, data.table(date = Sys.Date(), dt[.N, .(close, rowid = rowid+1)])[, x := ifelse(rm_weekend, rowid, date)]), fill = TRUE)
    setkeyv(dt, "date")
    
    # plot
    p = ggplot(dt) + 
        geom_step(aes(x = x, y = close), color = color_up) +
        geom_segment(aes(x = x1, y = y1, xend = x2, yend = y1), color = color_down, na.rm = TRUE) +
        geom_segment(aes(x = x2, y = y1, xend = x2, yend = y2), color = color_down, na.rm = TRUE) +
        annotate("text", x = dt[1, x], y = Inf,
                 label = dt_N[, date], hjust = 0, vjust = 1, color = "gray") + 
        annotate("text", x = dt[.N, x], y = Inf,
                 label = subtitle_str, hjust = 1, vjust = 1, color = "gray", parse = TRUE) +
        annotate("text", label="@getpedr", x=dt[.N %/% 2, x], y=-Inf, vjust = -1, color = "gray", alpha = 0.3) + 
        labs(x=NULL, y=NULL) + theme_bw()
    
    # set xaxis
    if (rm_xaxis) {
        p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    } else {
        p = ped1_set_xaxis(p, dt, rm_weekend = rm_weekend)
    }
    
    # add linear trend line
    if (!is.null(linear_trend)) do.call(ped1_add_linear_trend, args = list(p=p, dt=dt, rm_weekend=rm_weekend, linear_trend=linear_trend))
    
    return(p)
}



# set xaxis 
ped1_set_xaxis = function(p, dt, rm_weekend = TRUE, ...) {
    xfreqs = c("y", "q", "m", "w")
    
    xticks = dt[, `:=`(
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
# add linear trend line
ped1_add_linear_trend = function(p, dt, rm_weekend = TRUE, linear_trend = c(0, 1, -1), ...) {
    add1_linear_trend = function(p, dt, num_sd, rm_weekend) {
        # add x column
        if (!("x" %in% names(dt))) {
            dt[, x := date]
            if (rm_weekend) dt[, x := .I]
        }
        
        # line color
        line_color = "blue"
        if (num_sd == 0) line_color = "red"
        
        # plot
        p = p + 
            geom_smooth(data = dt, aes(x = x, y = close), 
                        method = lm, formula = y+num_sd*sd(y)~x, 
                        na.rm = TRUE, se = FALSE, color = line_color, size = 0.2)
        
        return(p)
    }
    
    for (i in linear_trend) p = add1_linear_trend(p, dt, i, rm_weekend)
    
    return(p)
}
# # add technical indicators
# ped1_add_ti = function() {
#     
# }

# set yaxis
ped1_set_yaxis = function(p, dt, yaxis_log = FALSE) {
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
# set title
ped1_set_title = function(p, dt, title = NULL) {
    # plot title 
    if ("symbol" %in% names(dt)) title = paste(dt[1, symbol], title)
    # if ("name" %in% names(dt)) title = paste(dt[1, name], title)
    
    # title string
    title_str = sprintf("%s [%s/%s]", title, dt[1,date], dt[.N,date])
    
    # adding title and annotation
    p = p + 
        ggtitle(label = title_str) + 
        theme(plot.title = element_text(margin = margin(b=0))) 
    
    return(p)
}



# library(getpedr)
# library(data.table)
# library(ggplot2)
# 
# dt = getmd(c("^000001", "^399001"), date_range = "max", source = "163")
# p = ped1_candle(dt, rm_weekend = T)
# p = ped1_bar(dt, rm_weekend = T)
# p = ped1_line(dt, rm_weekend = T)
# p = ped1_step(dt, rm_weekend = F)

# p2 = ped1_set_xaxis(p, dt, rm_weekend = FALSE)
# print(ped1_set_yaxis(p))
# p2 = ped1_add_linear_trend(p, dt, rm_weekend = F)
# ped1_set_yaxis(p2, yaxis_log = F)

# @importFrom scales trans_breaks trans_format math_format
ped1_plot = function(dt, chart_type = "line", date_range="max", from = NULL, to = Sys.Date(), rm_weekend = NULL, title = NULL, y = "close|value", yaxis_log = FALSE, color_up = "#F6736D", color_down = "#18C0C4", linear_trend = c(0, 1, -1), ...) {
    # set dt as datatable
    setDT(dt, key="date")
    
    # set chart type
    chart_type = check_arg(chart_type, c("candle", "bar", "line", "step"), default = "line")
    if (!all(c("open", "high", "low", "close") %in% names(dt))) {
        if (chart_type %in% c("candle", "bar")) {
            chart_type = "line"
            warning("The chart_type is set to 'line'.")
        }
    }
    
    # from to 
    if (is.null(to)) to = dt[, max(date)]
    to = check_fromto(to, type = tolower(dt[, class(date)]), shift=1) 
    from = get_from_daterange(date_range, to, min_date=dt[,min(date)])
    # set range for data
    dat = dt[date>=from & date<=to]
    
    # remove 0 rows in variable
    y = names(dt)[grepl(y, names(dt))][1]
    dt = dt[eval(parse(text=y)) != 0]
    
    # plot
    p = do.call(paste0("ped1_",chart_type), args = list(dt=dt, chart_type=chart_type, color_up=color_up, color_down=color_down, rm_weekend=rm_weekend, y=y, linear_trend=linear_trend))
    # set yaxis and title
    p = ped1_set_yaxis(p, dt, yaxis_log)
    p = ped1_set_title(p, dt, title)

    return(p)
}

pedn_plot = function(dt, y="close|value", date_range="max", from=NULL, to=Sys.Date(), title=NULL, chart_type="line",  yaxis_log=FALSE) {
    . = symbol = .x = NULL
    
    # bind list of dataframe
    if (is.list(dt) & !is.data.frame(dt)) {
        dat = rbindlist(dt, fill = TRUE, idcol = "id")
    }
    setkeyv(dat, c("id","date"))
    
    # y
    y = names(dat)[grepl(y, names(dat))][1]
    
    # from to 
    if (is.null(to)) to = dt[, max(date)]
    to = check_fromto(to, type = tolower(dt[, class(date)]), shift=1) 
    from = get_from_daterange(date_range, to, min_date=dat[,min(date)])
    # set range for data
    dat = dat[date>=from & date<=to]
    
    # plot title 
    if (is.null(title)) title = ""
    title_str = sprintf("[%s/%s] %s", dat[,date[1]], dat[,date[.N]], title )
    
    # final x y vlaues
    x_final = dat[, .(date = date[.N]), by=symbol]
    y_final = dat[x_final, on=c("symbol", "date")]

    p = ggplot(data = dat) + 
        stat_identity(aes(x = date, y = eval(parse(text=y)), color=symbol), geom = chart_type) + 
        geom_hline(yintercept=y_final[[y]], color="gray", size=0.1) +
        annotate("text", x=x_final[["date"]], y=y_final[[y]], label=y_final[[y]], color="gray", hjust=0, size=3) +
        labs(title=title_str, x=NULL, y=NULL, color=NULL) + 
        theme_bw() +
        theme(legend.position=c(0.1,0.9), legend.background=element_blank(), legend.key=element_blank()) 
    
    
    if (yaxis_log) {
        p = p + 
            scale_y_continuous(trans='log10', position = "right") +
            # scale_y_log10(
            #     breaks = trans_breaks("log10", function(x) 10^x),
            #     labels = trans_format("log10", math_format(10^.x)),
            #     position = "right"
            #     ) + 
            annotation_logticks(sides = "l") 
    } else {
        p = p + scale_y_continuous(position = "right")
    }
    
    return(p)
}


# ti_plot = function(dat, ti=list(sma=20, sma=50, mm=10), p) {
#     dat = ped_addti(dt = dat, ti = ti, print_step = 0L)
# 
#     names(ti)
#     # trend_ti = c("mm", "sma", "ema", "smma", "bb", "sar")
#     # momentum_ti = c("macd", "ppo", "roc", "rsi", "cci")
# 
# 
# 
# 
# }

#' create a chart for timeseries data
#' 
#' `ped_plot` creates a charts for a time series dataset. 
#' 
#' @param dt a time series dataset
#' @param y the variable display on chart
#' @param date_range date range. Available value including '1m'-'11m', 'ytd', 'max' and '1y'-. Default is max.
#' @param from the start date. Default is max date in input data.
#' @param to the end date. Default is min date in data.
#' @param title chart title. If it is not specified, the symbol of dataset will be used as chart title.
#' @param type chart type, including line, step, candle.
#' @param yaxis_log logical, default is FALSE.
#' @param perf logical, whether to show the performance of input data. Default is FALSE. 
#' @param multi_series logical, whether to show multiple series. Default is FALSE. 
#' @param linear_trend non-negative numeric. Default is 0, which donot show linear trend line. If it larger than 0, show linear_trend*sd from linear trend line. 
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
    date_range="max", from = NULL, to = Sys.Date(), rm_weekend = NULL, 
    title = NULL, 
    y = "close|value", yaxis_log = FALSE, 
    color_up = "#F6736D", color_down = "#18C0C4", 
    linear_trend = c(0, 1, -1), 
    perf = FALSE, multi_series = FALSE) {
    
    # check arguments
    ## date range
    if (!grepl("ytd|[1-9,10,11]m|[1-9][0-9]*y", tolower(date_range))) date_range = "max"
    ## chart type
    chart_type = check_arg(chart_type, c("line", "step")) # candle bar
    
    # change data to performance
    if (perf) {
        dt = ped_perf(dt, y=y, date_range=date_range, from=from, to=to)
        title = paste(title,"perf")
    }
    
    
    # plot graphic
    ## multiple series
    if (multi_series) return(do.call(pedn_plot, args = list(dt=dt, y=y, date_range=date_range, from=from, to=to, title=title, chart_type=chart_type, yaxis_log=yaxis_log)))
    
    ## single series
    plist = list()
    dt_names = names(dt)
    if (is.list(dt) & !is.data.frame(dt)) {
        dt = lapply(dt, setDT)
        for (i in dt_names) {
            plist[[i]] = do.call(ped1_plot, args = list(dt=dt[[i]], chart_type = chart_type, date_range = date_range, from = from, to = to, rm_weekend = rm_weekend, title = title, y = y, yaxis_log = yaxis_log, color_up = color_up, color_down = color_down, linear_trend = linear_trend))
        }
        
    } else if (is.data.frame(dt)) {
        setDT(dt)
        i = 1
        if ("symbol" %in% dt_names) i = dt[1, symbol]
        
        plist[[i]] = do.call(ped1_plot, args = list(dt=dt, chart_type = chart_type, date_range = date_range, from = from, to = to, rm_weekend = rm_weekend, title = title, y = y, yaxis_log = yaxis_log, color_up = color_up, color_down = color_down, linear_trend = linear_trend))
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

